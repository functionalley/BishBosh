{-# LANGUAGE ScopedTypeVariables #-}
{-
	Copyright (C) 2018 Dr. Alistair Ward

	This file is part of BishBosh.

	BishBosh is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	BishBosh is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with BishBosh.  If not, see <http://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]	Interacts with the user by receiving commands & displaying the game on a raw text-based interface.
-}

module BishBosh.UI.Raw(
-- * Functions
--	readMove,
	takeTurns
 ) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.WeightedMeanAndCriterionValues		as Attribute.WeightedMeanAndCriterionValues
import qualified	BishBosh.Component.Move						as Component.Move
import qualified	BishBosh.Component.QualifiedMove				as Component.QualifiedMove
import qualified	BishBosh.Component.Turn						as Component.Turn
import qualified	BishBosh.Concurrent.Pondering					as Concurrent.Pondering
import qualified	BishBosh.ContextualNotation.PGN					as ContextualNotation.PGN
import qualified	BishBosh.ContextualNotation.PositionHashQualifiedMoveTree	as ContextualNotation.PositionHashQualifiedMoveTree
import qualified	BishBosh.ContextualNotation.QualifiedMoveForest			as ContextualNotation.QualifiedMoveForest
import qualified	BishBosh.Data.Exception						as Data.Exception
import qualified	BishBosh.Data.Time						as Data.Time
import qualified	BishBosh.Evaluation.Fitness					as Evaluation.Fitness
import qualified	BishBosh.Evaluation.PositionHashQuantifiedGameTree		as Evaluation.PositionHashQuantifiedGameTree
import qualified	BishBosh.Evaluation.QuantifiedGame				as Evaluation.QuantifiedGame
import qualified	BishBosh.Input.IOOptions					as Input.IOOptions
import qualified	BishBosh.Input.NativeUIOptions					as Input.NativeUIOptions
import qualified	BishBosh.Input.Options						as Input.Options
import qualified	BishBosh.Input.SearchOptions					as Input.SearchOptions
import qualified	BishBosh.Input.StandardOpeningOptions				as Input.StandardOpeningOptions
import qualified	BishBosh.Input.UIOptions					as Input.UIOptions
import qualified	BishBosh.Model.Game						as Model.Game
import qualified	BishBosh.Notation.MoveNotation					as Notation.MoveNotation
import qualified	BishBosh.Property.ForsythEdwards				as Property.ForsythEdwards
import qualified	BishBosh.Property.Null						as Property.Null
import qualified	BishBosh.Property.ShowFloat					as Property.ShowFloat
import qualified	BishBosh.Search.Search						as Search.Search
import qualified	BishBosh.Search.SearchState					as Search.SearchState
import qualified	BishBosh.State.ApplicationTerminationReason			as State.ApplicationTerminationReason
import qualified	BishBosh.State.Board						as State.Board
import qualified	BishBosh.State.MaybePieceByCoordinates				as State.MaybePieceByCoordinates
import qualified	BishBosh.State.PlayState					as State.PlayState
import qualified	BishBosh.Text.Show						as Text.Show
import qualified	BishBosh.Text.ShowList						as Text.ShowList
import qualified	BishBosh.Types							as T
import qualified	BishBosh.UI.Command						as UI.Command
import qualified	BishBosh.UI.PrintObject						as UI.PrintObject
import qualified	BishBosh.UI.SetObject						as UI.SetObject
import qualified	Control.Concurrent
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Control.Monad
import qualified	Control.Monad.Reader
import qualified	Data.Array.IArray
import qualified	Data.Bits
import qualified	Data.Default
import qualified	Data.List.Extra
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Data.Time.Clock
import qualified	Numeric
import qualified	System.IO
import qualified	System.Random
import qualified	ToolShed.System.Random

{- |
	* Reads a command-sequence from the user, terminating in either a request to move or to exit the game.

	* Since the user can also request roll-back to an earlier game before then requesting a new move, a new game is returned rather than just the requested move.
-}
readMove :: forall column criterionValue criterionWeight pieceSquareValue positionHash randomGen rankValue row weightedMean x y. (
	Control.DeepSeq.NFData	column,
	Control.DeepSeq.NFData	criterionWeight,
	Control.DeepSeq.NFData	pieceSquareValue,
	Control.DeepSeq.NFData	rankValue,
	Control.DeepSeq.NFData	row,
	Control.DeepSeq.NFData	x,
	Control.DeepSeq.NFData	y,
	Data.Array.IArray.Ix	x,
	Data.Bits.Bits		positionHash,
	Fractional		criterionValue,
	Fractional		pieceSquareValue,
	Fractional		rankValue,
	Fractional		weightedMean,
	Integral		column,
	Integral		x,
	Integral		y,
	Ord			positionHash,
	Read			x,
	Read			y,
	Real			criterionValue,
	Real			criterionWeight,
	Real			pieceSquareValue,
	Real			rankValue,
	Real			weightedMean,
	Show			column,
	Show			pieceSquareValue,
	Show			row,
	Show			x,
	Show			y,
	System.Random.RandomGen	randomGen
 )
	=> ContextualNotation.PositionHashQualifiedMoveTree.PositionHashQualifiedMoveTree x y positionHash
	-> randomGen
	-> Data.Time.Clock.UTCTime
	-> State.PlayState.PlayState column criterionValue criterionWeight pieceSquareValue positionHash rankValue row weightedMean x y
	-> IO (State.PlayState.PlayState column criterionValue criterionWeight pieceSquareValue positionHash rankValue row weightedMean x y)
{-# SPECIALISE readMove :: (
	Control.DeepSeq.NFData	column,
	Control.DeepSeq.NFData	rankValue,
	Control.DeepSeq.NFData	row,
	Fractional		rankValue,
	Integral		column,
	Real			rankValue,
	Show			column,
	Show			row,
	System.Random.RandomGen	randomGen
 )
	=> ContextualNotation.PositionHashQualifiedMoveTree.PositionHashQualifiedMoveTree T.X T.Y T.PositionHash
	-> randomGen
	-> Data.Time.Clock.UTCTime
	-> State.PlayState.PlayState column T.CriterionValue T.CriterionWeight T.PieceSquareValue T.PositionHash rankValue row T.WeightedMean T.X T.Y
	-> IO (State.PlayState.PlayState column T.CriterionValue T.CriterionWeight T.PieceSquareValue T.PositionHash rankValue row T.WeightedMean T.X T.Y)
 #-}
readMove positionHashQualifiedMoveTree randomGen startUTCTime playState	= let
	(game, options)			= State.PlayState.getGame &&& State.PlayState.getOptions $ playState
	(searchOptions, ioOptions)	= Input.Options.getSearchOptions &&& Input.Options.getIOOptions $ options

	searchDepthByLogicalColour	= Input.SearchOptions.getSearchDepthByLogicalColour searchOptions
	fullyManual			= Data.Map.null searchDepthByLogicalColour

	(uiOptions, maybeMaximumPGNNames)	= Input.IOOptions.getUIOptions &&& Input.IOOptions.getMaybeMaximumPGNNames $ ioOptions

	moveNotation	= Input.UIOptions.getMoveNotation uiOptions
	nDecimalDigits	= Input.UIOptions.getNDecimalDigits uiOptions
	verbosity	= Input.UIOptions.getVerbosity uiOptions
 in (
	\nativeUIOptions -> let
		show2D :: Model.Game.Game x y -> String
		show2D	= uncurry State.MaybePieceByCoordinates.show2D (
			snd {-columns-} . Input.NativeUIOptions.getBoardMagnification &&& Input.NativeUIOptions.getColourScheme $ nativeUIOptions
		 ) (
			Notation.MoveNotation.getOrigin moveNotation
		 ) . State.Board.getMaybePieceByCoordinates . Model.Game.getBoard

		onCommand :: UI.Command.Command x y -> IO (State.PlayState.PlayState column criterionValue criterionWeight pieceSquareValue positionHash rankValue row weightedMean x y)
		onCommand UI.Command.Hint	= do
			Control.Monad.unless (Model.Game.isTerminated game) . Data.Maybe.maybe (
				do
					Control.Monad.when (verbosity > Data.Default.def && not (ContextualNotation.PositionHashQualifiedMoveTree.isTerminal positionHashQualifiedMoveTree)) . System.IO.hPutStrLn System.IO.stderr $ Text.Show.showsInfoPrefix "failed to find any suitable archived move."

					let
						searchResult	= Control.Monad.Reader.runReader (
							Search.Search.search (Input.SearchOptions.getSearchDepth searchOptions) $ State.PlayState.getSearchState playState
						 ) searchOptions

					case Search.Search.getQuantifiedGames searchResult of
						quantifiedGame : _	-> putStrLn . Text.Show.showsInfoPrefix . showString "Try " $ if verbosity == maxBound
							then Notation.MoveNotation.showsNotationFloatToNDecimals moveNotation nDecimalDigits searchResult ""
							else Data.Maybe.maybe (
								Control.Exception.throw $ Data.Exception.mkNullDatum "BishBosh.UI.Raw.readMove.onCommand:\tzero turns have been made."
							) (
								showChar '"' . flip (Notation.MoveNotation.showsNotation moveNotation) "\"."
							) . Model.Game.maybeLastTurn $ Evaluation.QuantifiedGame.getGame quantifiedGame
						_		-> Control.Exception.throwIO . Data.Exception.mkRequestFailure . showString "BishBosh.UI.Raw.readMove.onCommand:\tunexpectedly failed to find any moves; " $ shows game "."	-- CAVEAT: the game should have terminated.
			 ) (
				\(qualifiedMove, names) -> putStrLn . Text.Show.showsInfoPrefix . showString "Try \"" . Notation.MoveNotation.showsNotation moveNotation qualifiedMove . showString "\" from:" $ ContextualNotation.QualifiedMoveForest.showsNames maybeMaximumPGNNames names ""
			 ) $ ContextualNotation.PositionHashQualifiedMoveTree.maybeRandomlySelectOnymousQualifiedMove randomGen (
				Input.StandardOpeningOptions.getMatchSwitches $ Input.SearchOptions.getStandardOpeningOptions searchOptions
			 ) game positionHashQualifiedMoveTree

			return {-to IO-monad-} playState	-- N.B.: though one could merely call "eventLoop", a new random-generator is desirable in case an alternative hint is requested.
		onCommand (UI.Command.Print printObject)	= do
			case printObject of
				UI.PrintObject.Board		-> putStrLn $ show2D game
				UI.PrintObject.Configuration	-> putStrLn $ Property.ShowFloat.showsFloatToN nDecimalDigits options "."
				UI.PrintObject.FEN		-> putStrLn $ Property.ForsythEdwards.showFEN game
				UI.PrintObject.Game		-> print game
				UI.PrintObject.Help		-> putStrLn . showString "Enter either a move in " . shows moveNotation $ showString "-notation, or:\n" UI.Command.usageMessage
				UI.PrintObject.Moves		-> putStrLn . showString (
					showString Component.Move.tag "s"
				 ) . Text.ShowList.showsAssociation $ Text.ShowList.showsFormattedList' (
					Notation.MoveNotation.showsNotation moveNotation
				 ) (
					Model.Game.listTurnsChronologically game
				 ) "."
				UI.PrintObject.PGN		-> ContextualNotation.PGN.showsGame game >>= putStrLn . ($ "")

			eventLoop
		onCommand UI.Command.Quit		= do
			Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.Show.showsInfoPrefix "quitting on request."

			return {-to IO-monad-} playState { State.PlayState.getMaybeApplicationTerminationReason = Just State.ApplicationTerminationReason.byRequest }
		onCommand UI.Command.Resign		= do
			Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.Show.showsInfoPrefix "resigning."

			return {-to IO-monad-} $ State.PlayState.resign playState
		onCommand UI.Command.Restart
			| Property.Null.isNull game	= do
				Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr $ Text.Show.showsWarningPrefix "the game is unstarted."

				return {-to IO-monad-} playState
			| otherwise			= do
				Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.Show.showsInfoPrefix "restarting game."

				return {-to IO-monad-} $ State.PlayState.reconstructPositionHashQuantifiedGameTree Data.Default.def {-game-} playState	-- By exiting the event-loop, the new game will be persisted where appropriate.
		onCommand (UI.Command.RollBack maybeNPlies)	= let
			rollBack :: Component.Move.NMoves -> IO (State.PlayState.PlayState column criterionValue criterionWeight pieceSquareValue positionHash rankValue row weightedMean x y)
			rollBack nPlies
				| (game', _) : _ <- drop (pred nPlies) $ Model.Game.rollBack game	= do
					Control.Monad.when (verbosity == maxBound) . putStrLn $ show2D game'

					return {-to IO-monad-} $ State.PlayState.reconstructPositionHashQuantifiedGameTree game' playState
				| otherwise	= onCommand UI.Command.Restart
		 in Data.Maybe.maybe (
			let
				nPlies	= succ $ Data.Map.size searchDepthByLogicalColour	-- In fully manual play, rollback one ply, in semi-manual play rollback two plies.
			in do
				Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.Show.showsInfoPrefix . showString "rolling-back " $ shows nPlies " plies."

				rollBack nPlies
		 ) (
			\nPlies -> if nPlies <= 0
				then do
					System.IO.hPutStrLn System.IO.stderr $ Text.Show.showsErrorPrefix "the number of plies to rollback, must exceed zero."

					eventLoop
				else rollBack nPlies
		 ) maybeNPlies
		onCommand UI.Command.Save	= do
			Data.Maybe.maybe (
				return {-to IO-monad-} $ Text.Show.showsErrorPrefix "the file-path at which to save the game, hasn't been defined."
			 ) (
				\(filePath, automatic) -> if automatic
					then return {-to IO-monad-} $ Text.Show.showsWarningPrefix "the state of the game is, in accordance with configuration, saved automatically."
					else Control.Exception.catch (
						do
							System.IO.withFile filePath System.IO.WriteMode (`System.IO.hPutStrLn` show game)

							return {-to IO-monad-} . Text.Show.showsInfoPrefix . showString "the game-state has been saved in " $ shows filePath "."
					 ) $ \e -> return {-to IO-monad-} . Text.Show.showsErrorPrefix $ show (e :: Control.Exception.SomeException)
			 ) (
				Input.IOOptions.getMaybePersistence ioOptions
			 ) >>= System.IO.hPutStrLn System.IO.stderr

			eventLoop
		onCommand (UI.Command.Set setObject)
			| fullyManual	= do
				System.IO.hPutStrLn System.IO.stderr . Text.Show.showsWarningPrefix $ shows UI.Command.setTag " requires an automated opponent."

				return {-to IO-monad-} playState
			| otherwise					= Control.Exception.catchJust (
				\e -> if Data.Exception.isBadData e
					then Just $ show e
					else Nothing
			) (
				do
					Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.Show.showsInfoPrefix . showString "setting " $ shows setObject "."

					readMove positionHashQualifiedMoveTree randomGen startUTCTime playState {
						State.PlayState.getOptions	= Control.DeepSeq.force $ case setObject of
							UI.SetObject.SearchDepth searchDepth	-> options {
								Input.Options.getSearchOptions	= Input.SearchOptions.setSearchDepth searchDepth $ Input.Options.getSearchOptions options
							}
					}
			) (
				\s -> do
					Control.Monad.unless (verbosity == minBound) . System.IO.hPutStrLn System.IO.stderr $ Text.Show.showsErrorPrefix s

					eventLoop
			)
		onCommand UI.Command.Swap
			| fullyManual	= do
				Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.Show.showsWarningPrefix . showString " there aren't any " $ shows Input.SearchOptions.searchDepthTag " to swap."

				eventLoop
			| otherwise					= do
				Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.Show.showsInfoPrefix . showString "swapping " $ shows Input.SearchOptions.searchDepthTag "."

				return {-to IO-monad-} playState { State.PlayState.getOptions = Input.Options.swapSearchDepth options }

		eventLoop :: IO (State.PlayState.PlayState column criterionValue criterionWeight pieceSquareValue positionHash rankValue row weightedMean x y)
		eventLoop	= getLine >>= \line -> case Data.List.Extra.trim line of
			"?"	-> onCommand $ UI.Command.Print UI.PrintObject.Help
			':' : s	-> either (
				\errorMessage -> do
					System.IO.hPutStrLn System.IO.stderr . Text.Show.showsErrorPrefix $ showString errorMessage "."

					eventLoop
			 ) (
				\(command, remainder) -> if null $ Data.List.Extra.trimStart remainder
					then onCommand command
					else do
						System.IO.hPutStrLn System.IO.stderr . showString "unexpected trailing text " $ shows remainder "."

						eventLoop
			 ) . UI.Command.readsCommand $ UI.Command.autoComplete s
			s	-> let
				corrections	= State.PlayState.suggestCorrections s playState
			 in case Notation.MoveNotation.readsQualifiedMove moveNotation s of
				[(eitherQualifiedMove, "")]
					| Just errorMessage <- Model.Game.validateEitherQualifiedMove eitherQualifiedMove game	-> do
						System.IO.hPutStrLn System.IO.stderr . Text.Show.showsErrorPrefix . shows s . showString " is illegal; " $ shows errorMessage "."

						Control.Monad.unless (null corrections) . System.IO.hPutStrLn System.IO.stderr . Text.Show.showsInfoPrefix . showString "did you mean " $ shows corrections " ?"

						eventLoop	-- Recurse.
					| otherwise	-> do
						elapsedTime	<- Data.Time.measureElapsedTime startUTCTime

						let
							game'		= Model.Game.applyEitherQualifiedMove eitherQualifiedMove game
							playState'	= State.PlayState.updateWithManualMove game' playState

						Control.Monad.when (verbosity == maxBound) $ do
							System.IO.hPutStrLn System.IO.stderr . Text.Show.showsInfoPrefix . showString Component.Move.tag . Text.ShowList.showsAssociation . shows (
								Notation.MoveNotation.showNotation moveNotation . Data.Maybe.fromMaybe (
									Control.Exception.throw $ Data.Exception.mkResultUndefined "BishBosh.UI.Raw.readMove.eventLoop:\tModel.Game.maybeLastTurn failed."
								) $ Model.Game.maybeLastTurn game'
							 ) . showString " was requested after " $ Data.Time.showsTimeAsSeconds nDecimalDigits elapsedTime "."

							case ContextualNotation.PositionHashQualifiedMoveTree.findNextOnymousQualifiedMovesForPosition game' positionHashQualifiedMoveTree of
								[]			-> return {-to IO-monad-} ()
								onymousQualifiedMoves	-> System.IO.hPutStrLn System.IO.stderr . Text.Show.showsInfoPrefix . showString "matches archived game(s):" $ ContextualNotation.QualifiedMoveForest.showsNames maybeMaximumPGNNames (
									concatMap (
										map fst {-Name-} . snd {-[OnymousResult]-}
									) onymousQualifiedMoves
								 ) ""

						return {-to IO-monad-} playState'	-- It's now the other player's move.
				[(_, remainder)]		-> do
					System.IO.hPutStrLn System.IO.stderr . Text.Show.showsErrorPrefix . showString "the specified " . showString Component.Move.tag . showString " was correctly formatted, but was followed by unexpected text" . Text.ShowList.showsAssociation $ shows remainder "."

					eventLoop	-- Recurse.
				_ {-no parse-}			-> do
					Control.Monad.unless (null s) $ do
						System.IO.hPutStrLn System.IO.stderr . Text.Show.showsErrorPrefix . shows s . showString " /~ " $ Notation.MoveNotation.showsMoveSyntax moveNotation "."	-- CAVEAT: this error also results from source == destination.

						Control.Monad.unless (null corrections) . System.IO.hPutStrLn System.IO.stderr . Text.Show.showsInfoPrefix . showString "did you mean " $ shows corrections " ?"

					eventLoop	-- Recurse.
	 in do
		putStrLn . showString "Enter either a move in " . shows moveNotation . showString "-notation, or '?' for " $ showString UI.PrintObject.helpTag ":"

		eventLoop
 ) `either` (
	const . Control.Exception.throwIO $ Data.Exception.mkInvalidDatum "BishBosh.UI.Raw.readMove:\tunexpected CECP-options."
 ) $ Input.UIOptions.getEitherNativeUIOrCECPOptions uiOptions

-- | Plays the game.
takeTurns :: forall column criterionValue criterionWeight pieceSquareValue positionHash randomGen rankValue row weightedMean x y. (
	Control.DeepSeq.NFData	column,
	Control.DeepSeq.NFData	criterionWeight,
	Control.DeepSeq.NFData	pieceSquareValue,
	Control.DeepSeq.NFData	rankValue,
	Control.DeepSeq.NFData	row,
	Control.DeepSeq.NFData	weightedMean,
	Control.DeepSeq.NFData	x,
	Control.DeepSeq.NFData	y,
	Data.Array.IArray.Ix	x,
	Data.Bits.Bits		positionHash,
	Fractional		criterionValue,
	Fractional		pieceSquareValue,
	Fractional		rankValue,
	Fractional		weightedMean,
	Integral		column,
	Integral		x,
	Integral		y,
	Ord			positionHash,
	Read			x,
	Read			y,
	Real			criterionValue,
	Real			criterionWeight,
	Real			pieceSquareValue,
	Real			rankValue,
	Real			weightedMean,
	Show			column,
	Show			pieceSquareValue,
	Show			row,
	Show			x,
	Show			y,
	System.Random.RandomGen	randomGen
 )
	=> ContextualNotation.PositionHashQualifiedMoveTree.PositionHashQualifiedMoveTree x y positionHash
	-> randomGen
	-> State.PlayState.PlayState column criterionValue criterionWeight pieceSquareValue positionHash rankValue row weightedMean x y
	-> IO (State.PlayState.PlayState column criterionValue criterionWeight pieceSquareValue positionHash rankValue row weightedMean x y)
{-# SPECIALISE takeTurns :: (
	Control.DeepSeq.NFData	column,
	Control.DeepSeq.NFData	row,
	Integral		column,
	Show			column,
	Show			row,
	System.Random.RandomGen	randomGen
 )
	=> ContextualNotation.PositionHashQualifiedMoveTree.PositionHashQualifiedMoveTree T.X T.Y T.PositionHash
	-> randomGen
	-> State.PlayState.PlayState column T.CriterionValue T.CriterionWeight T.PieceSquareValue T.PositionHash T.RankValue row T.WeightedMean T.X T.Y
	-> IO (State.PlayState.PlayState column T.CriterionValue T.CriterionWeight T.PieceSquareValue T.PositionHash T.RankValue row T.WeightedMean T.X T.Y)
 #-}
takeTurns positionHashQualifiedMoveTree randomGen playState	= let
	options	= State.PlayState.getOptions playState

	(ioOptions, searchOptions)	= Input.Options.getIOOptions &&& Input.Options.getSearchOptions $ options

	uiOptions	= Input.IOOptions.getUIOptions ioOptions

	moveNotation	= Input.UIOptions.getMoveNotation uiOptions
	nDecimalDigits	= Input.UIOptions.getNDecimalDigits uiOptions
	verbosity	= Input.UIOptions.getVerbosity uiOptions
 in (
	\nativeUIOptions -> do
		mVar	<- Control.Concurrent.newEmptyMVar

		let
			show2D :: Model.Game.Game x y -> String
			show2D	= uncurry State.MaybePieceByCoordinates.show2D (
				snd {-columns-} . Input.NativeUIOptions.getBoardMagnification &&& Input.NativeUIOptions.getColourScheme $ nativeUIOptions
			 ) (
				Notation.MoveNotation.getOrigin moveNotation
			 ) . State.Board.getMaybePieceByCoordinates . Model.Game.getBoard

			slave :: Maybe (Concurrent.Pondering.Pondering (Component.Move.Move x y)) -> Maybe Component.Move.NMoves -> [randomGen] -> State.PlayState.PlayState column criterionValue criterionWeight pieceSquareValue positionHash rankValue row weightedMean x y -> IO (State.PlayState.PlayState column criterionValue criterionWeight pieceSquareValue positionHash rankValue row weightedMean x y)
			slave maybePondering maybeMaximumPlies ~(randomGen' : randomGens) playState'	= let
				(game', searchOptions')		= State.PlayState.getGame &&& Input.Options.getSearchOptions . State.PlayState.getOptions $ playState'
			 in Data.Maybe.maybe (
				do
					startUTCTime	<- Data.Time.Clock.getCurrentTime

					Data.Maybe.maybe (
						do
							playState''	<- readMove positionHashQualifiedMoveTree randomGen' startUTCTime playState'

							(,) playState'' `fmap` if playState' `State.PlayState.hasMorePlies` playState''
								then {-rolled-back-} do
									Data.Maybe.maybe (
										return {-to IO-monad-} ()
									 ) (
										\Concurrent.Pondering.MkPondering { Concurrent.Pondering.getThreadId = threadId } -> Concurrent.Pondering.abort mVar threadId >>= Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "pondering invalidated by roll-back => "
									 ) maybePondering

									return {-to IO-monad-} Nothing
								else return {-to IO-monad-} maybePondering
					 ) (
						\searchDepth' -> Data.Maybe.maybe (
							do
								Control.Monad.when (
									verbosity > Data.Default.def && not (
										ContextualNotation.PositionHashQualifiedMoveTree.isTerminal positionHashQualifiedMoveTree
									)
								 ) . System.IO.hPutStrLn System.IO.stderr $ Text.Show.showsInfoPrefix "failed to find any suitable archived move."

								let
									search ss	= Control.Monad.Reader.runReader (Search.Search.search searchDepth' ss) searchOptions'
									searchResult	= search $ State.PlayState.getSearchState playState'

								Data.Maybe.maybe (
									return {-to IO-monad-} searchResult
								 ) (
									\Concurrent.Pondering.MkPondering {
										Concurrent.Pondering.getPremis		= movePremis,
										Concurrent.Pondering.getThreadId	= threadId
									} -> if Data.Maybe.maybe False ((== movePremis) . Component.QualifiedMove.getMove . Component.Turn.getQualifiedMove) $ Model.Game.maybeLastTurn game'
										then do
											Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "move-premis validated => waiting on " $ shows threadId "."

											Control.Concurrent.takeMVar mVar
										else do
											Concurrent.Pondering.abort mVar threadId >>= Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "pondering invalidated by incorrect move-premis => "

											return {-to IO-monad-} searchResult
								 ) maybePondering >>= (
									\searchResult' -> let
										searchState'	= Search.Search.getSearchState searchResult'
									in case Search.Search.getQuantifiedGames searchResult' of
										quantifiedGame : continuation	-> let
											bestTurn	= Evaluation.QuantifiedGame.getLastTurn quantifiedGame
										 in do
											elapsedTime	<- bestTurn `seq` Data.Time.measureElapsedTime startUTCTime

											System.IO.hPutStrLn System.IO.stderr . Text.Show.showsInfoPrefix . (
												if verbosity == maxBound
													then Property.ShowFloat.showsFloatToN nDecimalDigits (
														Control.Monad.Reader.runReader (
															Evaluation.Fitness.evaluateFitness Nothing game'
														) $ Input.Options.getEvaluationOptions options	:: Attribute.WeightedMeanAndCriterionValues.WeightedMeanAndCriterionValues weightedMean criterionValue
													) . Search.Search.showsSeparator	-- Prepend the fitness of the original game prior to the result.
													else id
											 ) $ (
												if verbosity > Data.Default.def
													then Notation.MoveNotation.showsNotationFloatToNDecimals moveNotation nDecimalDigits searchResult' . showString " in " . Data.Time.showsTimeAsSeconds nDecimalDigits elapsedTime
													else Notation.MoveNotation.showsNotation moveNotation bestTurn
											 ) "."

											let selectedGame	= Evaluation.QuantifiedGame.getGame quantifiedGame

											Control.Monad.unless (verbosity == minBound) . putStrLn $ show2D selectedGame

											(,) (
												State.PlayState.updateWithAutomaticMove (
													Attribute.WeightedMeanAndCriterionValues.getCriterionValues $ Evaluation.QuantifiedGame.getWeightedMeanAndCriterionValues quantifiedGame
												) searchState' playState'
											 ) `fmap` if Input.SearchOptions.getUsePondering searchOptions
												then case continuation of
													quantifiedGame' : _	-> let
														turnPremis	= Evaluation.QuantifiedGame.getLastTurn quantifiedGame'
													 in fmap Just . (
														\positionHashQuantifiedGameTree'' -> Concurrent.Pondering.ponder (
															Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix
														) (
															Component.QualifiedMove.getMove $ Component.Turn.getQualifiedMove turnPremis
														) (
															showString "move-premis" . Text.ShowList.showsAssociation $ Notation.MoveNotation.showsNotation moveNotation turnPremis "."
														) (
															search searchState' { Search.SearchState.getPositionHashQuantifiedGameTree = positionHashQuantifiedGameTree'' }
														) mVar
													 ) . Data.Maybe.fromMaybe (
														Control.Exception.throw $ Data.Exception.mkIncompatibleData "BishBosh.UI.Raw.takeTurns.slave:\tData.RoseTree.reduce failed."
													 ) . Evaluation.PositionHashQuantifiedGameTree.reduce (
														(== Evaluation.QuantifiedGame.getLastTurn quantifiedGame') . Evaluation.QuantifiedGame.getLastTurn . Evaluation.PositionHashQuantifiedGameTree.getQuantifiedGame
													 ) $ Search.SearchState.getPositionHashQuantifiedGameTree searchState'
													_		-> return {-to IO-monad-} Nothing
												else return {-to IO-monad-} Nothing
										_	-> Control.Exception.throwIO . Data.Exception.mkRequestFailure . showString "BishBosh.UI.Raw.takeTurns.slave:\tunexpectedly failed to find any moves; " $ shows game' "."	-- A gameTerminationReason should have been defined.
								 )
						 ) (
							\(qualifiedMove, names) -> do
								elapsedTime	<- Data.Time.measureElapsedTime startUTCTime

								Data.Maybe.maybe (
									return {-to IO-monad-} ()
								 ) (
									\Concurrent.Pondering.MkPondering { Concurrent.Pondering.getThreadId = threadId } -> Concurrent.Pondering.abort mVar threadId >>= Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "pondering pre-empted by standard-opening match => "
								 ) maybePondering

								let selectedGame	= Model.Game.applyQualifiedMove qualifiedMove game'

								if verbosity == minBound
									then System.IO.hPutStrLn System.IO.stderr . Text.Show.showsInfoPrefix $ Notation.MoveNotation.showsNotation moveNotation qualifiedMove "."
									else do
										Control.Monad.when (verbosity > Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.Show.showsInfoPrefix . showString "selected " . Notation.MoveNotation.showsNotation moveNotation qualifiedMove . showString " from:" . ContextualNotation.QualifiedMoveForest.showsNames (
											Input.IOOptions.getMaybeMaximumPGNNames ioOptions
										 ) names . showString "\n\tin " $ Data.Time.showsTimeAsSeconds nDecimalDigits elapsedTime "."

										putStrLn $ show2D selectedGame

								return {-to IO-monad-} (State.PlayState.updateWithManualMove selectedGame playState', Nothing)	-- N.B.: one could ponder, but would have to construct a game-tree, & the chance of a subsequent standard-opening move is high.
						 ) $ ContextualNotation.PositionHashQualifiedMoveTree.maybeRandomlySelectOnymousQualifiedMove randomGen' (
							Input.StandardOpeningOptions.getMatchSwitches $ Input.SearchOptions.getStandardOpeningOptions searchOptions
						 ) game' positionHashQualifiedMoveTree
					 ) (
						Model.Game.getNextLogicalColour game' `Data.Map.lookup` Input.SearchOptions.getSearchDepthByLogicalColour searchOptions'
					 ) >>= (
						\(playState'', maybePondering') -> do
							Data.Maybe.maybe (
								return {-to IO-monad-} ()
							 ) (
								\(filePath, automatic) -> let
									game''	= State.PlayState.getGame playState''
								in Control.Monad.when (automatic && show game'' /= show game') . Control.Exception.catch (
									System.IO.withFile filePath System.IO.WriteMode (`System.IO.hPrint` game'')
								) $ \e -> System.IO.hPutStrLn System.IO.stderr . Text.Show.showsErrorPrefix $ show (e :: Control.Exception.SomeException)
							 ) $ Input.IOOptions.getMaybePersistence ioOptions

							if State.PlayState.hasApplicationTerminationBeenRequested playState''
								then return {-to IO-monad-} playState''
								else if Data.Maybe.maybe False (<= 1) maybeMaximumPlies
									then return {-to IO-monad-} playState'' { State.PlayState.getMaybeApplicationTerminationReason = Just State.ApplicationTerminationReason.maximumPlies }
									else slave maybePondering' (fmap pred maybeMaximumPlies) randomGens playState''	-- Tail recurse.
					 )
			 ) (
				\gameTerminationReason -> if State.PlayState.hasApplicationTerminationBeenRequested playState'
					then return {-to IO-monad-} playState'
					else {-don't terminate the application-} do
						putStrLn . Text.Show.showsInfoPrefix $ shows gameTerminationReason "."

						let
							criterionValueStatistics	= State.PlayState.calculateCriterionValueStatistics playState'

							showsFloat :: Double -> ShowS
							showsFloat	= Numeric.showFFloat $ Just nDecimalDigits

						Control.Monad.when (
							verbosity == maxBound && not (null criterionValueStatistics)
						 ) . System.IO.hPutStrLn System.IO.stderr . Text.Show.showsInfoPrefix . showString "mean & standard-deviation of criterion-values" . Text.ShowList.showsAssociation $ Text.ShowList.showsFormattedList' (
							\(mean, standardDeviation) -> showChar '(' . showsFloat mean . Text.ShowList.showsSeparator . showsFloat standardDeviation . showChar ')'
						 ) criterionValueStatistics "."

						readMove positionHashQualifiedMoveTree randomGen' (
							Control.Exception.throw $ Data.Exception.mkInvalidDatum "BishBosh.UI.Raw.takeTurns.slave:\tundefined startUTCTime."
						 ) playState' {-there're zero valid moves, but the user can issue commands-} >>= slave Nothing maybeMaximumPlies randomGens	-- Tail recurse.

			 ) $ Model.Game.getMaybeTerminationReason game'

		Control.Monad.unless (verbosity == minBound) . putStrLn . show2D $ State.PlayState.getGame playState

		slave Nothing (Input.Options.getMaybeMaximumPlies options) (ToolShed.System.Random.randomGens randomGen) playState
 ) `either` (
	const . Control.Exception.throwIO $ Data.Exception.mkInvalidDatum "BishBosh.UI.Raw.takeTurns:\tunexpected CECP-options."
 ) $ Input.UIOptions.getEitherNativeUIOrCECPOptions uiOptions

