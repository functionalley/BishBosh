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

import			Control.Arrow((&&&), (|||))
import			Control.Monad((>=>), (<=<))
import qualified	BishBosh.Component.Move						as Component.Move
import qualified	BishBosh.Component.QualifiedMove				as Component.QualifiedMove
import qualified	BishBosh.Component.Turn						as Component.Turn
import qualified	BishBosh.Concurrent.Pondering					as Concurrent.Pondering
import qualified	BishBosh.ContextualNotation.PGN					as ContextualNotation.PGN
import qualified	BishBosh.ContextualNotation.PositionHashQualifiedMoveTree	as ContextualNotation.PositionHashQualifiedMoveTree
import qualified	BishBosh.ContextualNotation.QualifiedMoveForest			as ContextualNotation.QualifiedMoveForest
import qualified	BishBosh.Data.Exception						as Data.Exception
import qualified	BishBosh.Evaluation.Fitness					as Evaluation.Fitness
import qualified	BishBosh.Evaluation.PositionHashQuantifiedGameTree		as Evaluation.PositionHashQuantifiedGameTree
import qualified	BishBosh.Evaluation.QuantifiedGame				as Evaluation.QuantifiedGame
import qualified	BishBosh.Input.IOOptions					as Input.IOOptions
import qualified	BishBosh.Input.NativeUIOptions					as Input.NativeUIOptions
import qualified	BishBosh.Input.Options						as Input.Options
import qualified	BishBosh.Input.SearchOptions					as Input.SearchOptions
import qualified	BishBosh.Input.StandardOpeningOptions				as Input.StandardOpeningOptions
import qualified	BishBosh.Input.UIOptions					as Input.UIOptions
import qualified	BishBosh.Metric.WeightedMeanAndCriterionValues			as Metric.WeightedMeanAndCriterionValues
import qualified	BishBosh.Model.Game						as Model.Game
import qualified	BishBosh.Notation.MoveNotation					as Notation.MoveNotation
import qualified	BishBosh.Property.ExtendedPositionDescription			as Property.ExtendedPositionDescription
import qualified	BishBosh.Property.ForsythEdwards				as Property.ForsythEdwards
import qualified	BishBosh.Property.ShowFloat					as Property.ShowFloat
import qualified	BishBosh.Property.Switchable					as Property.Switchable
import qualified	BishBosh.Search.Search						as Search.Search
import qualified	BishBosh.Search.SearchState					as Search.SearchState
import qualified	BishBosh.State.ApplicationTerminationReason			as State.ApplicationTerminationReason
import qualified	BishBosh.State.Board						as State.Board
import qualified	BishBosh.State.InstancesByPosition				as State.InstancesByPosition
import qualified	BishBosh.State.MaybePieceByCoordinates				as State.MaybePieceByCoordinates
import qualified	BishBosh.State.PlayState					as State.PlayState
import qualified	BishBosh.Text.ShowColouredPrefix				as Text.ShowColouredPrefix
import qualified	BishBosh.Text.ShowList						as Text.ShowList
import qualified	BishBosh.Text.ShowPrefix					as Text.ShowPrefix
import qualified	BishBosh.Time.StopWatch						as Time.StopWatch
import qualified	BishBosh.Type.Count						as Type.Count
import qualified	BishBosh.Type.Crypto						as Type.Crypto
import qualified	BishBosh.UI.Command						as UI.Command
import qualified	BishBosh.UI.PrintObject						as UI.PrintObject
import qualified	BishBosh.UI.ReportObject					as UI.ReportObject
import qualified	BishBosh.UI.SetObject						as UI.SetObject
import qualified	Control.Concurrent
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Control.Monad
import qualified	Control.Monad.Reader
import qualified	Data.Bits
import qualified	Data.Default
import qualified	Data.Foldable
import qualified	Data.List.Extra
import qualified	Data.Map.Strict							as Map
import qualified	Data.Maybe
import qualified	System.IO
import qualified	System.Random
import qualified	ToolShed.System.Random

{- |
	* Reads a command-sequence from the user, terminating in either a request to move or to exit the game.

	* Since the user can also request roll-back to an earlier game before then requesting a new move, a new game is returned rather than just the requested move.
-}
readMove :: forall positionHash randomGen. (
	Data.Bits.Bits		positionHash,
	Ord			positionHash,
	System.Random.RandomGen	randomGen
 )
	=> ContextualNotation.PositionHashQualifiedMoveTree.PositionHashQualifiedMoveTree positionHash
	-> randomGen
	-> Time.StopWatch.StopWatch
	-> State.PlayState.PlayState positionHash
	-> IO (State.PlayState.PlayState positionHash)
{-# SPECIALISE readMove :: ContextualNotation.PositionHashQualifiedMoveTree.PositionHashQualifiedMoveTree Type.Crypto.PositionHash -> System.Random.StdGen -> Time.StopWatch.StopWatch -> State.PlayState.PlayState Type.Crypto.PositionHash -> IO (State.PlayState.PlayState Type.Crypto.PositionHash) #-}
readMove positionHashQualifiedMoveTree randomGen runningWatch playState	= let
	(game, options)			= State.PlayState.getGame &&& State.PlayState.getOptions $ playState
	(searchOptions, ioOptions)	= Input.Options.getSearchOptions &&& Input.Options.getIOOptions $ options

	searchDepthByLogicalColour	= Input.SearchOptions.getSearchDepthByLogicalColour searchOptions
	fullyManual			= Data.Foldable.null searchDepthByLogicalColour

	(uiOptions, maybeMaximumPGNNames)	= Input.IOOptions.getUIOptions &&& Input.IOOptions.getMaybeMaximumPGNNames $ ioOptions

	moveNotation	= Input.UIOptions.getMoveNotation uiOptions
	nDecimalDigits	= Input.UIOptions.getNDecimalDigits uiOptions
	verbosity	= Input.UIOptions.getVerbosity uiOptions
 in (
	\nativeUIOptions -> let
		show2D :: Model.Game.Game -> String
		show2D g	= State.MaybePieceByCoordinates.show2D (
			State.Board.getMaybePieceByCoordinates $ Model.Game.getBoard g
		 ) (
			snd {-columns-} $ Input.NativeUIOptions.getBoardMagnification nativeUIOptions
		 ) (
			Input.NativeUIOptions.getColourScheme nativeUIOptions
		 ) (
			Input.NativeUIOptions.getDepictFigurine nativeUIOptions
		 ) (
			Notation.MoveNotation.getOrigin moveNotation
		 )

		onCommand :: UI.Command.Command -> IO (State.PlayState.PlayState positionHash)
		onCommand UI.Command.Hint	= do
			Control.Monad.unless (Model.Game.isTerminated game) . Data.Maybe.maybe (
				do
					Control.Monad.when (verbosity > Data.Default.def && not (ContextualNotation.PositionHashQualifiedMoveTree.isTerminal positionHashQualifiedMoveTree)) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowColouredPrefix.showsPrefixInfo "failed to find any suitable archived move."

					let
						searchResult	= Control.Monad.Reader.runReader (
							Search.Search.search (Input.SearchOptions.getSearchDepth searchOptions) $ State.PlayState.getSearchState playState
						 ) searchOptions

					case Search.Search.getQuantifiedGames searchResult of
						quantifiedGame : _	-> putStrLn . Text.ShowColouredPrefix.showsPrefixInfo . showString "Try " $ if verbosity == maxBound
							then Notation.MoveNotation.showsNotationFloatToNDecimals moveNotation nDecimalDigits searchResult ""
							else Data.Maybe.maybe (
								Control.Exception.throw $ Data.Exception.mkNullDatum "BishBosh.UI.Raw.readMove.onCommand:\tzero turns have been made."
							) (
								showChar '"' . flip (Notation.MoveNotation.showsNotation moveNotation) "\"."
							) . Model.Game.maybeLastTurn $ Evaluation.QuantifiedGame.getGame quantifiedGame
						_		-> Control.Exception.throwIO . Data.Exception.mkRequestFailure . showString "BishBosh.UI.Raw.readMove.onCommand:\tunexpectedly failed to find any moves; " $ shows game "."	-- CAVEAT: the game should have terminated.
			 ) (
				\(qualifiedMove, names) -> putStrLn . Text.ShowColouredPrefix.showsPrefixInfo . showString "Try \"" . Notation.MoveNotation.showsNotation moveNotation qualifiedMove . showString "\" from:" $ ContextualNotation.QualifiedMoveForest.showsNames maybeMaximumPGNNames names ""
			 ) $ uncurry (
				ContextualNotation.PositionHashQualifiedMoveTree.maybeRandomlySelectOnymousQualifiedMove randomGen
			 ) (
				Input.StandardOpeningOptions.getPreferVictories &&& Input.StandardOpeningOptions.getMatchSwitches $ Input.SearchOptions.getStandardOpeningOptions searchOptions
			 ) positionHashQualifiedMoveTree game

			return {-to IO-monad-} playState	-- N.B.: though one could merely call "eventLoop", a new random-generator is desirable in case an alternative hint is requested.
		onCommand (UI.Command.Print printObject)	= do
			case printObject of
				UI.PrintObject.Configuration	-> putStrLn $ Property.ShowFloat.showsFloatToN nDecimalDigits options "."
				UI.PrintObject.Help		-> putStrLn . showString "Enter either a move in " . shows moveNotation $ showString "-notation, or:\n" UI.Command.usageMessage

			eventLoop
		onCommand UI.Command.Quit	= do
			Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowColouredPrefix.showsPrefixInfo "quitting on request."

			return {-to IO-monad-} playState { State.PlayState.getMaybeApplicationTerminationReason = Just State.ApplicationTerminationReason.byRequest }
		onCommand (UI.Command.Report reportObject)	= do
			($ game) $ case reportObject of
				UI.ReportObject.AvailableMoves	-> putStrLn . ($ ".") . Text.ShowList.showsFormattedList (
					showChar '|'
				 ) (
					Notation.MoveNotation.showsNotation moveNotation
				 ) . Model.Game.findQualifiedMovesAvailableToNextPlayer
				UI.ReportObject.Board			-> putStrLn . show2D
				UI.ReportObject.EPD			-> putStrLn . Property.ExtendedPositionDescription.showEPD
				UI.ReportObject.FEN			-> putStrLn . Property.ForsythEdwards.showFEN
				UI.ReportObject.Game			-> print
				UI.ReportObject.MaxPositionInstances	-> print . State.InstancesByPosition.findMaximumInstances . Model.Game.getInstancesByPosition
				UI.ReportObject.Moves			-> putStrLn . ($ "") . Text.ShowList.showsFormattedList' (
					Notation.MoveNotation.showsNotation moveNotation
				 ) . Model.Game.listTurnsChronologically
				UI.ReportObject.PGN			-> putStrLn . ($ ".") <=< ContextualNotation.PGN.showsGame
				UI.ReportObject.ReversiblePlyCount	-> print . State.InstancesByPosition.countConsecutiveRepeatablePlies . Model.Game.getInstancesByPosition

			eventLoop
		onCommand UI.Command.Resign	= do
			Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowColouredPrefix.showsPrefixInfo "resigning."

			return {-to IO-monad-} $ State.PlayState.resign playState
		onCommand UI.Command.Restart	= do
			Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowColouredPrefix.showsPrefixInfo "restarting game."

			Input.IOOptions.persist ioOptions (verbosity == maxBound) (Data.Default.def :: Model.Game.Game)

			return {-to IO-monad-} $ State.PlayState.resetPositionHashQuantifiedGameTree playState
		onCommand (UI.Command.RollBack maybeNPlies)	= let
			rollBack :: Type.Count.NPlies -> IO (State.PlayState.PlayState positionHash)
			rollBack nPlies
				| (game', _) : _ <- drop (fromIntegral $ pred nPlies) $ Model.Game.rollBack game	= do
					Control.Monad.when (verbosity == maxBound) . putStrLn $ show2D game'

					return {-to IO-monad-} $ State.PlayState.reconstructPositionHashQuantifiedGameTree game' playState
				| otherwise	= onCommand UI.Command.Restart
		 in Data.Maybe.maybe (
			let
				nPlies :: Type.Count.NPlies
				nPlies	= fromIntegral . succ $ Data.Foldable.length searchDepthByLogicalColour	-- In fully manual play, rollback one ply, in semi-manual play rollback two plies.
			in do
				Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixInfo . showString "rolling-back " $ shows nPlies " plies."

				rollBack nPlies
		 ) rollBack maybeNPlies
		onCommand UI.Command.Save	= do
			Data.Maybe.maybe (
				return {-to IO-monad-} $ Text.ShowColouredPrefix.showsPrefixError "the file-path at which to save the game, hasn't been defined."
			 ) (
				\(filePath, automatic) -> if automatic
					then return {-to IO-monad-} $ Text.ShowColouredPrefix.showsPrefixWarning "the state of the game is, in accordance with configuration, saved automatically."
					else Control.Exception.catch (
						do
							System.IO.withFile filePath System.IO.WriteMode (`System.IO.hPutStrLn` show game)

							return {-to IO-monad-} . Text.ShowColouredPrefix.showsPrefixInfo . showString "the game-state has been saved in " $ shows filePath "."
					 ) $ \e -> return {-to IO-monad-} . Text.ShowColouredPrefix.showsPrefixError $ show (e :: Control.Exception.SomeException)
			 ) (
				Input.IOOptions.getMaybePersistence ioOptions
			 ) >>= System.IO.hPutStrLn System.IO.stderr

			eventLoop
		onCommand (UI.Command.Set setObject)	= Control.Exception.catchJust (
			\e -> if Data.Exception.isBadData e
				then Just $ show e
				else Nothing
		 ) (
			do
				Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixInfo . showString "setting " $ shows setObject "."

				case setObject of
					UI.SetObject.EPD game'	-> do
						Control.Monad.when (verbosity == maxBound) . putStrLn $ show2D game'

						readMove positionHashQualifiedMoveTree randomGen runningWatch $ State.PlayState.reconstructPositionHashQuantifiedGameTree game' playState	-- Recurse.
					UI.SetObject.SearchDepth searchDepth
						| fullyManual	-> do
							System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixWarning $ shows setObject " requires an automated opponent."

							eventLoop
						| otherwise	-> readMove positionHashQualifiedMoveTree randomGen runningWatch playState {
							State.PlayState.getOptions	= Control.DeepSeq.force $ options {
								Input.Options.getSearchOptions	= Input.SearchOptions.setSearchDepth searchDepth $ Input.Options.getSearchOptions options
							}
						} -- Recurse.
		 ) (
			\s -> do
				Control.Monad.unless (verbosity == minBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowColouredPrefix.showsPrefixError s

				eventLoop
		 )
		onCommand UI.Command.Swap
			| fullyManual	= do
				Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixWarning . showString " there aren't any " $ shows Input.SearchOptions.searchDepthTag " to swap."

				eventLoop
			| otherwise	= do
				Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixInfo . showString "swapping " $ shows Input.SearchOptions.searchDepthTag "."

				return {-to IO-monad-} playState { State.PlayState.getOptions = Input.Options.swapSearchDepth options }

		eventLoop :: IO (State.PlayState.PlayState positionHash)
		eventLoop	= getLine >>= \line -> case Data.List.Extra.trim line of
			"?"	-> onCommand $ UI.Command.Print UI.PrintObject.Help
			':' : s	-> (
				(
					\errorMessage -> do
						System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixError $ showString errorMessage "."

						eventLoop
				) ||| (
					\(command, remainder) -> if null $ Data.List.Extra.trimStart remainder
						then onCommand command
						else do
							System.IO.hPutStrLn System.IO.stderr . showString "unexpected trailing text " $ shows remainder "."

							eventLoop
				)
			 ) . UI.Command.readsCommand $ UI.Command.autoComplete s
			s	-> let
				corrections	= State.PlayState.suggestCorrections s playState
			 in case Notation.MoveNotation.readsQualifiedMove moveNotation s of
				[(eitherQualifiedMove, "")]
					| Just errorMessage <- Model.Game.validateEitherQualifiedMove game eitherQualifiedMove	-> do
						System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixError . shows s . showString " is illegal; " $ shows errorMessage "."

						Control.Monad.unless (null corrections) . System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixInfo . showString "did you mean " $ shows corrections " ?"

						eventLoop	-- Recurse.
					| otherwise	-> do
						stoppedWatch	<- Property.Switchable.toggle runningWatch

						let
							game'		= Model.Game.applyEitherQualifiedMove eitherQualifiedMove game
							playState'	= State.PlayState.updateWithManualMove game' playState

						Control.Monad.when (verbosity == maxBound) $ do
							System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixInfo . showString Component.Move.tag . Text.ShowList.showsAssociation . shows (
								Notation.MoveNotation.showNotation moveNotation . Data.Maybe.fromMaybe (
									Control.Exception.throw $ Data.Exception.mkResultUndefined "BishBosh.UI.Raw.readMove.eventLoop:\tModel.Game.maybeLastTurn failed."
								) $ Model.Game.maybeLastTurn game'
							 ) . showString " was requested after " $ Property.ShowFloat.showsFloatToN nDecimalDigits stoppedWatch "s."

							case ContextualNotation.PositionHashQualifiedMoveTree.findNextOnymousQualifiedMovesForPosition positionHashQualifiedMoveTree game' of
								[]			-> return {-to IO-monad-} ()
								onymousQualifiedMoves	-> System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixInfo . showString "matches archived game(s):" $ ContextualNotation.QualifiedMoveForest.showsNames maybeMaximumPGNNames (
									concatMap (
										map fst {-Name-} . snd {-[OnymousResult]-}
									) onymousQualifiedMoves
								 ) ""

						return {-to IO-monad-} playState'	-- It's now the other player's move.
				[(_, remainder)]		-> do
					System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixError . showString "the specified " . showString Component.Move.tag . showString " was correctly formatted, but was followed by unexpected text" . Text.ShowList.showsAssociation $ shows remainder "."

					eventLoop	-- Recurse.
				_ {-no parse-}			-> do
					Control.Monad.unless (null s) $ do
						System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixError . shows s . showString " /~ " $ Notation.MoveNotation.showsMoveSyntax moveNotation "."	-- CAVEAT: this error also results from source == destination.

						Control.Monad.unless (null corrections) . System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixInfo . showString "did you mean " $ shows corrections " ?"

					eventLoop	-- Recurse.
	 in do
		Control.Monad.unless (verbosity == minBound) . putStrLn . showString "Enter either a move in " . shows moveNotation . showString "-notation, or '?' for " $ showString UI.PrintObject.helpTag ":"

		eventLoop
 ) ||| const (
	Control.Exception.throwIO $ Data.Exception.mkInvalidDatum "BishBosh.UI.Raw.readMove:\tunexpected CECP-options."
 ) $ Input.UIOptions.getEitherNativeUIOrCECPOptions uiOptions

-- | Plays the game.
takeTurns :: forall positionHash randomGen. (
	Data.Bits.Bits		positionHash,
	Ord			positionHash,
	System.Random.RandomGen	randomGen
 )
	=> ContextualNotation.PositionHashQualifiedMoveTree.PositionHashQualifiedMoveTree positionHash
	-> randomGen
	-> State.PlayState.PlayState positionHash
	-> IO (State.PlayState.PlayState positionHash)
{-# SPECIALISE takeTurns :: ContextualNotation.PositionHashQualifiedMoveTree.PositionHashQualifiedMoveTree Type.Crypto.PositionHash -> System.Random.StdGen -> State.PlayState.PlayState Type.Crypto.PositionHash -> IO (State.PlayState.PlayState Type.Crypto.PositionHash) #-}
takeTurns positionHashQualifiedMoveTree randomGen playState	= let
	options	= State.PlayState.getOptions playState

	(searchOptions, ioOptions)	= Input.Options.getSearchOptions &&& Input.Options.getIOOptions $ options

	uiOptions	= Input.IOOptions.getUIOptions ioOptions

	moveNotation	= Input.UIOptions.getMoveNotation uiOptions
	nDecimalDigits	= Input.UIOptions.getNDecimalDigits uiOptions
	verbosity	= Input.UIOptions.getVerbosity uiOptions
 in (
	\nativeUIOptions -> do
		mVar	<- Control.Concurrent.newEmptyMVar

		let
			show2D :: Model.Game.Game -> String
			show2D game	= State.MaybePieceByCoordinates.show2D (
				State.Board.getMaybePieceByCoordinates $ Model.Game.getBoard game
			 ) (
				snd {-columns-} $ Input.NativeUIOptions.getBoardMagnification nativeUIOptions
			 ) (
				Input.NativeUIOptions.getColourScheme nativeUIOptions
			 ) (
				Input.NativeUIOptions.getDepictFigurine nativeUIOptions
			 ) (
				Notation.MoveNotation.getOrigin moveNotation
			 )

			slave
				:: Maybe (Concurrent.Pondering.Pondering Component.Move.Move)
				-> Maybe Type.Count.NPlies
				-> [randomGen]
				-> State.PlayState.PlayState positionHash
				-> IO (State.PlayState.PlayState positionHash)
			slave maybePondering maybeMaximumPlies ~(randomGen' : randomGens) playState'	= let
				(game', searchOptions')		= State.PlayState.getGame &&& Input.Options.getSearchOptions . State.PlayState.getOptions $ playState'	-- Deconstruct.
			 in Data.Maybe.maybe (
				do
					runningWatch	<- Property.Switchable.on

					Data.Maybe.maybe (
						do
							playState''	<- readMove positionHashQualifiedMoveTree randomGen' runningWatch playState'	-- Block reading a command-sequence terminating in a move from a manual player.

							(,) playState'' <$> (
								if playState' `State.PlayState.hasMorePlies` playState''
									then {-rolled-back-} Data.Maybe.maybe (
										return {-to IO-monad-} Nothing
									) $ \pondering -> do
										Concurrent.Pondering.abort mVar pondering >>= Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowPrefix.showsPrefixInfo . showString "pondering invalidated by roll-back => "

										return {-to IO-monad-} Nothing	-- Pondering has been terminated.
									else return {-to IO-monad-}
							 ) maybePondering
					 ) (
						\searchDepth' -> Data.Maybe.maybe (
							do
								Control.Monad.when (
									verbosity > Data.Default.def && not (
										ContextualNotation.PositionHashQualifiedMoveTree.isTerminal positionHashQualifiedMoveTree
									)
								 ) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowColouredPrefix.showsPrefixInfo "failed to find any suitable archived move."

								let
									search ss	= Control.Monad.Reader.runReader (
										Search.Search.search searchDepth' ss
									 ) searchOptions'

									searchResult	= search $ State.PlayState.getSearchState playState'

								Data.Maybe.maybe (
									return {-to IO-monad-} searchResult	-- Pondering hasn't been configured, so the search must be evaluated.
								 ) (
									\pondering -> if Data.Maybe.maybe False (
										(== Concurrent.Pondering.getPremise pondering) . Component.QualifiedMove.getMove . Component.Turn.getQualifiedMove
									) $ Model.Game.maybeLastTurn game'	-- Confirm whether the pondering initiated at the start of the opponent's turn, was founded on the move they eventually made.
										then do
											Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowPrefix.showsPrefixInfo "move-premise validated => waiting."

											Control.Concurrent.takeMVar mVar	-- Blocking read, while the pondering terminates.
										else do
											Concurrent.Pondering.abort mVar pondering >>= Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowPrefix.showsPrefixInfo . showString "pondering invalidated by incorrect move-premise => "

											return {-to IO-monad-} searchResult	-- Pondering wasn't well-founded, so the search must be evaluated.
								 ) maybePondering >>= (
									\searchResult' -> let
										searchState'	= Search.Search.getSearchState searchResult'
									in case Search.Search.getQuantifiedGames searchResult' of
										quantifiedGame : continuation {-optimal move-sequence-}	-> let
											bestTurn	= Evaluation.QuantifiedGame.getLastTurn quantifiedGame
											showsMove	= Notation.MoveNotation.showsNotation moveNotation bestTurn
										 in do
											stoppedWatch	<- bestTurn `seq` Property.Switchable.toggle runningWatch

											if verbosity == minBound
												then putStrLn $ showsMove ""	-- CAVEAT: potentially machine-interpreted.
												else do
													System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixInfo . (
														if verbosity == maxBound
															then Property.ShowFloat.showsFloatToN nDecimalDigits (
																Control.Monad.Reader.runReader (
																	Evaluation.Fitness.evaluateFitness Nothing game'
																) $ Input.Options.getEvaluationOptions options
															) . Search.Search.showsSeparator	-- Prepend the fitness of the original game prior to the new result.
															else id
													 ) $ if verbosity > Data.Default.def
														then Notation.MoveNotation.showsNotationFloatToNDecimals moveNotation nDecimalDigits searchResult' . showString " in " $ Property.ShowFloat.showsFloatToN nDecimalDigits stoppedWatch "s."
														else showsMove "."

													putStrLn . show2D $ Evaluation.QuantifiedGame.getGame quantifiedGame

											(,) (
												State.PlayState.updateWithAutomaticMove (
													Metric.WeightedMeanAndCriterionValues.getCriterionValues $ Evaluation.QuantifiedGame.getWeightedMeanAndCriterionValues quantifiedGame
												) searchState' playState'
											 ) <$> if Input.SearchOptions.getUsePondering searchOptions
												then case continuation of
													quantifiedGame' {-1st move after ours in optimal move-sequence-} : _	-> fmap Just . (
														\positionHashQuantifiedGameTree'' -> Concurrent.Pondering.ponder (
															Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowPrefix.showsPrefixInfo
														) (
															showString "move-premise" . Text.ShowList.showsAssociation . ($ ".") . Notation.MoveNotation.showsNotation moveNotation &&& Component.QualifiedMove.getMove . Component.Turn.getQualifiedMove $ Evaluation.QuantifiedGame.getLastTurn quantifiedGame'
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
										_	-> Control.Exception.throwIO . Data.Exception.mkRequestFailure . showString "BishBosh.UI.Raw.takeTurns.slave:\tunexpectedly failed to find any future moves; " $ shows game' "."	-- A gameTerminationReason should have been defined.
								 )
						 ) (
							\(qualifiedMove, names) -> do
								stoppedWatch	<- Property.Switchable.toggle runningWatch

								Data.Maybe.maybe (
									return {-to IO-monad-} ()
								 ) (
									Concurrent.Pondering.abort mVar >=> Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowPrefix.showsPrefixInfo . showString "pondering pre-empted by standard-opening match => "
								 ) maybePondering

								let selectedGame	= Model.Game.applyQualifiedMove qualifiedMove game'

								if verbosity == minBound
									then putStrLn $ Notation.MoveNotation.showsNotation moveNotation qualifiedMove ""	-- CAVEAT: potentially machine-interpreted.

									else do
										Control.Monad.when (verbosity > Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixInfo . showString "selected " . Notation.MoveNotation.showsNotation moveNotation qualifiedMove . showString " from:" . ContextualNotation.QualifiedMoveForest.showsNames (
											Input.IOOptions.getMaybeMaximumPGNNames ioOptions
										 ) names . showString "\n\tin " $ Property.ShowFloat.showsFloatToN nDecimalDigits stoppedWatch "s."

										putStrLn $ show2D selectedGame

								return {-to IO-monad-} (State.PlayState.updateWithManualMove selectedGame playState', Nothing)	-- N.B.: one could ponder, but would have to construct a game-tree, & the chance of a subsequent standard-opening move is high.
						 ) $ uncurry (
							ContextualNotation.PositionHashQualifiedMoveTree.maybeRandomlySelectOnymousQualifiedMove randomGen'
						 ) (
							Input.StandardOpeningOptions.getPreferVictories &&& Input.StandardOpeningOptions.getMatchSwitches $ Input.SearchOptions.getStandardOpeningOptions searchOptions
						 ) positionHashQualifiedMoveTree game'	-- Determine whether the automated player's move can be decided by a search of recorded games or we must decide ourself.
					 ) (
						Model.Game.getNextLogicalColour game' `Map.lookup` Input.SearchOptions.getSearchDepthByLogicalColour searchOptions'	-- Determinate whether the next player is manual.
					 ) >>= (
						\(playState'', maybePondering') -> do
							Input.IOOptions.persist ioOptions (verbosity == maxBound) $ State.PlayState.getGame playState''

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
						putStrLn $ if verbosity == minBound
							then show gameTerminationReason	-- CAVEAT: potentially machine-interpreted.
							else Text.ShowColouredPrefix.showsPrefixInfo $ shows gameTerminationReason "."

						let
							criterionValueStatistics	= State.PlayState.calculateCriterionValueStatistics playState'

							showsFloat :: Double -> ShowS
							showsFloat	= Property.ShowFloat.showsFloatToN' nDecimalDigits

						Control.Monad.when (
							verbosity == maxBound && not (null criterionValueStatistics)
						 ) . System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixInfo . showString "mean & standard-deviation of criterion-values" . Text.ShowList.showsAssociation $ Text.ShowList.showsFormattedList' (
							\(mean, standardDeviation) -> showChar '(' . showsFloat mean . Text.ShowList.showsSeparator . showsFloat standardDeviation . showChar ')'
						 ) criterionValueStatistics "."

						readMove positionHashQualifiedMoveTree randomGen' (
							Control.Exception.throw $ Data.Exception.mkInvalidDatum "BishBosh.UI.Raw.takeTurns.slave:\tundefined stop-watch."
						 ) playState' {-there're zero valid moves, but the user can issue commands-} >>= slave Nothing maybeMaximumPlies randomGens	-- Tail recurse.

			 ) $ Model.Game.getMaybeTerminationReason game'

		Control.Monad.unless (verbosity == minBound) . putStrLn . show2D $ State.PlayState.getGame playState

		slave Nothing (Input.Options.getMaybeMaximumPlies options) (ToolShed.System.Random.randomGens randomGen) playState
 ) ||| const (
	Control.Exception.throwIO $ Data.Exception.mkInvalidDatum "BishBosh.UI.Raw.takeTurns:\tunexpected CECP-options."
 ) $ Input.UIOptions.getEitherNativeUIOrCECPOptions uiOptions

