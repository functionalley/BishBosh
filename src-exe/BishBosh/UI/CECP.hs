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

 [@DESCRIPTION@]	Interfaces via CECP with a GUI; <https://www.gnu.org/software/xboard/engine-intf.html#6>.
-}

module BishBosh.UI.CECP(
-- * Constants
--	hintTag,
--	moveTag,
--	offerTag,
--	pongTag,
--	protoverTag,
--	explicitEnpassant,
-- * Functions
--	tellUser,
--	mkMessage,
--	mkErrorMessage,
--	mkIllegalMoveMessage,
--	mkUnknownCommandError,
--	mkTooManyParametersError,
--	showsThinking,
--	readMove,
	takeTurns
 ) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.Rank						as Attribute.Rank
import qualified	BishBosh.Attribute.RankValues					as Attribute.RankValues
import qualified	BishBosh.Attribute.WeightedMeanAndCriterionValues		as Attribute.WeightedMeanAndCriterionValues
import qualified	BishBosh.Component.Move						as Component.Move
import qualified	BishBosh.Component.QualifiedMove				as Component.QualifiedMove
import qualified	BishBosh.Component.Turn						as Component.Turn
import qualified	BishBosh.Concurrent.Pondering					as Concurrent.Pondering
import qualified	BishBosh.ContextualNotation.PGN					as ContextualNotation.PGN
import qualified	BishBosh.ContextualNotation.PositionHashQualifiedMoveTree	as ContextualNotation.PositionHashQualifiedMoveTree
import qualified	BishBosh.ContextualNotation.QualifiedMoveForest			as ContextualNotation.QualifiedMoveForest
import qualified	BishBosh.ContextualNotation.StandardAlgebraic			as ContextualNotation.StandardAlgebraic
import qualified	BishBosh.Data.Exception						as Data.Exception
import qualified	BishBosh.Data.Time						as Data.Time
import qualified	BishBosh.Evaluation.Fitness					as Evaluation.Fitness
import qualified	BishBosh.Evaluation.PositionHashQuantifiedGameTree		as Evaluation.PositionHashQuantifiedGameTree
import qualified	BishBosh.Evaluation.QuantifiedGame				as Evaluation.QuantifiedGame
import qualified	BishBosh.Input.CECPFeatures					as Input.CECPFeatures
import qualified	BishBosh.Input.CECPOptions					as Input.CECPOptions
import qualified	BishBosh.Input.EvaluationOptions				as Input.EvaluationOptions
import qualified	BishBosh.Input.IOOptions					as Input.IOOptions
import qualified	BishBosh.Input.Options						as Input.Options
import qualified	BishBosh.Input.SearchOptions					as Input.SearchOptions
import qualified	BishBosh.Input.StandardOpeningOptions				as Input.StandardOpeningOptions
import qualified	BishBosh.Input.UIOptions					as Input.UIOptions
import qualified	BishBosh.Model.Game						as Model.Game
import qualified	BishBosh.Model.GameTerminationReason				as Model.GameTerminationReason
import qualified	BishBosh.Notation.MoveNotation					as Notation.MoveNotation
import qualified	BishBosh.Property.ForsythEdwards				as Property.ForsythEdwards
import qualified	BishBosh.Property.ShowFloat					as Property.ShowFloat
import qualified	BishBosh.Search.Search						as Search.Search
import qualified	BishBosh.Search.SearchState					as Search.SearchState
import qualified	BishBosh.State.ApplicationTerminationReason			as State.ApplicationTerminationReason
import qualified	BishBosh.State.PlayState					as State.PlayState
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
import qualified	Data.List
import qualified	Data.List.Extra
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Data.Time.Clock
import qualified	Numeric
import qualified	System.IO
import qualified	System.Random
import qualified	ToolShed.System.Random

-- | Used in output to prefix hints.
hintTag :: String
hintTag			= "Hint:"

-- | Used in output, to prefix moves.
moveTag :: String
moveTag			= "move"

-- | Used in output, to qualify an draw-offer.
offerTag :: String
offerTag		= "offer"

-- | The response to a ping request.
pongTag :: String
pongTag			= "pong"

-- | The command used to request the version of the CECP protocol.
protoverTag :: String
protoverTag		= "protover"

-- | Constant.
explicitEnpassant :: ContextualNotation.StandardAlgebraic.ExplicitEnPassant
explicitEnpassant	= False

-- | Used to prefix messages targetted at the user.
tellUser :: ShowS
tellUser	= showString "telluser" . showChar ' '

-- | Constructor.
mkMessage
	:: String	-- ^ Classification.
	-> String	-- ^ Reason.
	-> ShowS
mkMessage classification reason	= showString classification . showString " (" . showString reason . showString "): "

-- | Constructor.
mkIllegalMoveMessage :: String -> ShowS
mkIllegalMoveMessage	= mkMessage "Illegal move"

-- | Constructor.
mkErrorMessage :: String -> ShowS
mkErrorMessage	= mkMessage "Error"

-- | Constructor.
mkUnknownCommandError :: ShowS
mkUnknownCommandError	= mkErrorMessage "unknown command"

-- | Constructor.
mkTooManyParametersError :: ShowS
mkTooManyParametersError	= mkErrorMessage "too many parameters"

-- | Constructor.
mkParseFailureError :: ShowS
mkParseFailureError	= mkErrorMessage "parse-failure"

-- | Format thinking-output suitable to be posted to xboard.
showsThinking :: (
	Fractional	rankValue,
	Real		rankValue,
	Real		weightedMean
 )
	=> Input.SearchOptions.SearchDepth
	-> Input.EvaluationOptions.EvaluationOptions criterionWeight pieceSquareValue rankValue x y
	-> weightedMean
	-> Data.Time.Clock.NominalDiffTime	-- ^ Elapsed time.
	-> Component.Move.NMoves		-- ^ Nodes searched.
	-> String				-- ^ Principal variation.
	-> ShowS
showsThinking searchDepth evaluationOptions weightedMean elapsedTime nMoves principalVariation	= Text.ShowList.showsDelimitedList (showChar ' ') id id [
	shows searchDepth,
	shows . (
		round	:: Double -> Int
	) $ 100 {-centi-Pawns-} * realToFrac (
		uncurry (/) . (
			Attribute.RankValues.calculateMaximumTotalValue &&& Attribute.RankValues.findRankValue Attribute.Rank.Pawn
		) $ Input.EvaluationOptions.getRankValues evaluationOptions
	) * realToFrac weightedMean,
	shows (round $ 100 {-centi-seconds-} * elapsedTime :: Int),
	shows nMoves
 ] . showChar '\t' . showString principalVariation

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
	Control.DeepSeq.NFData	row,
	Show			column,
	Show			row,
	System.Random.RandomGen	randomGen
 )
	=> ContextualNotation.PositionHashQualifiedMoveTree.PositionHashQualifiedMoveTree T.X T.Y T.PositionHash
	-> randomGen
	-> Data.Time.Clock.UTCTime
	-> State.PlayState.PlayState column T.CriterionValue T.CriterionWeight T.PieceSquareValue T.PositionHash T.RankValue row T.WeightedMean T.X T.Y
	-> IO (State.PlayState.PlayState column T.CriterionValue T.CriterionWeight T.PieceSquareValue T.PositionHash T.RankValue row T.WeightedMean T.X T.Y)
 #-}
readMove positionHashQualifiedMoveTree randomGen	= slave where
	slave startUTCTime playState	= let
		(game, options)			= State.PlayState.getGame &&& State.PlayState.getOptions $ playState
		(searchOptions, ioOptions)	= Input.Options.getSearchOptions &&& Input.Options.getIOOptions $ options

		(searchDepthByLogicalColour, tryToMatchSwitches)	= Input.SearchOptions.getSearchDepthByLogicalColour &&& Input.StandardOpeningOptions.getMatchSwitches . Input.SearchOptions.getStandardOpeningOptions $ searchOptions
		fullyManual						= Data.Map.null searchDepthByLogicalColour

		uiOptions	= Input.IOOptions.getUIOptions ioOptions

		moveNotation	= Input.UIOptions.getMoveNotation uiOptions
		nDecimalDigits	= Input.UIOptions.getNDecimalDigits uiOptions
		verbosity	= Input.UIOptions.getVerbosity uiOptions
	 in (
		const . Control.Exception.throwIO $ Data.Exception.mkIncompatibleData "BishBosh.UI.CECP.readMove.slave:\tunexpectedly found 'NativeUIOptions'."
	 ) `either` (
		\cecpOptions -> let
			displaySAN	= Input.CECPOptions.getDisplaySAN cecpOptions

			onCommand :: UI.Command.Command x y -> IO (State.PlayState.PlayState column criterionValue criterionWeight pieceSquareValue positionHash rankValue row weightedMean x y)
			onCommand UI.Command.Hint	= do
				Control.Monad.unless (Model.Game.isTerminated game) . Data.Maybe.maybe (
					do
						Control.Monad.when (verbosity > Data.Default.def && not (ContextualNotation.PositionHashQualifiedMoveTree.isTerminal positionHashQualifiedMoveTree)) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsInfoPrefix "failed to find any suitable archived move."

						let
							searchResult	= Control.Monad.Reader.runReader (
								Search.Search.search (Input.SearchOptions.getSearchDepth searchOptions) $ State.PlayState.getSearchState playState
							 ) searchOptions

						case Search.Search.getQuantifiedGames searchResult of
							quantifiedGame : _	-> putStrLn . showString hintTag . showChar ' ' $ if displaySAN
								then Data.Maybe.maybe (
									Control.Exception.throw $ Data.Exception.mkNullDatum "BishBosh.UI.CECP.readMove.onCommand:\tzero turns have been made."
								) (
									flip (ContextualNotation.StandardAlgebraic.showTurn explicitEnpassant) game
								) . Model.Game.maybeLastTurn $ Evaluation.QuantifiedGame.getGame quantifiedGame
								else Notation.MoveNotation.showNotation moveNotation $ Evaluation.QuantifiedGame.getLastTurn (quantifiedGame :: Evaluation.QuantifiedGame.QuantifiedGame x y criterionValue weightedMean)
							_		-> Control.Exception.throwIO . Data.Exception.mkRequestFailure . showString "BishBosh.UI.CECP.readMove.slave.onCommand:\tunexpectedly failed to find any moves; " $ shows game "."	-- CAVEAT: the game should have been terminated.
				 ) (
					\(qualifiedMove, _) -> putStrLn . showString hintTag . showChar ' ' $ if displaySAN
						then ContextualNotation.StandardAlgebraic.showMove explicitEnpassant qualifiedMove game
						else Notation.MoveNotation.showNotation moveNotation qualifiedMove
				 ) $ ContextualNotation.PositionHashQualifiedMoveTree.maybeRandomlySelectOnymousQualifiedMove randomGen tryToMatchSwitches game positionHashQualifiedMoveTree

				return {-to IO-monad-} playState	-- N.B.: though one could merely call "eventLoop", a new random-generator is desirable in case an alternative hint is requested.
			onCommand (UI.Command.Print printObject)	= do
				putStrLn . tellUser =<< case printObject of
					UI.PrintObject.Board		-> return {-to IO-monad-} . show $ Model.Game.getBoard game
					UI.PrintObject.Configuration	-> return {-to IO-monad-} $ Property.ShowFloat.showsFloatToN nDecimalDigits options "."
					UI.PrintObject.FEN		-> return {-to IO-monad-} $ Property.ForsythEdwards.showFEN game
					UI.PrintObject.Game		-> return {-to IO-monad-} $ show game
					UI.PrintObject.Help		-> return {-to IO-monad-} . showString "USAGE: " . showString UI.Command.printTag . showChar ' ' $ Text.ShowList.showsDelimitedList (showChar '|') (showChar '(') (showChar ')') (map shows UI.PrintObject.range) ""
					UI.PrintObject.Moves		-> return {-to IO-monad-} $ showString (
						showString Component.Move.tag "s"
					 ) . Text.ShowList.showsAssociation $ Text.ShowList.showsFormattedList' (
						Notation.MoveNotation.showsNotation moveNotation
					 ) (
						Model.Game.listTurnsChronologically game
					 ) "."
					UI.PrintObject.PGN		-> ($ "") `fmap` ContextualNotation.PGN.showsGame game

				eventLoop
			onCommand UI.Command.Quit	= do
				Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsInfoPrefix "quitting on request."

				return {-to IO-monad-} playState { State.PlayState.getMaybeApplicationTerminationReason = Just State.ApplicationTerminationReason.byRequest }
			onCommand UI.Command.Resign	= do
				Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsInfoPrefix "resigning."

				return {-to IO-monad-} $ State.PlayState.resign playState
			onCommand UI.Command.Restart	= let
				modeNames	= [s | (s, True) <- Input.CECPOptions.getNamedModes cecpOptions]	-- List-comprehension.
			 in do
				Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "restarting game" $ (
					if null modeNames
						then id
						else showString " & leaving " . shows modeNames
				 ) "."

				return {-to IO-monad-} $ State.PlayState.reconstructPositionHashQuantifiedGameTree Data.Default.def {-game-} playState {
					State.PlayState.getOptions	= options {
						Input.Options.getIOOptions	= ioOptions {
							Input.IOOptions.getUIOptions	= uiOptions {
								Input.UIOptions.getEitherNativeUIOrCECPOptions	= Right $ Input.CECPOptions.resetModes cecpOptions	-- Retain CECP-features & protocol-version.
							}
						}
					}
				}	-- By exiting the event-loop, the new game will be persisted where appropriate.
			onCommand (UI.Command.RollBack maybeNPlies)	= let
				rollBack :: Component.Move.NMoves -> IO (State.PlayState.PlayState column criterionValue criterionWeight pieceSquareValue positionHash rankValue row weightedMean x y)
				rollBack nPlies
					| (game', _) : _ <- drop (pred nPlies) $ Model.Game.rollBack game	= return {-to IO-monad-} $ State.PlayState.reconstructPositionHashQuantifiedGameTree game' playState
					| otherwise								= onCommand UI.Command.Restart
			 in Data.Maybe.maybe (
				let
					nPlies	= succ $ Data.Map.size searchDepthByLogicalColour	-- In fully manual play, rollback one ply, in semi-manual play rollback two plies.
				in do
					Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "rolling-back " $ shows nPlies " plies."

					rollBack nPlies
			 ) (
				\nPlies -> if nPlies <= 0
					then do
						System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsErrorPrefix "the number of plies to rollback, must exceed zero."

						eventLoop
					else rollBack nPlies
			 ) maybeNPlies
			onCommand UI.Command.Save	= do
				Data.Maybe.maybe (
					return {-to IO-monad-} $ Text.ShowList.showsErrorPrefix "the file-path at which to save the game, hasn't been defined."
				 ) (
					\(filePath, automatic) -> if automatic
						then return {-to IO-monad-} $ Text.ShowList.showsWarningPrefix "the state of the game is, in accordance with configuration, saved automatically."
						else Control.Exception.catch (
							do
								System.IO.withFile filePath System.IO.WriteMode (`System.IO.hPutStrLn` show game)

								return {-to IO-monad-} . Text.ShowList.showsInfoPrefix . showString "the game-state has been saved in " $ shows filePath "."
						 ) $ \e -> return {-to IO-monad-} . Text.ShowList.showsErrorPrefix $ show (e :: Control.Exception.SomeException)
				 ) (
					Input.IOOptions.getMaybePersistence ioOptions
				 ) >>= System.IO.hPutStrLn System.IO.stderr

				eventLoop
			onCommand (UI.Command.Set setObject)
				| fullyManual	= do
					Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix $ shows UI.Command.setTag " requires an automated opponent."

					return {-to IO-monad-} playState
				| otherwise					= Control.Exception.catchJust (
					\e -> if Data.Exception.isBadData e
						then Just $ show e
						else Nothing
				) (
					do
						Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "setting " $ shows setObject "."

						slave startUTCTime playState {
							State.PlayState.getOptions	= Control.DeepSeq.force $ case setObject of
								UI.SetObject.SearchDepth searchDepth	-> options {
									Input.Options.getSearchOptions	= Input.SearchOptions.setSearchDepth searchDepth $ Input.Options.getSearchOptions options
								}
						}
				) (
					\s -> do
						Control.Monad.unless (verbosity == minBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsErrorPrefix s

						eventLoop
				)
			onCommand UI.Command.Swap
				| fullyManual	= do
					Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString " there aren't any " $ shows Input.SearchOptions.searchDepthTag " to swap."

					eventLoop
				| otherwise						= do
					Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "swapping " $ shows Input.SearchOptions.searchDepthTag "."

					return {-to IO-monad-} playState { State.PlayState.getOptions = Input.Options.swapSearchDepth options }

			eventLoop :: IO (State.PlayState.PlayState column criterionValue criterionWeight pieceSquareValue positionHash rankValue row weightedMean x y)
			eventLoop	= getLine >>= \line -> do
				Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "received \"" $ showString line "\"."

				case lex line of
					[(nullaryCommand, "")]	-> case nullaryCommand of
						"analyze"
							| Input.CECPFeatures.isFeatureDisabled Input.CECPFeatures.analyseTag cecpFeatures	-> do
								Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString Input.CECPFeatures.featureTag . Text.ShowList.showsAssociation $ shows Input.CECPFeatures.analyseTag " disabled."

								putStrLn $ mkUnknownCommandError line

								eventLoop
							| analyseMode	-> do
								Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString "already in " $ shows Input.CECPOptions.analyseModeTag "."

								eventLoop
							| otherwise	-> do
								Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "entering " $ shows Input.CECPOptions.analyseModeTag "."

								return {-to IO-monad-} playState {
									State.PlayState.getOptions	= Input.Options.setEitherNativeUIOrCECPOptions (
										Right cecpOptions { Input.CECPOptions.getAnalyseMode = True }
									) options
								}
						"black"
							| Input.CECPFeatures.isFeatureDisabled Input.CECPFeatures.coloursTag cecpFeatures	-> do
								Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString Input.CECPFeatures.featureTag . Text.ShowList.showsAssociation $ shows Input.CECPFeatures.coloursTag " disabled."

								putStrLn $ mkUnknownCommandError line

								eventLoop
							| otherwise	-> return {-to IO-monad-} $ State.PlayState.reconstructPositionHashQuantifiedGameTree game {
								Model.Game.getNextLogicalColour	= minBound
							} playState {
								State.PlayState.getOptions	= Input.Options.swapSearchDepth options
							}
						"bk"	-> do
							putStrLn . unlines . map (
								\(qualifiedMove, onymousResults) -> showChar '\t' . (
									if displaySAN
										then ContextualNotation.StandardAlgebraic.showsMove explicitEnpassant qualifiedMove game
										else Notation.MoveNotation.showsNotation moveNotation qualifiedMove
								) $ (
									case onymousResults of
										[(name, _result)]	-> showString ":\t" . showString name
										_			-> id
								) ""
							 ) $ ContextualNotation.PositionHashQualifiedMoveTree.findNextOnymousQualifiedMoves tryToMatchSwitches game positionHashQualifiedMoveTree

							eventLoop
						"computer"	-> eventLoop	-- No action required.
						"draw"		-> do
							if Input.CECPFeatures.isFeatureDisabled Input.CECPFeatures.drawTag cecpFeatures
								then do
									Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString Input.CECPFeatures.featureTag . Text.ShowList.showsAssociation $ shows Input.CECPFeatures.drawTag " disabled."

									putStrLn $ mkUnknownCommandError line
								else putStrLn . showString offerTag $ showChar ' ' Input.CECPFeatures.drawTag	-- CAVEAT: the offer may be withdrawn before this can be accepted.

							eventLoop
						"easy"
							| ponderMode	-> do
								Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "leaving " $ shows Input.CECPOptions.ponderModeTag "."

								return {-to IO-monad-} playState {
									State.PlayState.getOptions	= Input.Options.setEitherNativeUIOrCECPOptions (
										Right cecpOptions { Input.CECPOptions.getPonderMode = False }
									) options
								}
							| otherwise	-> do
								Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString "not currently in " $ shows Input.CECPOptions.ponderModeTag "."

								eventLoop
						"edit"
							| editMode	-> do
								Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString "already in " $ shows Input.CECPOptions.editModeTag "."

								eventLoop
							| otherwise	-> do
								Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "entering " $ shows Input.CECPOptions.editModeTag "."

								return {-to IO-monad-} playState {
									State.PlayState.getOptions	= Input.Options.setEitherNativeUIOrCECPOptions (
										Right cecpOptions { Input.CECPOptions.getEditMode = True }
									) options
								}
						"exit"
							| analyseMode	-> do
								Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "leaving " $ shows Input.CECPOptions.analyseModeTag "."

								return {-to IO-monad-} playState {
									State.PlayState.getOptions	= Input.Options.setEitherNativeUIOrCECPOptions (
										Right cecpOptions { Input.CECPOptions.getAnalyseMode = False }
									) options
								}
							| otherwise	-> do
								Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString "not currently in " $ shows Input.CECPOptions.analyseModeTag "."

								eventLoop
						"force"
							| forceMode	-> do
								Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString "already in " $ shows Input.CECPOptions.forceModeTag "."

								eventLoop
							| otherwise	-> do
								Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "entering " $ shows Input.CECPOptions.forceModeTag "."

								return {-to IO-monad-} playState {
									State.PlayState.getOptions	= Input.Options.setEitherNativeUIOrCECPOptions (
										Right cecpOptions { Input.CECPOptions.getForceMode = True }
									) options
								}
						"go"	-> (
							if forceMode
								then do
									Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "leaving " $ shows Input.CECPOptions.forceModeTag "."

									return {-to IO-monad-} $ Input.Options.setEitherNativeUIOrCECPOptions (
										Right cecpOptions { Input.CECPOptions.getForceMode = False }
									 ) options
								else return {-to IO-monad-} options
						 ) >>= (
							\options' -> if fullyManual
								then do
									Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString " there aren't any " $ shows Input.SearchOptions.searchDepthTag " to swap."

									eventLoop
								else let
									nextLogicalColour	= Model.Game.getNextLogicalColour game
								in if nextLogicalColour `Data.Map.member` searchDepthByLogicalColour
									then return {-to IO-monad-} playState { State.PlayState.getOptions = options' }
									else do
										Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "swapping to play " $ shows nextLogicalColour ", i.e. next."

										return {-to IO-monad-} playState { State.PlayState.getOptions = Input.Options.swapSearchDepth options' }
						 )
						"hard"
							| ponderMode	-> do
								Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString "already in " $ shows Input.CECPOptions.ponderModeTag "."

								eventLoop
							| otherwise	-> do
								Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "entering " $ shows Input.CECPOptions.ponderModeTag "."

								return {-to IO-monad-} playState {
									State.PlayState.getOptions	= Input.Options.setEitherNativeUIOrCECPOptions (
										Right cecpOptions { Input.CECPOptions.getPonderMode = True }
									) options
								}
						"hint"	-> onCommand UI.Command.Hint
						"new"	-> onCommand UI.Command.Restart >>= (
							\playState' -> let
								engineLogicalColour	= minBound
								options'		= State.PlayState.getOptions playState'
							in if Data.Map.member engineLogicalColour . Input.SearchOptions.getSearchDepthByLogicalColour $ Input.Options.getSearchOptions options'
								then return {-to IO-monad-} playState'
								else do
									Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "swapping to play " $ shows engineLogicalColour "."

									return {-to IO-monad-} playState { State.PlayState.getOptions = Input.Options.swapSearchDepth options' }
						 )
						"nopost"
							| postMode	-> do
								Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "leaving " $ shows Input.CECPOptions.postModeTag "."

								return {-to IO-monad-} playState {
									State.PlayState.getOptions	= Input.Options.setEitherNativeUIOrCECPOptions (
										Right cecpOptions { Input.CECPOptions.getPostMode = False }
									) options
								}
							| otherwise	-> do
								Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString "not currently in " $ shows Input.CECPOptions.postModeTag "."

								eventLoop
						"post"
							| postMode	-> do
								Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString "already in " $ shows Input.CECPOptions.postModeTag "."

								eventLoop
							| otherwise	-> do
								Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "entering " $ shows Input.CECPOptions.postModeTag "."

								return {-to IO-monad-} playState {
									State.PlayState.getOptions	= Input.Options.setEitherNativeUIOrCECPOptions (
										Right cecpOptions { Input.CECPOptions.getPostMode = True }
									) options
								}
						"quit"		-> onCommand UI.Command.Quit
						"random"	-> eventLoop
						"remove"	-> onCommand . UI.Command.RollBack $ Just 2
						"undo"		-> onCommand . UI.Command.RollBack $ Just 1	-- N.B.: only received in force-mode.
						"white"
							| Input.CECPFeatures.isFeatureDisabled Input.CECPFeatures.coloursTag cecpFeatures	-> do
								Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString Input.CECPFeatures.featureTag . Text.ShowList.showsAssociation $ shows Input.CECPFeatures.coloursTag " disabled."

								putStrLn $ mkUnknownCommandError line

								eventLoop
							| otherwise	-> return {-to IO-monad-} $ State.PlayState.reconstructPositionHashQuantifiedGameTree game {
								Model.Game.getNextLogicalColour = maxBound
							} playState {
								State.PlayState.getOptions	= Input.Options.swapSearchDepth options
							}
						"xboard"	-> do
							putStrLn ""

							eventLoop	-- No action required.
						"."
							| editMode	-> do
								Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "leaving " $ shows Input.CECPOptions.editModeTag "."

								return {-to IO-monad-} playState {
									State.PlayState.getOptions	= Input.Options.setEitherNativeUIOrCECPOptions (
										Right cecpOptions { Input.CECPOptions.getEditMode = False }
									) options
								}
							| analyseMode	-> Control.Exception.throwIO $ Data.Exception.mkRequestFailure "BishBosh.UI.CECP.readMove.slave.eventLoop:\tunimplemented."
							| otherwise	-> do
								Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString "not currently in either " . shows Input.CECPOptions.editModeTag . showString " or " $ shows Input.CECPOptions.analyseModeTag "."

								eventLoop
						"?"	-> do
							Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsWarningPrefix "unimplemented."

							eventLoop
						_
							| protocolVersion > 1	-> case nullaryCommand of
								"nps" {-nodes per second-}	-> do
									if Input.CECPFeatures.isFeatureDisabled Input.CECPFeatures.npsTag cecpFeatures
										then do
											Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString Input.CECPFeatures.featureTag . Text.ShowList.showsAssociation $ shows Input.CECPFeatures.npsTag " disabled."

											putStrLn $ mkUnknownCommandError line
										else Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsWarningPrefix "unimplemented."

									eventLoop
								"pause"
									| Input.CECPFeatures.isFeatureDisabled Input.CECPFeatures.pauseTag cecpFeatures	-> do
										Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString Input.CECPFeatures.featureTag . Text.ShowList.showsAssociation $ shows Input.CECPFeatures.pauseTag " disabled."

										putStrLn $ mkUnknownCommandError line

										eventLoop
									| Data.Maybe.isJust maybePaused	-> do
										Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString "already " $ shows Input.CECPFeatures.pauseTag "d."

										eventLoop
									| otherwise	-> do
										elapsedTime	<- Data.Time.measureElapsedTime startUTCTime

										Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "paused => stopping the clock at " $ Data.Time.showsTimeAsSeconds nDecimalDigits elapsedTime "."

										return {-to IO-monad-} playState {
											State.PlayState.getOptions	= Input.Options.setEitherNativeUIOrCECPOptions (
												Right cecpOptions { Input.CECPOptions.getMaybePaused = Just elapsedTime }
											) options
										}
								"playother"
									| Input.CECPFeatures.isFeatureDisabled Input.CECPFeatures.playotherTag cecpFeatures	-> do
										Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString Input.CECPFeatures.featureTag . Text.ShowList.showsAssociation $ shows Input.CECPFeatures.playotherTag " disabled."

										putStrLn $ mkUnknownCommandError line

										eventLoop
									| fullyManual	-> do
										Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString " there aren't any " $ shows Input.SearchOptions.searchDepthTag " to swap."

										eventLoop
									| otherwise	-> do
										Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "leaving " . shows Input.CECPOptions.forceModeTag . showString " & swapping to play " $ shows (Model.Game.getNextLogicalColour game) ", i.e. next."

										return {-to IO-monad-} playState {
											State.PlayState.getOptions	= Input.Options.swapSearchDepth $ Input.Options.setEitherNativeUIOrCECPOptions (
												Right cecpOptions {
													Input.CECPOptions.getForceMode	= False	-- N.B.: the engine may not currently be in force-mode.
												}
											) options
										}
								"resume"
									| Just timeTaken <- maybePaused	-> do
										startUTCTime'	<- Data.Time.Clock.getCurrentTime

										Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsInfoPrefix "resuming => restarting the clock."

										slave (
											Data.Time.Clock.addUTCTime (negate timeTaken) startUTCTime'	-- Restart the clock.
										 ) playState {
											State.PlayState.getOptions	= Input.Options.setEitherNativeUIOrCECPOptions (
												Right cecpOptions { Input.CECPOptions.getMaybePaused = Nothing }
											) options
										}
									| otherwise		-> do
										Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsWarningPrefix "not currently paused."

										eventLoop	-- Recurse.
								_
									| Input.CECPFeatures.isFeatureDisabled Input.CECPFeatures.usermoveTag cecpFeatures	-> moveCommand nullaryCommand
									| otherwise	-> do
										Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString "unrecognised unary command for " . showString protoverTag . Text.ShowList.showsAssociation . shows protocolVersion . showString "; " $ shows line "."

										putStrLn $ mkUnknownCommandError line

										eventLoop
							| Input.CECPFeatures.isFeatureDisabled Input.CECPFeatures.usermoveTag cecpFeatures	-> moveCommand nullaryCommand
							| otherwise	-> do
								Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString "unrecognised unary command for " . showString protoverTag . Text.ShowList.showsAssociation . shows protocolVersion . showString "; " $ shows line "."

								putStrLn $ mkUnknownCommandError line

								eventLoop
					[(command, arg)]	-> let
						arg'	= Data.List.Extra.trimStart arg
					 in case command of
						"cores"	-> do
							case reads arg' of
								[(nCores, "")]
									| nCores > 0	-> Control.Concurrent.setNumCapabilities nCores
									| otherwise	-> Control.Exception.throwIO . Data.Exception.mkParseFailure . showString "BishBosh.UI.CECP.readMove.slave.eventLoop:\tthe number of cores must exceed zero; " $ shows arg' "."
								_	-> Control.Exception.throwIO . Data.Exception.mkParseFailure . showString "BishBosh.UI.CECP.readMove.slave.eventLoop:\tfailed to parse an integer from " $ shows arg' "."

							eventLoop
						"egtpath" {-end-game table path-}	-> do
							Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsWarningPrefix "unimplemented."

							eventLoop
						"exclude"	-> do
							Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsWarningPrefix "unimplemented."

							eventLoop
						"hover"	-> do
							Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsWarningPrefix "unimplemented."

							eventLoop
						"include"	-> do
							Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsWarningPrefix "unimplemented."

							eventLoop
						"level"	-> do
							Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsWarningPrefix "unimplemented."

							eventLoop
						"lift"	-> do
							Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsWarningPrefix "unimplemented."

							eventLoop
						"memory"	-> do
							Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsWarningPrefix "unimplemented."

							eventLoop
						"name"	-> do
							Control.Monad.when (Input.CECPFeatures.isFeatureDisabled Input.CECPFeatures.nameTag cecpFeatures) $ do
								Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString Input.CECPFeatures.featureTag . Text.ShowList.showsAssociation $ shows Input.CECPFeatures.nameTag " disabled."

								putStrLn $ mkUnknownCommandError line

							eventLoop	-- No action required.
						"option"	-> case lex arg' of
							[("print", '=' : arg'')]	-> onCommand . UI.Command.Print $ case reads arg'' of
								[(printObject, "")]	-> printObject
								_			-> UI.PrintObject.Help
							[("set", arg'')]	-> case lex arg'' of
								[("searchDepth", '=' : arg''')]	-> case reads arg''' of
									[(searchDepth, "")]	-> onCommand . UI.Command.Set $ UI.SetObject.mkSearchDepth searchDepth
									_			-> do
										Control.Monad.unless (verbosity == minBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsErrorPrefix . showString "failed to parse " . shows Input.SearchOptions.searchDepthTag . showString " from " $ shows arg''' "."

										putStrLn $ mkParseFailureError line

										eventLoop
								_				-> do
									Control.Monad.unless (verbosity == minBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsErrorPrefix . showString "failed to parse set-" . shows Input.CECPFeatures.optionTag . showString " from " $ shows arg'' "."

									putStrLn $ mkParseFailureError line

									eventLoop
							_				-> do
								Control.Monad.unless (verbosity == minBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsErrorPrefix . showString "failed to parse " . shows Input.CECPFeatures.optionTag . showString " from " $ shows arg' "."

								putStrLn $ mkParseFailureError line

								eventLoop
						"otim" {-opponent's time-}	-> do
							if Input.CECPFeatures.isFeatureDisabled Input.CECPFeatures.timeTag cecpFeatures
								then do
									Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString Input.CECPFeatures.featureTag . Text.ShowList.showsAssociation $ shows Input.CECPFeatures.timeTag " disabled."

									putStrLn $ mkUnknownCommandError line
								else Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsWarningPrefix "unimplemented."

							eventLoop
						"protover"	-> do
							putStrLn . showString Input.CECPFeatures.featureTag . showChar ' ' . show $ foldr (
								\l -> Input.CECPFeatures.prependFeature (
									Input.CECPFeatures.optionTag,
									Right $ unwords l
								)
							 ) cecpFeatures [
								[
									UI.Command.printTag,
									Input.CECPFeatures.inputWidget,
									UI.PrintObject.configurationTag
								], [
									UI.Command.setTag,
									Input.SearchOptions.searchDepthTag,
									Input.CECPFeatures.sliderWidget
								] ++ map show [
									Data.Maybe.fromMaybe (
										Control.Exception.throw . Data.Exception.mkNullDatum . showString "BishBosh.UI.CECP.readMove.slave.eventLoop:\tundefined " $ shows Input.Options.tag "."
									) . Data.Maybe.listToMaybe $ Data.Map.elems searchDepthByLogicalColour,
									Input.SearchOptions.minimumSearchDepth,
									7	-- Arbitrary maximum.
								]
							 ]

							case reads arg' of
								[(protocolVersion', "")]	-> return {-to IO-monad-} playState {
									State.PlayState.getOptions	= Input.Options.setEitherNativeUIOrCECPOptions (
										Right $ Input.CECPOptions.setProtocolVersion protocolVersion' cecpOptions
									) options
								}
								_				-> Control.Exception.throwIO . Data.Exception.mkParseFailure . showString "BishBosh.UI.CECP.readMove.slave.eventLoop:\tfailed to parse " . shows Input.CECPOptions.protocolVersionTag . showString " from " $ shows arg' "."
						"put"	-> do
							Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsWarningPrefix "unimplemented."

							eventLoop
						"rating"	-> eventLoop
						"result"	-> case reads arg' of
							[(result, _comment)]	-> return {-to IO-monad-} $ State.PlayState.reconstructPositionHashQuantifiedGameTree (
								Model.Game.updateTerminationReasonWith result game
							 ) playState
							_			-> do
								Control.Monad.unless (verbosity == minBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsErrorPrefix "failed to parse argument to result."

								putStrLn $ mkParseFailureError line

								eventLoop
						"sd" {-set depth-}	-> case reads arg' of
							[(searchDepth, "")]	-> onCommand . UI.Command.Set $ UI.SetObject.mkSearchDepth searchDepth
							_			-> do
								Control.Monad.unless (verbosity == minBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsErrorPrefix . showString "failed to parse " . shows Input.SearchOptions.searchDepthTag . showString " from " $ shows arg' "."

								putStrLn $ mkParseFailureError line

								eventLoop
						"setscore"	-> do
							Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsWarningPrefix "unimplemented."

							eventLoop
						"st" {-set time-}	-> do
							Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsWarningPrefix "unimplemented."

							eventLoop
						"time"	-> do
							if Input.CECPFeatures.isFeatureDisabled Input.CECPFeatures.timeTag cecpFeatures
								then do
									Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString Input.CECPFeatures.featureTag . Text.ShowList.showsAssociation $ shows Input.CECPFeatures.timeTag " disabled."

									putStrLn $ mkUnknownCommandError line
								else Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsWarningPrefix "unimplemented."

							eventLoop
						"variant"	-> do
							putStrLn $ mkErrorMessage "unexpected variant" arg'

							eventLoop
						_
							| protocolVersion > 1	-> case command of
								"accepted"	-> do
									Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString Input.CECPFeatures.featureTag . Text.ShowList.showsAssociation $ shows arg' " accepted."

									eventLoop
								"ics" {-internet chess-server-}	-> do
									if Input.CECPFeatures.isFeatureDisabled Input.CECPFeatures.icsTag cecpFeatures
										then do
											Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString Input.CECPFeatures.featureTag . Text.ShowList.showsAssociation $ shows Input.CECPFeatures.icsTag " disabled."

											putStrLn $ mkUnknownCommandError line
										else Control.Monad.when (arg' == "-") . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsInfoPrefix "opponent is local."

									eventLoop
								"ping"	-> do
									if Input.CECPFeatures.isFeatureDisabled Input.CECPFeatures.pingTag cecpFeatures
										then do
											Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString Input.CECPFeatures.featureTag . Text.ShowList.showsAssociation $ shows Input.CECPFeatures.pingTag " disabled."

											putStrLn $ mkUnknownCommandError line
										else putStrLn $ showString pongTag arg

									eventLoop
								"rejected"	-> case lex arg' of	-- Any rejection of a String-valued feature because of its syntax, includes the argument.
									[(featureName, "")]
										| Just value	<- featureName `lookup` Input.CECPFeatures.getFeatures cecpFeatures	-> do
											Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString Input.CECPFeatures.featureTag . Text.ShowList.showsAssociation $ shows arg' " rejected."

											slave startUTCTime playState {
												State.PlayState.getOptions	= either (
													\i -> if i /= 0
														then \options' -> options' {
															Input.Options.getIOOptions	= Input.IOOptions.updateCECPFeature (arg', Left 0) ioOptions	-- Disable the feature.
														}
														else id
												) (
													const id	-- Though deleting the string-valued feature seems attractive, there may be more than one 'option' feature.
												) value options
											}
										| otherwise	-> do
											Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString "rejected " . showString Input.CECPFeatures.featureTag . Text.ShowList.showsAssociation $ shows arg' " unrecognised."

											eventLoop
									[(featureName, stringArg)]	-> do
										Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString Input.CECPFeatures.featureTag . Text.ShowList.showsAssociation $ shows arg' " rejected because of syntax."

										slave startUTCTime playState {
											State.PlayState.getOptions	= options {
												Input.Options.getIOOptions	= Input.IOOptions.deleteCECPFeature (
													featureName,
													Right $ Data.List.Extra.trimStart stringArg
												) ioOptions
											}
										}
									_	-> eventLoop
								"setboard"
									| Input.CECPFeatures.isFeatureDisabled Input.CECPFeatures.setboardTag cecpFeatures	-> do
										Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString Input.CECPFeatures.featureTag . Text.ShowList.showsAssociation $ shows Input.CECPFeatures.setboardTag " disabled."

										putStrLn $ mkUnknownCommandError line

										eventLoop
									| otherwise	-> Control.Exception.catchJust (
										\e -> if Data.Exception.isBadData e
											then Just $ show e
											else Nothing
									) (
										case Property.ForsythEdwards.readsFEN arg' of
											[(game', _)]	-> return {-to IO-monad-} $ State.PlayState.reconstructPositionHashQuantifiedGameTree game' playState
											_		-> do
												Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsErrorPrefix "parse-failure."

												putStrLn $ mkParseFailureError line

												eventLoop
									) (
										\s -> do
											Control.Monad.unless (verbosity == minBound) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsErrorPrefix s

											putStrLn $ mkParseFailureError s

											eventLoop
									)
								"usermove"
									| Input.CECPFeatures.isFeatureDisabled Input.CECPFeatures.usermoveTag cecpFeatures	-> do
										Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString Input.CECPFeatures.featureTag . Text.ShowList.showsAssociation $ shows Input.CECPFeatures.usermoveTag " disabled."

										putStrLn $ mkUnknownCommandError line

										eventLoop
									| otherwise	-> moveCommand arg'
								_	-> do
									putStrLn $ mkUnknownCommandError line

									eventLoop
							| otherwise	-> do
								Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString "unrecognised command for " . showString protoverTag . Text.ShowList.showsAssociation . shows protocolVersion . showString "; " $ shows (line, command, arg') "."

								putStrLn $ mkUnknownCommandError line

								eventLoop
					_	-> do
						Control.Monad.when (verbosity >= Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsWarningPrefix . showString "unrecognised command for " . showString protoverTag . Text.ShowList.showsAssociation . shows protocolVersion . showString "; " $ shows line "."

						putStrLn $ mkUnknownCommandError line

						eventLoop
				where
					(cecpFeatures, protocolVersion)	= Input.CECPOptions.getCECPFeatures &&& Input.CECPOptions.getProtocolVersion $ cecpOptions

					[analyseMode, editMode, forceMode, ponderMode, postMode]	= map ($ cecpOptions) [
						Input.CECPOptions.getAnalyseMode,
						Input.CECPOptions.getEditMode,
						Input.CECPOptions.getForceMode,
						Input.CECPOptions.getPonderMode,
						Input.CECPOptions.getPostMode
					 ]

					maybePaused	= Input.CECPOptions.getMaybePaused cecpOptions

					moveCommand :: String -> IO (State.PlayState.PlayState column criterionValue criterionWeight pieceSquareValue positionHash rankValue row weightedMean x y)
					moveCommand moveString	= case Notation.MoveNotation.readsQualifiedMove moveNotation moveString of
						[(eitherQualifiedMove, "")]
							| Just errorMessage <- Model.Game.validateEitherQualifiedMove eitherQualifiedMove game	-> do
								Control.Monad.unless (verbosity == minBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsErrorPrefix . shows moveString . showString " is illegal; " $ shows errorMessage "."

								putStrLn $ mkIllegalMoveMessage errorMessage moveString

								eventLoop	-- Recurse.
							| otherwise	-> do
								elapsedTime	<- if forceMode
									then return {-to IO-monad-} 0
									else Data.Time.measureElapsedTime startUTCTime

								let
									game'		= Model.Game.applyEitherQualifiedMove eitherQualifiedMove game
									playState'	= State.PlayState.updateWithManualMove game' playState

								Control.Monad.when (verbosity == maxBound) $ do
									System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString Component.Move.tag . Text.ShowList.showsAssociation . shows (
										Notation.MoveNotation.showNotation moveNotation . Data.Maybe.fromMaybe (
											Control.Exception.throw $ Data.Exception.mkResultUndefined "BishBosh.UI.CECP.readMove.onCommand.moveCommand:\tModel.Game.maybeLastTurn failed."
										) $ Model.Game.maybeLastTurn game'
									 ) . showString " was requested after " $ Data.Time.showsTimeAsSeconds nDecimalDigits elapsedTime "."

									case ContextualNotation.PositionHashQualifiedMoveTree.findNextOnymousQualifiedMovesForPosition game' positionHashQualifiedMoveTree of
										[]			-> return ()
										onymousQualifiedMoves	-> System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "matches archived game(s):" $ ContextualNotation.QualifiedMoveForest.showsNames (
											Input.IOOptions.getMaybeMaximumPGNNames ioOptions
										 ) (
											concatMap (
												map fst {-Name-} . snd {-[OnymousResult]-}
											) onymousQualifiedMoves
										 ) ""

								return {-to IO-monad-} playState'	-- It's now the other player's move.
						[(_, remainder)]	-> do
							Control.Monad.unless (verbosity == minBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsErrorPrefix . showString "the specified " . showString Component.Move.tag . showString " was correctly formatted, but was followed by unexpected text" . Text.ShowList.showsAssociation $ shows remainder "."

							putStrLn $ mkTooManyParametersError moveString

							eventLoop	-- Recurse.
						_ {-no parse-}		-> do
							Control.Monad.unless (null moveString || verbosity == minBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsErrorPrefix . shows moveString . showString " /~ " $ Notation.MoveNotation.showsMoveSyntax moveNotation "."	-- CAVEAT: this error also results from source == destination.

							putStrLn $ mkIllegalMoveMessage "parse-failure" moveString

							eventLoop	-- Recurse.
		in eventLoop
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
	Control.DeepSeq.NFData	rankValue,
	Control.DeepSeq.NFData	row,
	Fractional		rankValue,
	Real			rankValue,
	Show			column,
	Show			row,
	System.Random.RandomGen	randomGen
 )
	=> ContextualNotation.PositionHashQualifiedMoveTree.PositionHashQualifiedMoveTree T.X T.Y T.PositionHash
	-> randomGen
	-> State.PlayState.PlayState column T.CriterionValue T.CriterionWeight T.PieceSquareValue T.PositionHash rankValue row T.WeightedMean T.X T.Y
	-> IO (State.PlayState.PlayState column T.CriterionValue T.CriterionWeight T.PieceSquareValue T.PositionHash rankValue row T.WeightedMean T.X T.Y)
 #-}
takeTurns positionHashQualifiedMoveTree randomGen playState	= do
	mVar	<- Control.Concurrent.newEmptyMVar

	let
		options	= State.PlayState.getOptions playState

		ioOptions		= Input.Options.getIOOptions options
		evaluationOptions	= Input.Options.getEvaluationOptions options
		searchOptions		= Input.Options.getSearchOptions options

		(uiOptions, maybeMaximumPGNNames)	= Input.IOOptions.getUIOptions &&& Input.IOOptions.getMaybeMaximumPGNNames $ ioOptions

		moveNotation	= Input.UIOptions.getMoveNotation uiOptions
		nDecimalDigits	= Input.UIOptions.getNDecimalDigits uiOptions
		verbosity	= Input.UIOptions.getVerbosity uiOptions

		slave :: Maybe (Concurrent.Pondering.Pondering (Component.Move.Move x y)) -> Maybe Component.Move.NMoves -> [randomGen] -> State.PlayState.PlayState column criterionValue criterionWeight pieceSquareValue positionHash rankValue row weightedMean x y -> IO (State.PlayState.PlayState column criterionValue criterionWeight pieceSquareValue positionHash rankValue row weightedMean x y)
		slave maybePondering maybeMaximumPlies ~(randomGen' : randomGens) playState'	= let
			(game', options')		= State.PlayState.getGame &&& State.PlayState.getOptions $ playState'
			searchOptions'			= Input.Options.getSearchOptions options'
			uiOptions'			= Input.IOOptions.getUIOptions $ Input.Options.getIOOptions options'
			cecpOptions'			= either (
				const . Control.Exception.throw $ Data.Exception.mkIncompatibleData "BishBosh.UI.CECP.takeTurns.slave:\tunexpectedly found 'NativeUIOptions'."
			 ) id $ Input.UIOptions.getEitherNativeUIOrCECPOptions uiOptions'
			(ponderMode, isPostMode)	= Input.CECPOptions.getPonderMode &&& Input.CECPOptions.getPostMode $ cecpOptions'
		 in Data.Maybe.maybe (
			do
				startUTCTime	<- Data.Time.Clock.getCurrentTime

				Data.Maybe.maybe (
					do
						playState''	<- readMove positionHashQualifiedMoveTree randomGen' startUTCTime playState'	-- Read the user's command or move.

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
							 ) . System.IO.hPutStrLn System.IO.stderr $ Text.ShowList.showsInfoPrefix "failed to find any suitable archived move."

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
									quantifiedGames@(quantifiedGame : continuation)	-> let
										bestTurn	= Evaluation.QuantifiedGame.getLastTurn quantifiedGame
									 in do
										elapsedTime	<- bestTurn `seq` Data.Time.measureElapsedTime startUTCTime

										Control.Monad.when (verbosity > Data.Default.def) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . (
											if verbosity == maxBound
												then Property.ShowFloat.showsFloatToN nDecimalDigits (
													Control.Monad.Reader.runReader (
														Evaluation.Fitness.evaluateFitness Nothing game'
													) evaluationOptions	:: Attribute.WeightedMeanAndCriterionValues.WeightedMeanAndCriterionValues weightedMean criterionValue
												) . Search.Search.showsSeparator	-- Prepend the fitness of the original game prior to the result.
												else id
										 ) . Notation.MoveNotation.showsNotationFloatToNDecimals moveNotation nDecimalDigits searchResult' . showString " in " $ Data.Time.showsTimeAsSeconds nDecimalDigits elapsedTime "."

										Control.Monad.when isPostMode . putStrLn $ showsThinking searchDepth' evaluationOptions (
											Evaluation.QuantifiedGame.getFitness $ last quantifiedGames
										 ) elapsedTime (
											Search.Search.getNMovesEvaluated searchResult'
										 ) (
											Text.ShowList.showsDelimitedList (
												showChar ' '
											) id id (
												zipWith (
													\turn originalGame -> if either (
														const . Control.Exception.throw $ Data.Exception.mkIncompatibleData "BishBosh.UI.CECP.takeTurns:\tunexpectedly found 'NativeUIOptions'."
													) Input.CECPOptions.getDisplaySAN $ Input.UIOptions.getEitherNativeUIOrCECPOptions uiOptions
														then ContextualNotation.StandardAlgebraic.showsTurn explicitEnpassant turn originalGame
														else Notation.MoveNotation.showsNotation moveNotation turn
												) (
													map Evaluation.QuantifiedGame.getLastTurn quantifiedGames
												) $ game' : map Evaluation.QuantifiedGame.getGame quantifiedGames
											) "."
										 ) ""

										putStrLn . showString moveTag . showChar ' ' $ Notation.MoveNotation.showNotation moveNotation bestTurn	-- Send the move to the GUI.

										(,) (
											State.PlayState.updateWithAutomaticMove (
												Attribute.WeightedMeanAndCriterionValues.getCriterionValues $ Evaluation.QuantifiedGame.getWeightedMeanAndCriterionValues quantifiedGame
											) searchState' playState'
										 ) `fmap` if ponderMode && Input.SearchOptions.getUsePondering searchOptions
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
													Control.Exception.throw $ Data.Exception.mkIncompatibleData "BishBosh.UI.CECP.takeTurns.slave:\tData.RoseTree.reduce failed."
												 ) . Evaluation.PositionHashQuantifiedGameTree.reduce (
													(== Evaluation.QuantifiedGame.getLastTurn quantifiedGame') . Evaluation.QuantifiedGame.getLastTurn . Evaluation.PositionHashQuantifiedGameTree.getQuantifiedGame
												 ) $ Search.SearchState.getPositionHashQuantifiedGameTree searchState'
												_		-> return {-to IO-monad-} Nothing
											else return {-to IO-monad-} Nothing
									_	-> Control.Exception.throwIO . Data.Exception.mkRequestFailure . showString "BishBosh.UI.CECP.takeTurns.slave:\tunexpectedly failed to find any moves; " $ shows game' "."	-- A gameTerminationReason should have been defined.
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

							Control.Monad.when (verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "selected " . Notation.MoveNotation.showsNotation moveNotation qualifiedMove . showString " from:" . ContextualNotation.QualifiedMoveForest.showsNames maybeMaximumPGNNames names . showString "\n\tin " $ Data.Time.showsTimeAsSeconds nDecimalDigits elapsedTime "."

							Control.Monad.when isPostMode . putStrLn $ showsThinking searchDepth' evaluationOptions (0 :: weightedMean) elapsedTime 0 (
								Data.List.intercalate (
									".\n" ++ replicate 4 ' '	-- Continuations must be preceded by at least 4 spaces.
								) $ Data.Maybe.maybe names (`take` names) maybeMaximumPGNNames
							 ) "."

							putStrLn . showString moveTag . showChar ' ' $ Notation.MoveNotation.showNotation moveNotation qualifiedMove	-- Send the move to the GUI.

							return {-to IO-monad-} (State.PlayState.updateWithManualMove selectedGame playState', Nothing)	-- N.B.: one could ponder, but would have to construct a game-tree, & the chance of a subsequent standard-opening move is high.
					 ) $ ContextualNotation.PositionHashQualifiedMoveTree.maybeRandomlySelectOnymousQualifiedMove randomGen' (
						Input.StandardOpeningOptions.getMatchSwitches $ Input.SearchOptions.getStandardOpeningOptions searchOptions
					 ) game' positionHashQualifiedMoveTree
				 ) (
					if Input.UIOptions.isCECPManualMode uiOptions'
						then Nothing
						else Model.Game.getNextLogicalColour game' `Data.Map.lookup` Input.SearchOptions.getSearchDepthByLogicalColour searchOptions'
				 ) >>= (
					\(playState'', maybePondering') -> do
						Data.Maybe.maybe (
							return {-to IO-monad-} ()
						 ) (
							\(filePath, automatic) -> let
								game''	= State.PlayState.getGame playState''
							in Control.Monad.when (automatic && show game'' /= show game') . Control.Exception.catch (
								System.IO.withFile filePath System.IO.WriteMode (`System.IO.hPrint` game'')
							) $ \e -> System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsErrorPrefix $ show (e :: Control.Exception.SomeException)
						 ) $ Input.IOOptions.getMaybePersistence ioOptions

						if State.PlayState.hasApplicationTerminationBeenRequested playState''
							then return {-to IO-monad-} playState''
							else if Data.Maybe.maybe False (<= 1) maybeMaximumPlies
								then return {-to IO-monad-} playState'' { State.PlayState.getMaybeApplicationTerminationReason = Just State.ApplicationTerminationReason.maximumPlies }
								else slave maybePondering' (fmap pred maybeMaximumPlies) randomGens playState''	-- Tail recurse.
				 )
		 ) (
			\gameTerminationReason -> do
				putStrLn . shows (Model.GameTerminationReason.toResult gameTerminationReason) . showString " {" $ shows gameTerminationReason "}"	-- Send the result to the GUI.

				let
					criterionValueStatistics	= State.PlayState.calculateCriterionValueStatistics playState'

					showsFloat :: Double -> ShowS
					showsFloat	= Numeric.showFFloat $ Just nDecimalDigits

				Control.Monad.when (
					verbosity == maxBound && not (null criterionValueStatistics)
				 ) . System.IO.hPutStrLn System.IO.stderr . Text.ShowList.showsInfoPrefix . showString "mean & standard-deviation of criterion-values" . Text.ShowList.showsAssociation $ Text.ShowList.showsFormattedList' (
					\(mean, standardDeviation) -> showChar '(' . showsFloat mean . Text.ShowList.showsSeparator . showsFloat standardDeviation . showChar ')'
				 ) criterionValueStatistics "."

				readMove positionHashQualifiedMoveTree randomGen' (
					Control.Exception.throw $ Data.Exception.mkInvalidDatum "BishBosh.UI.CECP.takeTurns.slave:\tundefined startUTCTime."
				 ) playState' {-there're zero valid moves, but the user can issue commands-} >>= slave Nothing maybeMaximumPlies randomGens	-- Tail recurse.

		 ) $ Model.Game.getMaybeTerminationReason game'

	Control.Exception.bracket (
		mapM System.IO.hGetBuffering fileHandles <* mapM_ (`System.IO.hSetBuffering` System.IO.LineBuffering) fileHandles
	 ) (
		mapM_ (uncurry System.IO.hSetBuffering) . zip fileHandles
	 ) . const $ slave Nothing (Input.Options.getMaybeMaximumPlies options) (ToolShed.System.Random.randomGens randomGen) playState where
		fileHandles	= [System.IO.stdin, System.IO.stdout]	-- Those file-handles which need special buffering for the duration of the interaction, by CECP, with the GUI.

