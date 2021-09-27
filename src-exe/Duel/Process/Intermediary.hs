{-# LANGUAGE CPP #-}
{-
	Copyright (C) 2021 Dr. Alistair Ward

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

 [@DESCRIPTION@]	Starts the two bishbosh-processes & acts as an intermediary.
-}

module Duel.Process.Intermediary (
-- * Types
-- ** Type-synonyms
--	IOHandles,
--	GameTerminationReasonsMap,
-- * Functions
--	runBishBosh,
--	readMove,
--	copyMove,
--	play,
--	purge,
--	startGame
--	startProcess,
--	bracketProcess
	initialise
) where

import			Control.Arrow((&&&), (|||))
import			Control.Category((>>>))
import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Data.Exception			as Data.Exception
import qualified	BishBosh.Input.CommandLineOption	as Input.CommandLineOption
import qualified	BishBosh.Input.IOOptions		as Input.IOOptions
import qualified	BishBosh.Input.Options			as Input.Options
import qualified	BishBosh.Input.SearchOptions		as Input.SearchOptions
import qualified	BishBosh.Input.UIOptions		as Input.UIOptions
import qualified	BishBosh.Input.Verbosity		as Input.Verbosity
import qualified	BishBosh.Notation.MoveNotation		as Notation.MoveNotation
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	BishBosh.Property.SelfValidating	as Property.SelfValidating
import qualified	BishBosh.Property.Switchable		as Property.Switchable
import qualified	BishBosh.Rule.GameTerminationReason	as Rule.GameTerminationReason
import qualified	BishBosh.Text.ShowList			as Text.ShowList
import qualified	BishBosh.Time.GameClock			as Time.GameClock
import qualified	BishBosh.Type.Count			as Type.Count
import qualified	BishBosh.Type.Length			as Type.Length
import qualified	BishBosh.Type.Mass			as Type.Mass
import qualified	BishBosh.UI.Command			as UI.Command
import qualified	Control.Exception
import qualified	Control.Monad
import qualified	Data.Default
import qualified	Data.Map.Strict
import qualified	Data.Maybe
import qualified	Duel.Data.Options			as Data.Options
import qualified	Duel.IO.Logger				as IO.Logger
import qualified	Duel.Process.Handles			as Process.Handles
import qualified	System.Exit
import qualified	System.FilePath
import qualified	System.IO
import qualified	Text.XML.HXT.Core			as HXT

#ifdef MOVE_NOTATION
#	if MOVE_NOTATION == 'I'
import qualified	BishBosh.Notation.ICCFNumeric		as Notation.ICCFNumeric
type MoveNotation	= Notation.ICCFNumeric.ICCFNumeric
#	elif MOVE_NOTATION == 'P'
import qualified	BishBosh.Notation.PureCoordinate	as Notation.PureCoordinate
type MoveNotation	= Notation.PureCoordinate.PureCoordinate
#	elif MOVE_NOTATION == 'S'
import qualified	BishBosh.Notation.Smith			as Notation.Smith
type MoveNotation	= Notation.Smith.Smith
#	else
#		error "MOVE_NOTATION ill-defined"
#	endif
#else
#	error "MOVE_NOTATION undefined"
#endif

-- | Fork 'bishbosh' using the specified configuration-file & return its IO-handles.
runBishBosh
	:: Input.Verbosity.Verbosity
	-> System.FilePath.FilePath	-- ^ The path to a configuration-file.
	-> IO Process.Handles.Handles
runBishBosh verbosity configFilePath	= let
	command	= "bishbosh"
	args	= [
		showString Input.CommandLineOption.longFlagPrefix . showString Input.Verbosity.tag . showChar '=' $ show (minBound :: Input.Verbosity.Verbosity),	-- CAVEAT: any greater verbosity will return a board-image encoded within a string.
		showString Input.CommandLineOption.longFlagPrefix . showString "inputConfigFilePath" $ showChar '=' configFilePath,
		"+RTS",
		"-N2",	-- Two CPU cores.
		"-RTS"
	 ]
 in do
	Control.Monad.when (verbosity > Data.Default.def) . IO.Logger.printInfo . showString "Starting command; " $ shows (command, args) "."

	Process.Handles.mkHandles command args

-- | Read either a move or a game-termination reason from the specified handle.
readMove
	:: Input.Verbosity.Verbosity
	-> Type.Count.NSeconds				-- ^ Read-timout.
	-> Attribute.LogicalColour.LogicalColour	-- ^ Whose turn it is.
	-> System.IO.Handle				-- ^ Output handle from which data should be read.
	-> IO (Either Rule.GameTerminationReason.GameTerminationReason (MoveNotation Type.Length.X Type.Length.Y))
readMove verbosity readTimeout logicalColour stdOut = do
	Control.Monad.when (verbosity == maxBound) . IO.Logger.printInfo . showString "Waiting for " $ shows logicalColour "."

	inputReady	<- System.IO.hWaitForInput stdOut $ 1000 * fromIntegral readTimeout

	Control.Monad.unless inputReady . Control.Exception.throwIO . Data.Exception.mkRequestFailure . showString "Duel.Process.Intermediary.readMove:\ttimed-out after " $ shows readTimeout " s."

	line	<- System.IO.hGetLine stdOut

	case reads line of
		[(gameTerminationReason, "")]	-> do
			Control.Monad.when (verbosity > minBound) . IO.Logger.printInfo $ shows gameTerminationReason " => game over."

			return {-to IO-monad-} $ Left gameTerminationReason	-- Return the result.
		_				-> case reads line of
			[(moveNotation, "")]	-> do	-- CAVEAT: the only move-notation currently supported.
				Control.Monad.when (verbosity > Data.Default.def) . IO.Logger.printInfo . showString "Read from " . shows logicalColour . showString "; move='" $ shows moveNotation "'."

				return {-to IO-monad-} $ Right moveNotation
			_			-> Control.Exception.throwIO . Data.Exception.mkParseFailure . showString "Duel.Process.Intermediary.readMove:\tfailed to parse response from " . shows logicalColour . showString "; " $ show line

-- | Read either a move from the first handle & write it to the second.
copyMove
	:: Input.Verbosity.Verbosity
	-> Type.Count.NSeconds				-- ^ Read-timout.
	-> Attribute.LogicalColour.LogicalColour	-- ^ Whose turn it is.
	-> System.IO.Handle				-- ^ Output handle from which move should be read.
	-> System.IO.Handle				-- ^ Input handle to which move should be forwarded.
	-> IO (Maybe Rule.GameTerminationReason.GameTerminationReason)
copyMove verbosity readTimeout logicalColour stdOut stdIn = do
	readMove verbosity readTimeout logicalColour stdOut >>= return {-to IO-monad-} . Just ||| (
		\move	-> do
			System.IO.hPrint stdIn move

			return {-to IO-monad-} Nothing
	 )

-- | Contains /stdin/ & /stdout/ handles respectively.
type IOHandles	= (System.IO.Handle, System.IO.Handle)

-- | Shuttle moves between the two child processes until the game terminates.
play
	:: Property.Switchable.Switchable gameClock
	=> Input.Verbosity.Verbosity
	-> Type.Count.NSeconds	-- ^ Read-timout.
	-> IOHandles
	-> IOHandles
	-> gameClock
	-> IO (Rule.GameTerminationReason.GameTerminationReason, gameClock)
play verbosity readTimeout	= slave maxBound	where
	slave logicalColour producer@(_, stdOut) consumer@(stdIn', stdOut') gameClock	= copyMove verbosity readTimeout logicalColour stdOut stdIn' >>= Data.Maybe.maybe (
		do
			Control.Monad.when (verbosity == maxBound) $ IO.Logger.printInfo "Swapping player-roles."

			Property.Switchable.toggle gameClock >>= slave (Property.Opposable.getOpposite logicalColour) consumer producer	-- Recurse.
	 ) (
		\gameTerminationReason -> do
			readMove verbosity readTimeout logicalColour stdOut' >>= (
				\gameTerminationReason' -> do
					Control.Monad.unless (gameTerminationReason == gameTerminationReason') . Control.Exception.throwIO . Data.Exception.mkIncompatibleData . showString "Duel.Process.Intermediary.play:\tsecond game terminated for a different reason; " $ shows gameTerminationReason' "."

					return {-to IO-monad-} (gameTerminationReason, gameClock)
			 ) ||| (
				\move	-> Control.Exception.throwIO . Data.Exception.mkParseFailure . showString "Duel.Process.Intermediary.play:\tread from " . shows (Property.Opposable.getOpposite logicalColour) . showString ", unexpected move='" $ shows move "'."
			 )
	 )

-- | Purge the specified handle & discard the results.
purge :: System.IO.Handle -> IO ()
purge handle	= do
	isReady	<- System.IO.hReady handle

	Control.Monad.when isReady . Control.Monad.void $ System.IO.hGetLine handle

{- |
	* Accumulates the frequency of each game-termination reason

	* N.B.: the recorded result is merely a string, though it could be read into a 'BishBosh.Rule.GameTerminationReason'.
-}
type GameTerminationReasonsMap	= Data.Map.Strict.Map Rule.GameTerminationReason.GameTerminationReason Type.Count.NGames

{- |
	* Constructs a game-clock.

	* Starts two independently configured (though of matching move-notation) concurrent instances of 'bishbosh'.

	* One instance automates White & one automates Black.

	* Shuttle moves between the instances.

	* Plays repeatedly, measuring both the total time taken by each side & accumulating the final results of each game.

	* Prints the total time taken by either side, & returns the accumulated results of each game.
-}
startGame
	:: Input.Verbosity.Verbosity
	-> Type.Count.NDecimalDigits
	-> Type.Count.NSeconds	-- ^ Read-timeout.
	-> IOHandles		-- ^ White's handles.
	-> IOHandles		-- ^ Black's handles.
	-> GameTerminationReasonsMap
	-> Type.Count.NGames
	-> IO GameTerminationReasonsMap
startGame verbosity nDecimalDigits readTimeout producer consumer gameTerminationReasonsMap nGames	= Property.Switchable.on >>= slave gameTerminationReasonsMap nGames where
	slave :: GameTerminationReasonsMap -> Type.Count.NGames -> Time.GameClock.GameClock -> IO GameTerminationReasonsMap
	slave gameTerminationReasonsMap' 0 gameClock	= do
		Time.GameClock.showsElapsedTimes nDecimalDigits gameClock >>= IO.Logger.printInfo . showString "Elapsed time" . Text.ShowList.showsAssociation . ($ ".")

		return {-to IO-monad-} gameTerminationReasonsMap'
	slave gameTerminationReasonsMap' nGames' gameClock	= do
		Control.Monad.when (verbosity == maxBound) $ IO.Logger.printInfo "Starting game."

		(gameTerminationReason, gameClock')	<- play verbosity readTimeout producer consumer gameClock

		sequence_ $ [
			\(_, stdOut) -> do
				Control.Monad.when (verbosity == maxBound) $ IO.Logger.printInfo "Purging child's stdout."

				purge stdOut,
			\(stdIn, _) -> do
				Control.Monad.when (verbosity == maxBound) $ IO.Logger.printInfo "Requesting restart."

				System.IO.hPutStrLn stdIn $ UI.Command.commandPrefix : UI.Command.restartTag
		 ] <*> [consumer, producer]

		slave (
			Data.Map.Strict.insertWith (const succ) gameTerminationReason 1 gameTerminationReasonsMap'
		 ) (
			pred nGames' -- Recurse.
		 ) gameClock'

-- | Start 'bishbosh', print any errors, & return the process-handles.
startProcess
	:: Input.Verbosity.Verbosity
	-> System.FilePath.FilePath	-- ^ The path to a configuration-file.
	-> IO Process.Handles.Handles
startProcess verbosity configFilePath	= do
	handles	<- runBishBosh verbosity configFilePath

	IO.Logger.dump $ Process.Handles.getStdErr handles

	Control.Monad.when (verbosity == maxBound) $ Process.Handles.showHandles handles >>= mapM_ IO.Logger.printInfo

	return {-to IO-monad-} handles

-- | Starts the process, performs the requested action, then clean-up.
bracketProcess
	:: Input.Verbosity.Verbosity
	-> [System.FilePath.FilePath]		-- ^ The configuration-file paths for White & Black respectively.
	-> ([Process.Handles.Handles] -> IO ())	-- ^ Run the game.
	-> IO ()
bracketProcess verbosity inputConfigFilePaths	= Control.Exception.bracket (
	startProcess verbosity `mapM` inputConfigFilePaths
 ) (
	sequence . (
		[
			IO.Logger.dump . Process.Handles.getStdErr,
			Process.Handles.cleanupHandles
		] <*>
	)
 )

{- |
	* Unpacks the configuration-options.

	* Optionally verifies the compatibility of the two configuration-files.

	* Plays the requested number of games.

	* Prints the results.
-}
initialise :: Data.Options.Options -> IO ()
initialise options
	| errorMessages@(_ : _)	<- Property.SelfValidating.findInvalidity options	= Control.Exception.throwIO . Data.Exception.mkInsufficientData . showString "Duel.Process.Intermediary.initialise:\tinvalid options; " $ show errorMessages
	| otherwise									= let
		(verbosity, inputConfigFilePaths)	= Data.Options.getVerbosity &&& Data.Options.getInputConfigFilePaths $ options
		hxtTraceLevel				= fromEnum verbosity `min` 2 {-CAVEAT: HXT trace-levels 3 & 4 are too verbose-}
	in Control.Exception.catch (
		do
			Control.Monad.when (Data.Options.getVerifyConfiguration options) $ do
				[(logicalColoursFirst, moveNotationFirst), (logicalColoursSecond, moveNotationSecond)]	<- mapM (
					\configFilePath -> do
						[pair]	<- HXT.runX $ HXT.setTraceLevel hxtTraceLevel
							>>> HXT.xunpickleDocument HXT.xpickle [
								HXT.withRemoveWS HXT.yes,
								HXT.withStrictInput HXT.no	-- Only a fraction of the document is required.
							] configFilePath
							>>> HXT.arr (
								 \inputOptions -> Input.SearchOptions.identifyAutomatedPlayers . Input.Options.getSearchOptions &&& Input.UIOptions.getMoveNotation . Input.IOOptions.getUIOptions . Input.Options.getIOOptions $ (
									inputOptions	:: Input.Options.Options Type.Length.X Type.Mass.PieceSquareValue Type.Length.Y Type.Length.X Type.Length.Y	-- Arbitrary concrete type.
								 )
							) -- Lift function into an arrow.

						return {-to IO-monad-} pair
				 ) inputConfigFilePaths

				Control.Monad.unless (logicalColoursFirst == [maxBound] && logicalColoursSecond == [minBound]) . Control.Exception.throwIO . Data.Exception.mkIncompatibleData . showString "Duel.Process.Intermediary.initialise:\tconfiguration-files must automate White & Black respectively; " $ shows (logicalColoursFirst ++ logicalColoursSecond) "."

				Control.Monad.unless (moveNotationFirst == moveNotationSecond) . Control.Exception.throwIO . Data.Exception.mkIncompatibleData . showString "Duel.Process.Intermediary.initialise:\tconfiguration-files must define the same " . showString Notation.MoveNotation.tag . Text.ShowList.showsAssociation $ shows (moveNotationFirst, moveNotationSecond) "."

			bracketProcess verbosity inputConfigFilePaths $ \[handles, handles'] -> uncurry (
				uncurry (startGame verbosity) $ (Data.Options.getNDecimalDigits &&& Data.Options.getReadTimeout) options
			 ) (
				($ handles) &&& ($ handles') $ Process.Handles.getHandlePair
			 ) Data.Map.Strict.empty (
				Data.Options.getNGames options
			 ) >>= IO.Logger.printInfo . show . Data.Map.Strict.toList
	) $ \e -> do
		IO.Logger.printError . showString "caught " $ show (e :: Control.Exception.SomeException)

		Control.Monad.when (verbosity == maxBound) $ IO.Logger.printInfo "Exiting."

		System.Exit.exitFailure
