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
	initialise
) where

import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Data.Exception			as Data.Exception
import qualified	BishBosh.Input.CommandLineOption	as Input.CommandLineOption
import qualified	BishBosh.Input.Verbosity		as Input.Verbosity
import qualified	BishBosh.Model.Game			as Model.Game
import qualified	BishBosh.Model.GameTerminationReason	as Model.GameTerminationReason
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	BishBosh.Types				as T
import qualified	BishBosh.UI.Command			as UI.Command
import qualified	Control.Exception
import qualified	Control.Monad
import qualified	Data.Default
import qualified	Data.Map.Strict
import qualified	Data.Maybe
import qualified	Duel.Data.Options			as Data.Options
import qualified	Duel.IO.Logger				as IO.Logger
import qualified	System.Exit
import qualified	System.FilePath
import qualified	System.IO
import qualified	System.Process

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
	-> IO (System.IO.Handle, System.IO.Handle, System.IO.Handle, System.Process.ProcessHandle)
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

	System.Process.runInteractiveProcess command args Nothing Nothing

-- | Read either a move or a game-termination reason from the specified handle.
readMove
	:: Input.Verbosity.Verbosity
	-> Data.Options.ReadTimeout
	-> Attribute.LogicalColour.LogicalColour	-- ^ Whose turn it is.
	-> System.IO.Handle				-- ^ Output handle from which data should be read.
	-> IO (Either Model.GameTerminationReason.GameTerminationReason (MoveNotation T.X T.Y))
readMove verbosity readTimeout logicalColour stdOut = do
	Control.Monad.when (verbosity == maxBound) . IO.Logger.printInfo . showString "Waiting for " $ shows logicalColour "."

	inputReady	<- System.IO.hWaitForInput stdOut $ 1000 * readTimeout

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
	-> Data.Options.ReadTimeout
	-> Attribute.LogicalColour.LogicalColour	-- ^ Whose turn it is.
	-> System.IO.Handle				-- ^ Output handle from which move should be read.
	-> System.IO.Handle				-- ^ Input handle to which move should be forwarded.
	-> IO (Maybe Model.GameTerminationReason.GameTerminationReason)
copyMove verbosity readTimeout logicalColour stdOut stdIn = do
	readMove verbosity readTimeout logicalColour stdOut >>= (
		return {-to IO-monad-} . Just
	 ) `either` (
		\move	-> do
			System.IO.hPrint stdIn move

			return {-to IO-monad-} Nothing
	 )

-- | Contains stdin & stdout handles respectively.
type IOHandles	= (System.IO.Handle, System.IO.Handle)

-- | Shuttle moves between the two child processes until the game terminates.
play
	:: Input.Verbosity.Verbosity
	-> Data.Options.ReadTimeout
	-> IOHandles
	-> IOHandles
	-> IO Model.GameTerminationReason.GameTerminationReason
play verbosity readTimeout	= slave maxBound	where
	slave logicalColour producer@(_, stdOut) consumer@(stdIn', stdOut')	= copyMove verbosity readTimeout logicalColour stdOut stdIn' >>= Data.Maybe.maybe (
		do
			Control.Monad.when (verbosity == maxBound) $ IO.Logger.printInfo "Swapping player-roles."

			slave (Property.Opposable.getOpposite logicalColour) consumer producer	-- Recurse.
	 ) (
		\gameTerminationReason -> do
			readMove verbosity readTimeout logicalColour stdOut' >>= (
				\gameTerminationReason' -> do
					Control.Monad.unless (gameTerminationReason == gameTerminationReason') . Control.Exception.throwIO . Data.Exception.mkIncompatibleData . showString "Duel.Process.Intermediary.play:\tsecond game terminated for a different reason; " $ shows gameTerminationReason' "."

					return {-to IO-monad-} gameTerminationReason
			 ) `either` (
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

	* N.B.: the recorded result is merely a string, though it could be read into a 'BishBosh.Model.GameTerminationReason'.
-}
type GameTerminationReasonsMap	= Data.Map.Strict.Map Model.GameTerminationReason.GameTerminationReason Int

{- |
	* Start two independently configured (though of matching move-notation) instances of 'bishbosh'.

	* One instance automates White & one automates Black.

	* Shuttle moves between the instances.

	* Plays repeatedly & accumulating the final results of each game.
-}
startGame
	:: Input.Verbosity.Verbosity
	-> Data.Options.ReadTimeout
	-> IOHandles	-- ^ White's handles.
	-> IOHandles	-- ^ Black's handles.
	-> GameTerminationReasonsMap
	-> Model.Game.NGames
	-> IO GameTerminationReasonsMap
startGame verbosity readTimeout producer consumer	= slave where
	slave gameTerminationReasonsMap 0	= return {-to IO-monad-} gameTerminationReasonsMap
	slave gameTerminationReasonsMap nGames	= do
		Control.Monad.when (verbosity == maxBound) $ IO.Logger.printInfo "Starting game."

		gameTerminationReason	<- play verbosity readTimeout producer consumer

		sequence_ $ [
			\(_, stdOut) -> do
				Control.Monad.when (verbosity == maxBound) $ IO.Logger.printInfo "Purging child's stdout."

				purge stdOut,
			\(stdIn, _) -> do
				Control.Monad.when (verbosity == maxBound) $ IO.Logger.printInfo "Requesting restart."

				System.IO.hPutStrLn stdIn $ UI.Command.commandPrefix : UI.Command.restartTag
		 ] <*> [consumer, producer]

		slave (
			Data.Map.Strict.insertWith (const succ) gameTerminationReason 1 gameTerminationReasonsMap
		 ) $ pred nGames -- Recurse.

-- | Start 'bishbosh', print any errors, unbuffer its I/O, & return the process-handles.
startProcess
	:: Input.Verbosity.Verbosity
	-> System.FilePath.FilePath	-- ^ The path to a configuration-file.
	-> IO (System.IO.Handle, System.IO.Handle, System.IO.Handle, System.Process.ProcessHandle)
startProcess verbosity configFilePath	= do
	handles@(stdIn, stdOut, stdErr, _)	<- runBishBosh verbosity configFilePath

	IO.Logger.dump stdErr

	sequence_ $ [
		(`System.IO.hSetBuffering` System.IO.LineBuffering),
		(`System.IO.hSetEncoding` System.IO.latin1)
	 ] <*> [stdIn, stdOut]

	Control.Monad.when (verbosity == maxBound) $ mapM System.IO.hShow [stdIn, stdOut, stdErr] >>= IO.Logger.printInfo . show

	return {-to IO-monad-} handles

-- | Play the requested number of games & display the accumulated results.
initialise :: Data.Options.Options -> IO ()
initialise options	= let
	verbosity	= Data.Options.getVerbosity options
 in Control.Exception.catch (
	Control.Exception.bracket (
		startProcess verbosity `mapM` Data.Options.getInputConfigFilePaths options
	) (
		\processes@[(_, _, stdErr, _), (_, _, stdErr', _)] -> do
			mapM_ IO.Logger.dump [stdErr, stdErr']

			Control.Monad.when (verbosity == maxBound) $ IO.Logger.printInfo "Cleaning-up child processes."

			mapM_ (
				\(hIn, hOut, hErr, pId) -> System.Process.cleanupProcess (Just hIn, Just hOut, Just hErr, pId)
			 ) processes
	) (
		\[(stdIn, stdOut, _, _), (stdIn', stdOut', _, _)] -> startGame verbosity (
			Data.Options.getReadTimeout options
		) (
			stdIn, stdOut
		) (
			stdIn', stdOut'
		) Data.Map.Strict.empty (
			Data.Options.getNGames options
		) >>= IO.Logger.printInfo . show . Data.Map.Strict.toList
	)
 ) $ \e -> do
	IO.Logger.printError . showString "caught " $ show (e :: Control.Exception.SomeException)

	Control.Monad.when (verbosity == maxBound) $ IO.Logger.printInfo "Exiting."

	System.Exit.exitFailure