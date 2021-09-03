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

 [@DESCRIPTION@]	Wraps "System.Process".
-}

module Duel.Process.Handles(
-- * Types
-- ** Data-types
	Handles(
--		MkHandles,
--		getStdIn,
--		getStdOut,
		getStdErr
--		getPId
	),
-- * Functions
	cleanupHandles,
	showHandles,
-- ** Accessors
	getHandlePair,
-- ** Constructor
	mkHandles
) where

import qualified	System.IO
import qualified	System.Process

-- | A container for the IO-handles & process-id of a child process.
data Handles	= MkHandles {
	getStdIn	:: System.IO.Handle,
	getStdOut	:: System.IO.Handle,
	getStdErr	:: System.IO.Handle,
	getPId		:: System.Process.ProcessHandle
}

-- | Constructor.
mkHandles :: String -> [String] -> IO Handles
mkHandles command args	= do
	(stdIn, stdOut, stdErr, pId)	<- System.Process.runInteractiveProcess command args Nothing Nothing

	sequence_ $ [
		(`System.IO.hSetBuffering` System.IO.LineBuffering),
		(`System.IO.hSetEncoding` System.IO.latin1)
	 ] <*> [stdIn, stdOut]

	return {- to IO-monad -} MkHandles {
		getStdIn	= stdIn,
		getStdOut	= stdOut,
		getStdErr	= stdErr,
		getPId		= pId
	}

-- | Accessor.
getHandlePair :: Handles -> (System.IO.Handle, System.IO.Handle)
getHandlePair MkHandles {
	getStdIn	= stdIn,
	getStdOut	= stdOut
} = (stdIn, stdOut)

-- | Show the configuration of the IO-handles.
showHandles :: Handles -> IO [String]
showHandles MkHandles {
	getStdIn	= stdIn,
	getStdOut	= stdOut,
	getStdErr	= stdErr
} = mapM System.IO.hShow [stdIn, stdOut, stdErr]

-- | Terminates the child-process & closes any IO-handles.
cleanupHandles :: Handles -> IO ()
cleanupHandles MkHandles {
	getStdIn	= stdIn,
	getStdOut	= stdOut,
	getStdErr	= stdErr,
	getPId		= pId
} = System.Process.cleanupProcess (Just stdIn, Just stdOut, Just stdErr, pId)

