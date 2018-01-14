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

 [@DESCRIPTION@]	Provides signal-handlers.
-}
module BishBosh.Concurrent.SignalHandlers (
-- * Functions
	handleSignals
) where

import qualified	BishBosh.Text.Show	as Text.Show
import qualified	Control.Concurrent
import qualified	System.IO
import qualified	System.Posix.Signals	-- CAVEAT: the package 'unix' to which this module belongs, requires a POSIX environment (such as Cygwin) for compilation on Windows.

-- | Provide basic handlers for common signals.
handleSignals :: IO ()
handleSignals	= do
	threadId	<- Control.Concurrent.myThreadId

	mapM_ (
		\(signal, s)	-> System.Posix.Signals.installHandler signal (
			System.Posix.Signals.Catch $ do
				System.IO.hPutStrLn System.IO.stderr $ Text.Show.showsWarningPrefix "SIG" ++ s ++ " caught; exiting."

				Control.Concurrent.killThread threadId
		 ) Nothing
	 ) [
		(
			System.Posix.Signals.sigINT,			"INT"	-- '^C'.
		), (
			System.Posix.Signals.softwareTermination,	"TERM"	-- Default signal for 'kill'.
		), (
			System.Posix.Signals.sigQUIT,			"QUIT"	-- '^\'.
		)
	 ]

