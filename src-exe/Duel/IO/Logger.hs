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

 [@DESCRIPTION@]	* Facilitates & formats logging to stderr.
-}

module Duel.IO.Logger(
-- * Functions
--	putStrLn,
	printInfo,
	printWarning,
	printError,
	dump
) where

import qualified	BishBosh.Text.ShowPrefix	as Text.ShowPrefix
import qualified	Control.Monad
import qualified	System.IO

-- | Define the output-stream.
putString :: String -> IO ()
putString	= System.IO.hPutStrLn System.IO.stderr

-- | Log an informational message.
printInfo :: String -> IO ()
printInfo	= putString . Text.ShowPrefix.showsPrefixInfo

-- | Log a warning.
printWarning :: String -> IO ()
printWarning	= putString . Text.ShowPrefix.showsPrefixWarning

-- | Log a warning.
printError :: String -> IO ()
printError	= putString . Text.ShowPrefix.showsPrefixError

-- | Dump any lines written to the specified handle, without blocking.
dump :: System.IO.Handle -> IO ()
dump handle	= slave where
	slave	= do
		isReady	<- System.IO.hReady handle

		Control.Monad.when isReady $ do
			System.IO.hGetLine handle >>= putString

			slave	-- Recurse.
