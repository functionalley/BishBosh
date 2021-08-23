{-# OPTIONS_GHC -fno-warn-orphans #-}
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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary'.
-}

module BishBosh.Test.QuickCheck.Input.PGNOptions() where

import qualified	BishBosh.Input.PGNOptions	as Input.PGNOptions
import			BishBosh.Test.QuickCheck.Text.Encoding()
import qualified	System.FilePath
import qualified	Test.QuickCheck
import			System.FilePath((</>), (<.>))

instance Test.QuickCheck.Arbitrary Input.PGNOptions.PGNOptions where
	arbitrary	= Input.PGNOptions.mkPGNOptions (
		showChar System.FilePath.pathSeparator $ "tmp" </> "database" <.> "pgn"
	 ) <$> (
		Test.QuickCheck.elements . (Nothing :) . map Just $ words "bzcat lzcat xzcat zcat"	-- Decompressor.
	 ) <*> Test.QuickCheck.arbitrary {-isStrictlySequential-} <*> Test.QuickCheck.arbitrary {-validateMoves-} <*> Test.QuickCheck.arbitrary {-text-encoding-} <*> Test.QuickCheck.elements [
		["FICSGamesDBGameNo"],
		["ECO"],
		["FICSGamesDBGameNo", "ECO"]
	 ] <*> fmap (`mod` 128) Test.QuickCheck.arbitrary {-minimumPlies-} <*> Test.QuickCheck.elements (Nothing : map Just [1 .. 10]) {-maybeMaximumGames-}

