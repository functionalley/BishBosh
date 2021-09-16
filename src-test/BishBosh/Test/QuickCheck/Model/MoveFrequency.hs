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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties.
-}

module BishBosh.Test.QuickCheck.Model.MoveFrequency(
-- * Constants
	results
) where

import qualified	BishBosh.Model.GameTree			as Model.GameTree
import qualified	BishBosh.Model.MoveFrequency		as Model.MoveFrequency
import qualified	BishBosh.Test.QuickCheck.Model.GameTree	as Test.QuickCheck.Model.GameTree
import qualified	Data.Foldable
import qualified	Test.QuickCheck

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Test.QuickCheck.Model.GameTree.GameTree -> Test.QuickCheck.Property
		f gameTree	= Test.QuickCheck.label "MoveFrequency.prop_countMoves" $ Model.MoveFrequency.countEntries (
			Model.GameTree.toMoveFrequency gameTree
		 ) == pred {-the apex is counted in 'getNPlies'-} (
			fromIntegral . Data.Foldable.length $ Model.GameTree.deconstruct gameTree
		 )
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 16 } f
 ]
