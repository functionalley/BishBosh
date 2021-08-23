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

 [@DESCRIPTION@]	Defines /Foldable/-properties.
-}

module BishBosh.Test.QuickCheck.Data.Foldable (
-- * Constants
	results
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Data.Foldable	as Data.Foldable
import qualified	Data.Int
import qualified	Data.List
import qualified	Test.QuickCheck
import qualified	ToolShed.System.Random
import qualified	System.Random

-- | The constant test-results.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Int -> [Data.Int.Int8] -> Test.QuickCheck.Property
		f seed	= Test.QuickCheck.label "Foldable.prop_findDuplicates" . uncurry (==) . (
			findOrderedDuplicates &&& findOrderedDuplicates . ToolShed.System.Random.shuffle (System.Random.mkStdGen seed)
		 ) where
			findOrderedDuplicates	= Data.List.sort . Data.Foldable.findDuplicates
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f
 ]

