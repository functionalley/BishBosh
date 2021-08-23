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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties.
-}

module BishBosh.Test.QuickCheck.Attribute.Rank(
-- * Constants
	results
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	Data.List
import qualified	System.Random
import qualified	Test.QuickCheck
import qualified	ToolShed.System.Random
import qualified	ToolShed.Test.ReversibleIO

instance Test.QuickCheck.Arbitrary Attribute.Rank.Rank where
	arbitrary	= Test.QuickCheck.elements Property.FixedMembership.members

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Attribute.Rank.Rank -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Rank.prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: String -> Test.QuickCheck.Property
		f garbage	= Test.QuickCheck.label "Rank.prop_read" $ case (reads garbage :: [(Attribute.Rank.Rank, String)]) of
			[_]	-> True
			_	-> True	-- Unless the read-implementation throws an exception.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Attribute.Rank.Rank -> String -> Test.QuickCheck.Property
		f rank	= Test.QuickCheck.label "Rank.prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (const False) rank
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Int -> [Attribute.Rank.Rank] -> Test.QuickCheck.Property
		f seed	= Test.QuickCheck.label "Rank.prop_findUndefinedRanks" . uncurry (==) . (
			findOrderedUndefined &&& findOrderedUndefined . ToolShed.System.Random.shuffle (System.Random.mkStdGen seed)
		 ) where
			findOrderedUndefined	= Data.List.sort . Attribute.Rank.findUndefinedRanks
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 16 } f
 ]

