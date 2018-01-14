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

module BishBosh.Test.QuickCheck.Notation.ICCFNumeric(
-- * Types
-- ** Type-synonyms
--	ICCFNumeric,
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Component.Move()
import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.Rank		as Attribute.Rank
import qualified	BishBosh.Cartesian.Coordinates	as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate	as Cartesian.Ordinate
import qualified	BishBosh.Cartesian.Vector	as Cartesian.Vector
import qualified	BishBosh.Component.Move		as Component.Move
import qualified	BishBosh.Notation.ICCFNumeric	as Notation.ICCFNumeric
import qualified	BishBosh.Types			as T
import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO

-- | Defines a concrete type for testing.
type ICCFNumeric	= Notation.ICCFNumeric.ICCFNumeric T.X T.Y

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => Test.QuickCheck.Arbitrary (Notation.ICCFNumeric.ICCFNumeric x y) where
	{-# SPECIALISE instance Test.QuickCheck.Arbitrary ICCFNumeric #-}
	arbitrary	= do
		move	<- Test.QuickCheck.arbitrary

		Notation.ICCFNumeric.mkICCFNumeric move `fmap` if abs (
			Cartesian.Vector.getXDistance (Component.Move.measureDistance move :: Cartesian.Vector.VectorInt)
		 ) <= 1 && (
			Cartesian.Coordinates.getY . Component.Move.getSource &&& Cartesian.Coordinates.getY . Component.Move.getDestination $ move
		 ) `elem` [
			(succ Cartesian.Ordinate.yMin, Cartesian.Ordinate.yMin),	-- Black moving down.
			(pred Cartesian.Ordinate.yMax, Cartesian.Ordinate.yMax)		-- White moving up.
		 ]
			then Test.QuickCheck.oneof [
				return {-to Gen-monad-} Nothing,
				Test.QuickCheck.elements $ map Just Attribute.Rank.promotionProspects	-- CAVEAT: move & promotionRank are still unlikely to be compatible.
			]
			else return {-to Gen-monad-} Nothing

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: ICCFNumeric -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "ICCFNumeric.prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: String -> Test.QuickCheck.Property
		f garbage	= Test.QuickCheck.label "ICCFNumeric.prop_read" $ case (
			reads garbage :: [(Notation.ICCFNumeric.ICCFNumeric Int Int, String)]
		 ) of
			[_]	-> True
			_	-> True	-- Unless the read-implementation throws an exception.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: ICCFNumeric -> String -> Test.QuickCheck.Property
		f coordinate	= Test.QuickCheck.label "ICCFNumeric.prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (
			`elem` concatMap (show . fst {-digit-}) Notation.ICCFNumeric.toRank
		 ) coordinate
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 2048 } f
 ]
