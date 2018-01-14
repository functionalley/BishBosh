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

module BishBosh.Test.QuickCheck.Notation.Coordinate(
-- * Types
-- ** Type-synonyms
--	Coordinate,
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
import qualified	BishBosh.Notation.Coordinate	as Notation.Coordinate
import qualified	BishBosh.Types			as T
import qualified	Data.Char
import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO

-- | Defines a concrete type for testing.
type Coordinate	= Notation.Coordinate.Coordinate T.X T.Y

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => Test.QuickCheck.Arbitrary (Notation.Coordinate.Coordinate x y) where
	{-# SPECIALISE instance Test.QuickCheck.Arbitrary Coordinate #-}
	arbitrary	= do
		move	<- Test.QuickCheck.arbitrary

		Notation.Coordinate.mkCoordinate move `fmap` if abs (
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
		f :: Coordinate -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Coordinate.prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: String -> Test.QuickCheck.Property
		f garbage	= Test.QuickCheck.label "Coordinate.prop_read" $ case (
			reads garbage :: [(Notation.Coordinate.Coordinate Int Int, String)]
		 ) of
			[_]	-> True
			_	-> True	-- Unless the read-implementation throws an exception.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Coordinate -> String -> Test.QuickCheck.Property
		f coordinate	= Test.QuickCheck.label "Coordinate.prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (
			(
				`elem` concatMap show Attribute.Rank.promotionProspects
			) . Data.Char.toLower
		 ) coordinate
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 2048 } f
 ]
