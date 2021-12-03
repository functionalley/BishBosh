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

 [@DESCRIPTION@]	Static tests.
-}

module BishBosh.Test.HUnit.Cartesian.Coordinates(
-- * Constants
	testCases
) where

import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Colour.LogicalColourOfSquare	as Colour.LogicalColourOfSquare
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	Data.Array.IArray
import qualified	Data.List
import qualified	Test.HUnit
import			Test.HUnit((~?), (~?=), (~:))

-- | Check the sanity of the implementation, by validating a list of static test-cases.
testCases :: Test.HUnit.Test
testCases	= Test.HUnit.test [
	all (Colour.LogicalColourOfSquare.isBlack . Cartesian.Coordinates.getLogicalColourOfSquare) (
		Cartesian.Coordinates.extrapolate (minBound :: Cartesian.Coordinates.Coordinates) maxBound {-direction-}
	) ~? "'BishBosh.Cartesian.Coordinates.getLogicalColourOfSquare' failed to find black squares on the main diagonal.",
	"'BishBosh.Cartesian.Coordinates.getLogicalColourOfSquare' failed to count equal numbers of black & white squares." ~: (
		\(black, white) -> length black ~?= length white
	) . Data.List.partition Colour.LogicalColourOfSquare.isBlack $ map Cartesian.Coordinates.getLogicalColourOfSquare (
		Property.FixedMembership.members	:: [Cartesian.Coordinates.Coordinates]
	),
	"'BishBosh.Cartesian.Coordinates' failed to visit all squares." ~: length (
		Property.FixedMembership.members	:: [Cartesian.Coordinates.Coordinates]
	) ~?= fromIntegral Cartesian.Coordinates.nSquares,
	"instance 'Data.Array.IArray.Ix Coordinates' is incompatible with instance 'Ord Coordinates'." ~: Data.Array.IArray.indices (
		Cartesian.Coordinates.listArrayByCoordinates [0 .. ]	:: Cartesian.Coordinates.ArrayByCoordinates Int
	) ~?= Property.FixedMembership.members
 ]

