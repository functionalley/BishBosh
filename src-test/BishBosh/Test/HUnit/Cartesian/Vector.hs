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

module BishBosh.Test.HUnit.Cartesian.Vector(
-- * Constants
	testCases
-- * Functions
--	measureLength
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Cartesian.Vector		as Cartesian.Vector
import qualified	BishBosh.Types				as T
import qualified	Data.Maybe
import qualified	Test.HUnit
import			Test.HUnit((~?))

-- | Sum the absolute value of /x/ & /y/ distances.
measureLength :: Cartesian.Vector.VectorInt -> T.Distance
measureLength	= uncurry (+) . (abs . Cartesian.Vector.getXDistance &&& abs . Cartesian.Vector.getYDistance)

-- | Check the sanity of the implementation, by validating a list of static test-cases.
testCases :: Test.HUnit.Test
testCases	= Test.HUnit.test [
	all (
		Data.Maybe.isJust . Cartesian.Vector.toMaybeDirection
	) (
		Cartesian.Vector.attackVectorsForKing	:: [Cartesian.Vector.VectorInt]
	) ~? "'BishBosh.Cartesian.Vector.attackVectorsForKing' failed.",
	all (
		Data.Maybe.isNothing . Cartesian.Vector.toMaybeDirection
	) (
		Cartesian.Vector.attackVectorsForKnight	:: [Cartesian.Vector.VectorInt]
	) ~? "'BishBosh.Cartesian.Vector.attackVectorsForKnight' failed.",
	all (
		uncurry (&&) . (
			all ((== 2) . measureLength) &&& (== 2) . length
		) . Cartesian.Vector.attackVectorsForPawn
	) Attribute.LogicalColour.range ~? "'BishBosh.Cartesian.Vector.attackVectorsForPawn' failed.",
	uncurry (&&) (
		(
			all ((== 3) . measureLength) &&& (== 8) . length
		) Cartesian.Vector.attackVectorsForKnight
	) ~? "'BishBosh.Cartesian.Vector.attackVectorsForKnight'.",
	all (
		(`elem` [1, 2]) . measureLength
	) Cartesian.Vector.attackVectorsForKing ~? "'BishBosh.Cartesian.Vector.attackVectorsForKing' should all be of length 1 or 2."
 ]

