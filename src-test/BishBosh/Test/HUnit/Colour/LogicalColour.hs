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

module BishBosh.Test.HUnit.Colour.LogicalColour(
-- * Constants
	testCases
) where

import qualified	BishBosh.Colour.LogicalColour		as Colour.LogicalColour
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	Data.List
import qualified	Test.HUnit
import			Test.HUnit((~?))

-- | Check the sanity of the implementation, by validating a list of static test-cases.
testCases :: Test.HUnit.Test
testCases	= Test.HUnit.test [
	Property.FixedMembership.members == Data.List.sort Colour.LogicalColour.range ~? "BishBosh.Colour.LogicalColour.range is misordered."
 ]

