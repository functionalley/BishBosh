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
	along with BishBosh.  If not, see <https://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]

	* Static tests.
-}

module BishBosh.Test.HUnit.Model.PositionHashTree(
-- * Constants
	testCases
) where

import qualified	BishBosh.Model.PositionHashTree	as Model.PositionHashTree
import qualified	BishBosh.Type.Crypto		as Type.Crypto
import qualified	Data.Default
import qualified	Test.HUnit
import			Test.HUnit((~:), (~?=))

-- | Check the implementation, by validating a list of static test-cases.
testCases :: Test.HUnit.Test
testCases	= Test.HUnit.test [
	let
		maxDepth	= 4	-- CAVEAT: 5 requires to much RAM.
	in "'BishBosh.Model.PositionhashTree.countDistinctPositions' failed" ~: map (
		\i -> Model.PositionHashTree.countDistinctPositions i (Data.Default.def :: Model.PositionHashTree.PositionHashTree Type.Crypto.PositionHash)
	) [0 .. maxDepth] ~?= take (succ maxDepth) [1, 20, 400, 5362, 72078, 822518, 9417681, 96400068, 988187354, 9183421888, 85375278064, 726155461002]	-- <https://oeis.org/A083276>. CAVEAT: the point at which this test fails, depends on the square-root of the bits in 'Crypto.PositionHash'.
 ]

