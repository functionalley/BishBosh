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

module BishBosh.Test.HUnit.Component.Move(
-- * Constants
	testCases
) where

import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Component.QualifiedMove	as Component.QualifiedMove
import qualified	BishBosh.Direction.Direction		as Direction.Direction
import qualified	BishBosh.Notation.Smith			as Notation.Smith
import qualified	Test.HUnit
import			Test.HUnit((~?))

-- | Check the sanity of the implementation, by validating a list of static test-cases.
testCases :: Test.HUnit.Test
testCases	= Test.HUnit.test [
	all (
		\(direction, s) -> let
			move :: Component.Move.Move
			move	= Component.QualifiedMove.getMove . Notation.Smith.getQualifiedMove $ read s
		in Cartesian.Coordinates.extrapolate direction (Component.Move.getSource move) == Component.Move.interpolate move
	) [
		(
			Direction.Direction.ne,	"a1h8"
		), (
			Direction.Direction.n,	"a1a8"
		), (
			Direction.Direction.e,	"a1h1"
		), (
			Direction.Direction.se,	"a8h1"
		), (
			Direction.Direction.s,	"a8a1"
		), (
			Direction.Direction.e,	"a8h8"
		), (
			Direction.Direction.sw,	"h8a1"
		), (
			Direction.Direction.s,	"h8h1"
		), (
			Direction.Direction.w,	"h8a8"
		), (
			Direction.Direction.nw,	"h1a8"
		), (
			Direction.Direction.n,	"h1h8"
		), (
			Direction.Direction.w,	"h1a1"
		)
	] ~? "'BishBosh.Cartesian.Coordinates.extrapolate' is incompatible with 'BishBosh.Component.Move.interpolate'."
 ]

