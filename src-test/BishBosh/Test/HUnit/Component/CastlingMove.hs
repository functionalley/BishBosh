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

module BishBosh.Test.HUnit.Component.CastlingMove(
-- * Constants
--	kingsMoveLength,
	testCases
) where

import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Vector		as Cartesian.Vector
import qualified	BishBosh.Component.CastlingMove		as Component.CastlingMove
import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Test.HUnit.Component.Move	as Test.HUnit.Component.Move
import qualified	BishBosh.Type.Length			as Type.Length
import qualified	Data.List
import qualified	Test.HUnit
import			Test.HUnit((~?))

-- | Give the constant a concrete type.
kingsMoveLength :: Type.Length.X
kingsMoveLength	= Component.CastlingMove.kingsMoveLength

-- | Check the sanity of the implementation, by validating a list of static test-cases.
testCases :: Test.HUnit.Test
testCases	= Test.HUnit.test [
	and [
		(== 1) . length . Data.List.nub $ map (
			map (
				\castlingMove	-> getAbscissa (getMove castlingMove :: Test.HUnit.Component.Move.Move)
			) . Component.CastlingMove.getCastlingMoves
		) Property.FixedMembership.members |
			getMove		<- [Component.CastlingMove.getKingsMove, Component.CastlingMove.getRooksMove],
			getTerminus	<- [Component.Move.getSource, Component.Move.getDestination],
			let getAbscissa	= Cartesian.Coordinates.getX . getTerminus
	] ~? "'BishBosh.Component.CastlingMove':\tdistance spanned by castling-moves should be independent of logical colour.",
	all (
		\logicalColour	-> (== 1) . length $ Data.List.nub [
			Cartesian.Coordinates.getY $ getTerminus (getMove castlingMove :: Test.HUnit.Component.Move.Move) |
				castlingMove	<- Component.CastlingMove.getCastlingMoves logicalColour,
				getMove		<- [Component.CastlingMove.getKingsMove, Component.CastlingMove.getRooksMove],
				getTerminus	<- [Component.Move.getSource, Component.Move.getDestination]
		]
	) Property.FixedMembership.members ~? "'BishBosh.Component.CastlingMove':\tordinates of castling-moves for one logical colour should be equal.",
	and [
		abs (
			Cartesian.Vector.getXDistance $ Component.Move.measureDistance (
				Component.CastlingMove.getKingsMove castlingMove	:: Test.HUnit.Component.Move.Move
			)
		) == kingsMoveLength |
			logicalColour	<- Property.FixedMembership.members,
			let (longCastlingMove, shortCastlingMove)	= Component.CastlingMove.getLongAndShortMoves logicalColour,
			castlingMove	<- [longCastlingMove, shortCastlingMove]
	] ~? showString "'BishBosh.Component.CastlingMove':\tthe King should move " (
		shows kingsMoveLength " spaces (left or right) when castling."
	)
 ]

