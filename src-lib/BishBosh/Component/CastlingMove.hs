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

 [@DESCRIPTION@]	Defines all possible castling-moves.
-}

module BishBosh.Component.CastlingMove(
-- * Types
-- ** Data-types
	CastlingMove(
--		MkCastlingMove,
		getMoveType,
		getKingsMove,
		getRooksMove
	),
-- * Constants
	kingsMoveLength,
--	castlingMovesByLogicalColour,
-- * Functions
--	defineCastlingMoves,
	getLongAndShortMoves,
-- ** Accessors
	getCastlingMoves,
--	getCastlingMovesInt
) where

import			Control.Arrow((&&&))
import			Data.Array.IArray((!))
import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Attribute.MoveType		as Attribute.MoveType
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Data.Enum			as Data.Enum
import qualified	BishBosh.Data.Exception			as Data.Exception
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Type.Length			as Type.Length
import qualified	Control.Exception

-- | Defines a castling-move.
data CastlingMove x y	= MkCastlingMove {
	getMoveType	:: Attribute.MoveType.MoveType,	-- ^ CAVEAT: should only be a castling-move type.
	getKingsMove	:: Component.Move.Move x y,
	getRooksMove	:: Component.Move.Move x y
}

-- | The constant number of files over which the King always travels when castling.
kingsMoveLength :: Num x => x
kingsMoveLength	= 2

-- | Define all possible castling-moves for the specified /logical colour/.
defineCastlingMoves :: (
	Enum	x,
	Enum	y,
	Eq	y,
	Ord	x
 ) => Attribute.LogicalColour.LogicalColour -> [CastlingMove x y]
defineCastlingMoves logicalColour	= [
	MkCastlingMove {
		getMoveType	= Attribute.MoveType.longCastle,
		getKingsMove	= kingsMove $ subtract kingsMoveLength,
		getRooksMove	= uncurry Component.Move.mkMove . (id &&& translateX (+ 3)) $ if isBlack
			then Cartesian.Coordinates.topLeft
			else minBound
	}, MkCastlingMove {
		getMoveType	= Attribute.MoveType.shortCastle,
		getKingsMove	= kingsMove (+ kingsMoveLength),
		getRooksMove	= uncurry Component.Move.mkMove . (id &&& translateX (subtract 2)) $ if isBlack
			then maxBound
			else Cartesian.Coordinates.bottomRight
	}
 ] where
	isBlack :: Bool
	isBlack	= Attribute.LogicalColour.isBlack logicalColour

	kingsStartingCoordinates	= Cartesian.Coordinates.kingsStartingCoordinates logicalColour
	kingsMove translation		= Component.Move.mkMove kingsStartingCoordinates $ translateX translation kingsStartingCoordinates

	translateX :: (Enum x, Ord x) => (Int -> Int) -> Cartesian.Coordinates.Coordinates x y -> Cartesian.Coordinates.Coordinates x y
	translateX	= Cartesian.Coordinates.translateX . Data.Enum.translate

-- | Defines by /logical colour/, the constant list of all possible castling-moves.
castlingMovesByLogicalColour :: (
	Enum	x,
	Enum	y,
	Eq	y,
	Ord	x
 ) => Attribute.LogicalColour.ArrayByLogicalColour [CastlingMove x y]
castlingMovesByLogicalColour	= Attribute.LogicalColour.listArrayByLogicalColour $ map defineCastlingMoves Property.FixedMembership.members

{- |
	* Accessor.

	* CAVEAT: the moves are returned in unspecified order.
-}
getCastlingMoves :: (
	Enum	x,
	Enum	y,
	Eq	y,
	Ord	x
 ) => Attribute.LogicalColour.LogicalColour -> [CastlingMove x y]
{-# NOINLINE getCastlingMoves #-}	-- Ensure the rewrite-rule triggers.
{-# RULES "getCastlingMoves/Int" getCastlingMoves = getCastlingMovesInt #-}
getCastlingMoves	= defineCastlingMoves

-- | A specialisation of 'getCastlingMoves'.
getCastlingMovesInt :: Attribute.LogicalColour.LogicalColour -> [CastlingMove Type.Length.X Type.Length.Y]
getCastlingMovesInt	= (castlingMovesByLogicalColour !)

-- | Break-down the two castling-moves for the specified /logical colour/ into a long & a short castling-move.
getLongAndShortMoves :: (
	Enum	x,
	Enum	y,
	Eq	y,
	Ord	x
 ) => Attribute.LogicalColour.LogicalColour -> (CastlingMove x y, CastlingMove x y)
{-# SPECIALISE getLongAndShortMoves :: Attribute.LogicalColour.LogicalColour -> (CastlingMove Type.Length.X Type.Length.Y, CastlingMove Type.Length.X Type.Length.Y) #-}
getLongAndShortMoves logicalColour
	| [longCastlingMove, shortCastlingMove] <- getCastlingMoves logicalColour	= (longCastlingMove, shortCastlingMove)
	| otherwise									= Control.Exception.throw $ Data.Exception.mkIncompatibleData "BishBosh.Component.CastlingMove.getLongAndShortMoves:\tunexpected list-length."

