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

 [@DESCRIPTION@]	Defines the file on which an En-passant option currently exists.
-}

module BishBosh.State.EnPassantAbscissa (
-- * Types
-- ** Data-types
	EnPassantAbscissa(getAbscissa),
-- * Functions
-- ** Constructor
	mkMaybeEnPassantAbscissa
) where

import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Colour.LogicalColour		as Colour.LogicalColour
import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Component.Piece		as Component.Piece
import qualified	BishBosh.Component.QualifiedMove	as Component.QualifiedMove
import qualified	BishBosh.Component.Turn			as Component.Turn
import qualified	BishBosh.Component.Zobrist		as Component.Zobrist
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	BishBosh.State.MaybePieceByCoordinates	as State.MaybePieceByCoordinates
import qualified	BishBosh.StateProperty.Hashable		as StateProperty.Hashable
import qualified	BishBosh.Type.Length			as Type.Length
import qualified	Control.DeepSeq
import qualified	Data.Maybe

-- | Defines the file on which an En-passant option currently exists.
newtype EnPassantAbscissa	= MkEnPassantAbscissa {
	getAbscissa	:: Type.Length.X	-- ^ The file on which an En-passant option currently exists.
} deriving (Eq, Ord)

instance Control.DeepSeq.NFData EnPassantAbscissa where
	rnf MkEnPassantAbscissa { getAbscissa = x }	= Control.DeepSeq.rnf x

instance StateProperty.Hashable.Hashable EnPassantAbscissa where
	listRandoms zobrist MkEnPassantAbscissa { getAbscissa = x }	= return {-to List-monad-} $! Component.Zobrist.dereferenceRandomByEnPassantAbscissa zobrist x

-- | Constructor.
mkMaybeEnPassantAbscissa
	:: Colour.LogicalColour.LogicalColour	-- ^ The player who moves next, & who may have an En-passant capture-option.
	-> State.MaybePieceByCoordinates.MaybePieceByCoordinates
	-> Component.Turn.Turn			-- ^ The last /turn/ taken.
	-> Maybe EnPassantAbscissa
mkMaybeEnPassantAbscissa nextLogicalColour maybePieceByCoordinates lastTurn
	| Component.Turn.isPawnDoubleAdvance lastTurn $! Property.Opposable.getOpposite nextLogicalColour
	, let lastMoveDestination	= Component.Move.getDestination . Component.QualifiedMove.getMove $! Component.Turn.getQualifiedMove lastTurn
	, not $ null [
		adjacentPawnCoordinates |
			adjacentPawnCoordinates	<- filter (
				(== Just (Component.Piece.mkPawn nextLogicalColour)) . State.MaybePieceByCoordinates.dereference maybePieceByCoordinates	-- Confirm the existence of an enemy Pawn.
			) $ Cartesian.Coordinates.getAdjacents lastMoveDestination,	-- Find locations from which a Pawn may mount an attack en-passant.
			all (
				\(direction, rank) -> rank `elem` Attribute.Rank.plodders || (
					/= Just (Component.Piece.mkKing nextLogicalColour) -- Confirm that my King doesn't become checked.
				) (
					fmap snd {-piece-} . State.MaybePieceByCoordinates.findBlockingPiece maybePieceByCoordinates adjacentPawnCoordinates $ Property.Opposable.getOpposite direction	-- Check whether there's a piece in the opposite direction through the vacated square.
				)
			) $ Data.Maybe.mapMaybe (
				\direction -> (,) direction . snd {-rank-} <$> State.MaybePieceByCoordinates.findAttackerInDirection maybePieceByCoordinates nextLogicalColour adjacentPawnCoordinates direction	-- Identify attacks passing through the square vacated by the attacking Pawn; the square occupied by the double advanced Pawn, wasn't an issue before it advanced, so it isn't if taken.
			) Property.FixedMembership.members	-- Consider all directions.
	] {-list-comprehension-}	= Just . MkEnPassantAbscissa $ Cartesian.Coordinates.getX lastMoveDestination
	| otherwise			= Nothing

