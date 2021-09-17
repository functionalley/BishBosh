{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
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

import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Component.Piece		as Component.Piece
import qualified	BishBosh.Component.QualifiedMove	as Component.QualifiedMove
import qualified	BishBosh.Component.Turn			as Component.Turn
import qualified	BishBosh.Component.Zobrist		as Component.Zobrist
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	BishBosh.State.MaybePieceByCoordinates	as State.MaybePieceByCoordinates
import qualified	BishBosh.Type.Length			as Type.Length
import qualified	Control.DeepSeq
import qualified	Data.Array.IArray
import qualified	Data.Maybe

-- | Defines the file on which an En-passant option currently exists.
newtype EnPassantAbscissa x	= MkEnPassantAbscissa {
	getAbscissa	:: x	-- ^ The file on which an En-passant option currently exists.
} deriving (Eq, Ord)

instance Data.Array.IArray.Ix x => Component.Zobrist.Hashable1D EnPassantAbscissa x {-CAVEAT: FlexibleInstances, MultiParamTypeClasses-} where
	listRandoms1D MkEnPassantAbscissa { getAbscissa = x }	= return {-to List-monad-} . Component.Zobrist.dereferenceRandomByEnPassantAbscissa x

instance Control.DeepSeq.NFData x => Control.DeepSeq.NFData (EnPassantAbscissa x) where
	rnf MkEnPassantAbscissa { getAbscissa = x }	= Control.DeepSeq.rnf x

-- | Constructor.
mkMaybeEnPassantAbscissa :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 )
	=> Attribute.LogicalColour.LogicalColour	-- ^ The player who moves next, & who may have an En-passant capture-option.
	-> State.MaybePieceByCoordinates.MaybePieceByCoordinates x y
	-> Component.Turn.Turn x y			-- ^ The last /turn/ taken.
	-> Maybe (EnPassantAbscissa x)
{-# SPECIALISE mkMaybeEnPassantAbscissa :: Attribute.LogicalColour.LogicalColour -> State.MaybePieceByCoordinates.MaybePieceByCoordinates Type.Length.X Type.Length.Y -> Component.Turn.Turn Type.Length.X Type.Length.Y -> Maybe (EnPassantAbscissa Type.Length.X) #-}
mkMaybeEnPassantAbscissa nextLogicalColour maybePieceByCoordinates lastTurn
	| Component.Turn.isPawnDoubleAdvance (Property.Opposable.getOpposite nextLogicalColour) lastTurn
	, let lastMoveDestination	= Component.Move.getDestination . Component.QualifiedMove.getMove $ Component.Turn.getQualifiedMove lastTurn
	, not $ null [
		passedPawn |
			adjacentCoordinates	<- Cartesian.Coordinates.getAdjacents lastMoveDestination,
			Component.Piece.mkKing nextLogicalColour {- Will I expose my King ? -} `notElem` [
				blockingPiece |
					threatDirection		<- Property.FixedMembership.members,	-- Consider all directions.
					(_, attackerRank)	<- Data.Maybe.maybeToList $ State.MaybePieceByCoordinates.findAttackerInDirection nextLogicalColour threatDirection adjacentCoordinates maybePieceByCoordinates,	-- Find discovered attacks.
					attackerRank `notElem` Attribute.Rank.fixedAttackRange,	-- Any viable attack through the vacated square must be long-range.
					(_, blockingPiece)	<- Data.Maybe.maybeToList $ State.MaybePieceByCoordinates.findBlockingPiece (Property.Opposable.getOpposite threatDirection) adjacentCoordinates maybePieceByCoordinates	-- Find any discovered attack.
			], -- Confirm that the En-passant capture doesn't expose my King.
			passedPawn		<- filter (== Component.Piece.mkPawn nextLogicalColour) . Data.Maybe.maybeToList $ State.MaybePieceByCoordinates.dereference adjacentCoordinates maybePieceByCoordinates
	] = Just . MkEnPassantAbscissa $ Cartesian.Coordinates.getX lastMoveDestination
	| otherwise	= Nothing

