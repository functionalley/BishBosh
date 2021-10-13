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

 [@DESCRIPTION@]

	* Defines the state of the game, without regard to how it arrived there; <https://www.chessprogramming.org/Chess_Position>.

	* Games with the same /position/ may be considered to have converged, since they now have equal opportunity.

	* N.B.: /piece/s are fungible, i.e. they lack identity, so the location of identical /piece/s may be exchanged, without altering the /position/.
-}

module BishBosh.State.Position(
-- * Types
-- ** Data-types
	Position(
--		MkPosition,
--		getNextLogicalColour,
--		getMaybePieceByCoordinates,
--		getCastleableRooksByLogicalColour,
		getMaybeEnPassantAbscissa
	),
-- * Functions
-- ** Constructors
	mkPosition
) where

import qualified	BishBosh.Attribute.LogicalColour		as Attribute.LogicalColour
import qualified	BishBosh.Component.Turn				as Component.Turn
import qualified	BishBosh.Component.Zobrist			as Component.Zobrist
import qualified	BishBosh.Property.Opposable			as Property.Opposable
import qualified	BishBosh.Property.Reflectable			as Property.Reflectable
import qualified	BishBosh.State.CastleableRooksByLogicalColour	as State.CastleableRooksByLogicalColour
import qualified	BishBosh.State.EnPassantAbscissa		as State.EnPassantAbscissa
import qualified	BishBosh.State.MaybePieceByCoordinates		as State.MaybePieceByCoordinates
import qualified	Control.DeepSeq
import qualified	Data.Maybe

-- | The state of the game, without regard to how it arrived there.
data Position	= MkPosition {
	getNextLogicalColour			:: Attribute.LogicalColour.LogicalColour,	-- ^ The next player to move.
	getMaybePieceByCoordinates		:: State.MaybePieceByCoordinates.MaybePieceByCoordinates,
	getCastleableRooksByLogicalColour	:: State.CastleableRooksByLogicalColour.CastleableRooksByLogicalColour,
	getMaybeEnPassantAbscissa		:: Maybe State.EnPassantAbscissa.EnPassantAbscissa
} deriving Eq

instance Ord Position where
	position@MkPosition {
		getNextLogicalColour		= nextLogicalColour,
		getMaybePieceByCoordinates	= maybePieceByCoordinates
	} `compare` position'@MkPosition {
		getNextLogicalColour		= nextLogicalColour',
		getMaybePieceByCoordinates	= maybePieceByCoordinates'
	} = (
		nextLogicalColour,
		maybePieceByCoordinates,
		getCastleableRooksByLogicalColour position,
		getMaybeEnPassantAbscissa position
	 ) `compare` (
		nextLogicalColour',
		maybePieceByCoordinates',
		getCastleableRooksByLogicalColour position',
		getMaybeEnPassantAbscissa position'
	 )

instance Control.DeepSeq.NFData Position where
	rnf MkPosition {
		getNextLogicalColour			= nextLogicalColour,
		getMaybePieceByCoordinates		= maybePieceByCoordinates,
		getCastleableRooksByLogicalColour	= castleableRooksByLogicalColour,
		getMaybeEnPassantAbscissa		= maybeEnPassantAbscissa
	} = Control.DeepSeq.rnf (nextLogicalColour, maybePieceByCoordinates, castleableRooksByLogicalColour, maybeEnPassantAbscissa)

instance Property.Reflectable.ReflectableOnX Position where
	reflectOnX position@MkPosition {
		getNextLogicalColour			= nextLogicalColour,
		getMaybePieceByCoordinates		= maybePieceByCoordinates,
		getCastleableRooksByLogicalColour	= castleableRooksByLogicalColour
	} = position {
		getNextLogicalColour			= Property.Opposable.getOpposite nextLogicalColour,
		getMaybePieceByCoordinates		= Property.Reflectable.reflectOnX maybePieceByCoordinates,
		getCastleableRooksByLogicalColour	= Property.Reflectable.reflectOnX castleableRooksByLogicalColour
	}

instance Component.Zobrist.Hashable Position where
	listRandoms MkPosition {
		getNextLogicalColour			= nextLogicalColour,
		getMaybePieceByCoordinates		= maybePieceByCoordinates,
		getCastleableRooksByLogicalColour	= castleableRooksByLogicalColour,
		getMaybeEnPassantAbscissa		= maybeEnPassantAbscissa
	} zobrist	= (
		if Attribute.LogicalColour.isBlack nextLogicalColour
			then (Component.Zobrist.getRandomForBlacksMove zobrist :)
			else id
	 ) . Data.Maybe.maybe id (
		(++) . (`Component.Zobrist.listRandoms` zobrist)
	 ) maybeEnPassantAbscissa $ Component.Zobrist.listRandoms castleableRooksByLogicalColour zobrist ++ Component.Zobrist.listRandoms maybePieceByCoordinates zobrist

-- | Constructor.
mkPosition
	:: Attribute.LogicalColour.LogicalColour	-- ^ The logical colour of the next player to move.
	-> State.MaybePieceByCoordinates.MaybePieceByCoordinates
	-> State.CastleableRooksByLogicalColour.CastleableRooksByLogicalColour
	-> Maybe Component.Turn.Turn			-- ^ The last /turn/ made.
	-> Position
mkPosition nextLogicalColour maybePieceByCoordinates castleableRooksByLogicalColour maybeLastTurn	= MkPosition {
	getNextLogicalColour			= nextLogicalColour,
	getMaybePieceByCoordinates		= maybePieceByCoordinates,	-- N.B.: one could have used 'State.CoordinatesByRankByLogicalColour.CoordinatesByRankByLogicalColour', except that the coordinates have an undefined order.
	getCastleableRooksByLogicalColour	= castleableRooksByLogicalColour,
	getMaybeEnPassantAbscissa		= maybeLastTurn >>= State.EnPassantAbscissa.mkMaybeEnPassantAbscissa nextLogicalColour maybePieceByCoordinates
}

