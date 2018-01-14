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

 [@DESCRIPTION@]

	* Defines the state of the game, without regard to how it arrived there; <https://chessprogramming.wikispaces.com/Chess+Position>.

	* Games with the same /position/ have the same opportunities.

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
import qualified	BishBosh.Types					as T
import qualified	Control.DeepSeq
import qualified	Data.Array.IArray
import qualified	Data.Maybe

{- |
	* The state of the game, without regard to how it arrived there; <https://chessprogramming.wikispaces.com/Chess+Position>.

	* Games with the same /position/ may be considered to have converged, since they now have equal opportunity.
-}
data Position x y	= MkPosition {
	getNextLogicalColour			:: Attribute.LogicalColour.LogicalColour,	-- ^ The next player to move.
	getMaybePieceByCoordinates		:: State.MaybePieceByCoordinates.MaybePieceByCoordinates x y,
	getCastleableRooksByLogicalColour	:: State.CastleableRooksByLogicalColour.CastleableRooksByLogicalColour x,
	getMaybeEnPassantAbscissa		:: Maybe (State.EnPassantAbscissa.EnPassantAbscissa x)
} deriving Eq

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => Ord (Position x y) where
	{-# SPECIALISE instance Ord (Position T.X T.Y) #-}
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

instance (
	Control.DeepSeq.NFData	x,
	Control.DeepSeq.NFData	y
 ) => Control.DeepSeq.NFData (Position x y) where
	rnf MkPosition {
		getNextLogicalColour			= nextLogicalColour,
		getMaybePieceByCoordinates		= maybePieceByCoordinates,
		getCastleableRooksByLogicalColour	= castleableRooksByLogicalColour,
		getMaybeEnPassantAbscissa		= maybeEnPassantAbscissa
	} = Control.DeepSeq.rnf (nextLogicalColour, maybePieceByCoordinates, castleableRooksByLogicalColour, maybeEnPassantAbscissa)

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => Property.Reflectable.ReflectableOnX (Position x y) where
	reflectOnX position@MkPosition {
		getNextLogicalColour			= nextLogicalColour,
		getMaybePieceByCoordinates		= maybePieceByCoordinates,
		getCastleableRooksByLogicalColour	= castleableRooksByLogicalColour
	} = position {
		getNextLogicalColour			= Property.Opposable.getOpposite nextLogicalColour,
		getMaybePieceByCoordinates		= Property.Reflectable.reflectOnX maybePieceByCoordinates,
		getCastleableRooksByLogicalColour	= Property.Reflectable.reflectOnX castleableRooksByLogicalColour
	}

instance (Data.Array.IArray.Ix x, Enum x, Enum y, Ord y) => Component.Zobrist.Hashable2D Position x y {-CAVEAT: FlexibleInstances, MultiParamTypeClasses-} where
	listRandoms2D MkPosition {
		getNextLogicalColour			= nextLogicalColour,
		getMaybePieceByCoordinates		= maybePieceByCoordinates,
		getCastleableRooksByLogicalColour	= castleableRooksByLogicalColour,
		getMaybeEnPassantAbscissa		= maybeEnPassantAbscissa
	} zobrist	= (
		if Attribute.LogicalColour.isBlack nextLogicalColour
			then (Component.Zobrist.getRandomForBlacksMove zobrist :)
			else id
	 ) . Data.Maybe.maybe id (
		(++) . (`Component.Zobrist.listRandoms1D` zobrist)
	 ) maybeEnPassantAbscissa $ Component.Zobrist.listRandoms1D castleableRooksByLogicalColour zobrist ++ Component.Zobrist.listRandoms2D maybePieceByCoordinates zobrist

-- | Constructor.
mkPosition :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 )
	=> Attribute.LogicalColour.LogicalColour	-- ^ The logical colour of the next player to move.
	-> State.MaybePieceByCoordinates.MaybePieceByCoordinates x y
	-> State.CastleableRooksByLogicalColour.CastleableRooksByLogicalColour x
	-> Maybe (Component.Turn.Turn x y)		-- ^ The last /turn/ made.
	-> Position x y
mkPosition nextLogicalColour maybePieceByCoordinates castleableRooksByLogicalColour maybeLastTurn	= MkPosition {
	getNextLogicalColour			= nextLogicalColour,
	getMaybePieceByCoordinates		= maybePieceByCoordinates,	-- N.B.: one could have used 'State.CoordinatesByRankByLogicalColour.CoordinatesByRankByLogicalColour', except that the coordinates have an undefined order.
	getCastleableRooksByLogicalColour	= castleableRooksByLogicalColour,
	getMaybeEnPassantAbscissa		= maybeLastTurn >>= State.EnPassantAbscissa.mkMaybeEnPassantAbscissa nextLogicalColour maybePieceByCoordinates
}

