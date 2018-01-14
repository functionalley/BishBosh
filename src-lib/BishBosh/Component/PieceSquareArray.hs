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

 [@DESCRIPTION@]	Defines the relative value of a specific rank of piece, occupying a specific coordinate on the board, at a specific stage in the game.
-}

module BishBosh.Component.PieceSquareArray(
-- * Types
-- ** Type-synonyms
	InterpolatedPieceSquareValues,
--	InterpolatedPieceSquareValuesByCoordinates,
	FindPieceSquareValue,
-- ** Data-types
	PieceSquareArray,
-- * Functions
	findPieceSquareValue,
-- ** Constructor
	mkPieceSquareArray
) where

import			BishBosh.Data.Bool()
import			Data.Array.IArray((!))
import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Component.Piece		as Component.Piece
import qualified	BishBosh.Property.Reflectable		as Property.Reflectable
import qualified	Control.DeepSeq
import qualified	Data.Array.IArray

-- | Either a single /pieceSquareValue/ or an array of values indexed by the total number of pieces remaining on the board.
type InterpolatedPieceSquareValues pieceSquareValue	= Either pieceSquareValue (Data.Array.IArray.Array Component.Piece.NPieces pieceSquareValue)

-- | Self-documentation.
type InterpolatedPieceSquareValuesByCoordinates x y pieceSquareValue	= Cartesian.Coordinates.ByCoordinates x y (InterpolatedPieceSquareValues pieceSquareValue)

-- | The value for each type of /piece/ of occupying each coordinate, at each stage in the lifetime of the game.
newtype PieceSquareArray x y pieceSquareValue		= MkPieceSquareArray {
	deconstruct	:: Attribute.Rank.ByRank (InterpolatedPieceSquareValuesByCoordinates x y pieceSquareValue)
} deriving (Eq, Show)

instance (
	Control.DeepSeq.NFData	pieceSquareValue,
	Control.DeepSeq.NFData	x,
	Control.DeepSeq.NFData	y
 ) => Control.DeepSeq.NFData (PieceSquareArray x y pieceSquareValue) where
	rnf MkPieceSquareArray { deconstruct = byRank }	= Control.DeepSeq.rnf byRank

-- | Constructor.
mkPieceSquareArray :: (Attribute.Rank.Rank -> InterpolatedPieceSquareValuesByCoordinates x y pieceSquareValue) -> PieceSquareArray x y pieceSquareValue
mkPieceSquareArray	= MkPieceSquareArray . Attribute.Rank.listArrayByRank . (`map` Attribute.Rank.range)

-- | The type of a function which can find the appropriate piece-square value.
type FindPieceSquareValue x y pieceSquareValue
	= Attribute.LogicalColour.LogicalColour		-- ^ The piece's logical colour.
	-> Attribute.Rank.Rank				-- ^ The piece's rank.
	-> Cartesian.Coordinates.Coordinates x y	-- ^ The piece's location.
	-> pieceSquareValue

-- | Find the piece-square value, at a stage in the game's lifetime defined by the total number of piece remaining, for the specified /piece/ & /coordinates/.
findPieceSquareValue :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 )
	=> Component.Piece.NPieces			-- ^ The progress through the game.
	-> Attribute.LogicalColour.LogicalColour	-- ^ The piece's logical colour.
	-> Attribute.Rank.Rank				-- ^ The piece's rank.
	-> Cartesian.Coordinates.Coordinates x y	-- ^ The piece's location.
	-> PieceSquareArray x y pieceSquareValue
	-> pieceSquareValue
-- {-# SPECIALISE findPieceSquareValue :: Component.Piece.NPieces -> Attribute.LogicalColour.LogicalColour -> Attribute.Rank.Rank -> Cartesian.Coordinates.Coordinates T.X T.Y -> PieceSquareArray T.X T.Y pieceSquareValue -> pieceSquareValue #-}
{-# INLINE findPieceSquareValue #-}
findPieceSquareValue nPieces logicalColour rank coordinates MkPieceSquareArray { deconstruct = byRank }	= either id (! nPieces) $ byRank ! rank ! (
	if Attribute.LogicalColour.isBlack logicalColour
		then Property.Reflectable.reflectOnX
		else id
 ) coordinates
