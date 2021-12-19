{-# LANGUAGE CPP #-}
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

 [@DESCRIPTION@]	Defines the values of a single unspecified piece, of occupying various coordinates.
-}

module BishBosh.Component.PieceSquareValueByCoordinates(
-- * Types
-- ** Type-synonyms
--	ByCoordinates,
-- ** Data-types
	PieceSquareValueByCoordinates(),
-- * Functions
-- ** Accessors
	dereference,
	getPieceSquareValue,
	toList,
-- ** Constructors
	fromList
) where

import			Data.Array.IArray((!))
import qualified	BishBosh.Cartesian.Coordinates	as Cartesian.Coordinates
import qualified	BishBosh.Colour.LogicalColour	as Colour.LogicalColour
import qualified	BishBosh.Property.Reflectable	as Property.Reflectable
import qualified	BishBosh.Type.Mass		as Type.Mass
import qualified	Control.DeepSeq
import qualified	Data.Array.IArray

type ByCoordinates	=
#ifdef UNBOX_TYPEMASS_ARRAYS
	Cartesian.Coordinates.UArrayByCoordinates
#else
	Cartesian.Coordinates.ArrayByCoordinates
#endif
		Type.Mass.PieceSquareValue

-- | The piece-square value varies with coordinates at which the unspecified piece is located.
newtype PieceSquareValueByCoordinates	= MkPieceSquareValueByCoordinates ByCoordinates deriving (Eq, Show)

instance Control.DeepSeq.NFData PieceSquareValueByCoordinates where
	rnf (MkPieceSquareValueByCoordinates pieceSquareValueByCoordinates)	=
#ifdef UNBOX_TYPEMASS_ARRAYS
		Control.DeepSeq.rwhnf
#else
		Control.DeepSeq.rnf
#endif
			pieceSquareValueByCoordinates

-- | Constructor from a list ordered by /coordinates/.
fromList :: [Type.Mass.PieceSquareValue] -> PieceSquareValueByCoordinates
fromList	= MkPieceSquareValueByCoordinates . Cartesian.Coordinates.listArrayByCoordinates

-- | Deconstruct, returning the ordered list from which it was constructed.
toList :: PieceSquareValueByCoordinates -> [Type.Mass.PieceSquareValue]
toList (MkPieceSquareValueByCoordinates pieceSquareValueByCoordinates)	= Data.Array.IArray.elems pieceSquareValueByCoordinates

-- | Extract the value for the specified /coordinates/ (from @White@'s perspective).
dereference :: PieceSquareValueByCoordinates -> Cartesian.Coordinates.Coordinates -> Type.Mass.PieceSquareValue
dereference (MkPieceSquareValueByCoordinates pieceSquareValueByCoordinates)	= (pieceSquareValueByCoordinates !)

{- |
	* Get the piece-square value, for the specified /coordinates/.

	* Utilises symmetry to infer values for @Black@, by reflecting the specified /coordinates/ into @White@'s domain.
-}
getPieceSquareValue
	:: PieceSquareValueByCoordinates
	-> Colour.LogicalColour.LogicalColour	-- ^ The /piece/'s /logical colour/.
	-> Cartesian.Coordinates.Coordinates	-- ^ The /piece/'s location.
	-> Type.Mass.PieceSquareValue
getPieceSquareValue (MkPieceSquareValueByCoordinates pieceSquareValueByCoordinates) logicalColour	= (pieceSquareValueByCoordinates !) . if Colour.LogicalColour.isBlack logicalColour
	then Property.Reflectable.reflectOnX
	else id

