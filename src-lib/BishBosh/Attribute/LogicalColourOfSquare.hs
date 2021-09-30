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

	* Defines the data-type which represents the logical (rather than physical) colour of the squares on a chess-board.

	* N.B.: conceptually different from the logical colour of pieces.
-}

module BishBosh.Attribute.LogicalColourOfSquare(
-- * Types
-- ** Data-type
	LogicalColourOfSquare(),
-- * Constants
	black,
	white,
-- * Functions
-- ** Predicate
	isBlack
) where

import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour

-- | The logical colour of a square of the board, cf. of a piece or (conceptually) of a player.
newtype LogicalColourOfSquare	= MkLogicalColourOfSquare {
	deconstruct	:: Attribute.LogicalColour.LogicalColour
} deriving (Eq, Bounded)

-- | Constant.
black :: LogicalColourOfSquare
black	= MkLogicalColourOfSquare Attribute.LogicalColour.Black

-- | Constant.
white :: LogicalColourOfSquare
white	= MkLogicalColourOfSquare Attribute.LogicalColour.White

-- | Whether the specified /logical colour/ is @Black@.
isBlack :: LogicalColourOfSquare -> Bool
isBlack MkLogicalColourOfSquare { deconstruct = logicalColour }	= Attribute.LogicalColour.isBlack logicalColour

