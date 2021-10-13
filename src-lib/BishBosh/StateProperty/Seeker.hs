{-# LANGUAGE MultiParamTypeClasses #-}
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

 [@DESCRIPTION@]	Permits discovery within a board.
-}

module BishBosh.StateProperty.Seeker(
-- * Type-classes
	Seeker(..),
-- * Functions
	findAllPieces
) where

import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Component.Piece		as Component.Piece

-- | An interface which may be implemented by data which can search the board.
class Seeker seeker where
	-- | Locate any @Knight@s capable of taking a piece at the specified coordinates.
	findProximateKnights
		:: Attribute.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the @Knight@ for which to search.
		-> Cartesian.Coordinates.Coordinates		-- ^ The destination to which the @Knight@ is required to be capable of jumping.
		-> seeker
		-> [Cartesian.Coordinates.Coordinates]

	-- | Locate any /piece/s satisfying the specified predicate.
	findPieces
		:: (Component.Piece.Piece -> Bool)	-- ^ Predicate.
		-> seeker
		-> [Component.Piece.LocatedPiece]

-- | Locate all /piece/s on the board.
findAllPieces :: Seeker seeker => seeker -> [Component.Piece.LocatedPiece]
findAllPieces	= findPieces $ const True

