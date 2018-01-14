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

 [@DESCRIPTION@]	An interface for vector-like data which is orientated to the edges of the board.
-}

module BishBosh.Property.Orientated (
-- * Type-classes
	Orientated(..)
) where

import	Control.Arrow((&&&))

-- | An interface for vector-like data.
class Orientated a where
	isDiagonal	:: a -> Bool	-- ^ Whether it is diagonal wrt the edges of the board.
	isParallel	:: a -> Bool	-- ^ Whether it is parallel to an edge of the board.

	isStraight :: a -> Bool
	isStraight	= uncurry (||) . (isParallel &&& isDiagonal)	-- Default implementation.

