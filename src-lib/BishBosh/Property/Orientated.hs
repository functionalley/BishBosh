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

import	Control.Arrow((&&&), (|||))

{- |
	* An interface for vector-like data.

	* CAVEAT: the default implementations are mutually dependent, & could result in infinite recursion.
-}
class Orientated a where
	isVertical :: a -> Bool	-- ^ Whether the datum is aligned with a file of the board.
	isVertical	= uncurry (&&) . (isParallel &&& not . isHorizontal)	-- Default implementation.

	isHorizontal :: a -> Bool	-- ^ Whether the datum is aligned with a rank of the board.
	isHorizontal	= uncurry (&&) . (isParallel &&& not . isVertical)	-- Default implementation.

	isParallel	:: a -> Bool	-- ^ Whether it is parallel to an edge of the board.
	isParallel	= uncurry (||) . (isVertical &&& isHorizontal)		-- Default implementation.

	isDiagonal	:: a -> Bool	-- ^ Whether it is diagonal (45 degrees) wrt the edges of the board.
	isDiagonal	= uncurry (&&) . (isStraight &&& not . isParallel)	-- Default implementation.

	isStraight :: a -> Bool
	isStraight	= uncurry (||) . (isParallel &&& isDiagonal)		-- Default implementation.

instance (Orientated l, Orientated r) => Orientated (Either l r) where
	isVertical	= isVertical ||| isVertical
	isHorizontal	= isHorizontal ||| isHorizontal
	isDiagonal	= isDiagonal ||| isDiagonal
	isParallel	= isParallel ||| isParallel
	isStraight	= isStraight ||| isStraight

