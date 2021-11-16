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

-}

module BishBosh.Property.Arboreal(
-- * Type-classes
	Prunable(..),
-- * Types
-- ** Type-synonyms
	Depth
) where

-- | A distance down the tree.
type Depth	= Int

-- | An interface which tree-like data can support.
class Prunable tree where
	prune	:: Depth -> tree -> tree	-- ^ Remove branches after the specified depth.

