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

	* An interface for data which can support the concept of being empty.

	* N.B.: the empty state of the type must be unique.

	* cf. 'Data.Default.def' or /zero/ for a numeric type.
-}

module BishBosh.Property.Empty(
-- * Type-classes
	Empty(..)
) where

import qualified	Data.IntMap	as IntMap
import qualified	Data.Map	as Map
import qualified	Data.Set

-- | An interface which data which can support the concept of being empty, may implement.
class Empty a where
	empty	:: a	-- ^ A constant empty state.

instance (Empty a, Empty b) => Empty (a, b) where
	empty	= (empty, empty)

instance Empty (Maybe a) where
	empty	= Nothing

instance Empty [a] where
	empty	= []

instance Empty (IntMap.IntMap e) where
	empty	= IntMap.empty

instance Empty (Map.Map i e) where
	empty	= Map.empty

instance Empty (Data.Set.Set i) where
	empty	= Data.Set.empty

