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

	* cf. "Data.Default" or 'zero' for a numeric type.
-}

module BishBosh.Property.Empty(
-- * Type-classes
	Empty(..)
) where

-- | An interface which data which can support the concept of being empty, may implement.
class Empty a where
	empty	:: a	-- ^ Constant.

instance (Empty a, Empty b) => Empty (a, b) where
	empty	= (empty, empty)

