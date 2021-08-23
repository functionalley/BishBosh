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

 [@DESCRIPTION@]

	* An interface for data which can only have a fixed number of well-defined members.

	* Typically for sum-types with exclusively nullary constructors; it *could* be implemented for product-types, but the membership grows exponentially.
-}

module BishBosh.Property.FixedMembership(
-- * Type-classes
	FixedMembership(..)
) where

-- | An interface for data which can only have a fixed number of well-defined members.
class FixedMembership a where
	members	:: [a]	-- ^ Identify the members.

instance FixedMembership Bool where
	members	= [False, True]

instance FixedMembership Ordering where
	members	= [LT, EQ, GT]

instance FixedMembership a => FixedMembership (Maybe a) where
	members	= Nothing : map Just members

