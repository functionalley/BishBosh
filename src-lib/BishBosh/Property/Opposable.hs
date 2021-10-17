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

 [@DESCRIPTION@]	An interface for data which have the concept of an opposite.
-}

module BishBosh.Property.Opposable(
-- * Type-classes
	Opposable(..)
) where

import Prelude(Ordering(..))

-- | An interface which data which have the concept of an opposite, may implement.
class Opposable a where
	getOpposite	:: a -> a

instance Opposable Ordering where
	getOpposite LT	= GT
	getOpposite GT	= LT
	getOpposite _	= EQ

