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

 [@DESCRIPTION@]	An interface for data which can be transformed by reflection.
-}

module BishBosh.Property.Reflectable (
-- * Type-classes
	ReflectableOnX(..),
	ReflectableOnY(..)
) where

import	Prelude(map)

-- | An interface which data which can be transformed by reflection about the /x/-axis, may implement.
class ReflectableOnX a where
	reflectOnX	:: a -> a	-- ^ Reflect about the /x/-axis, i.e. to top to bottom & vice-versa.

instance ReflectableOnX a => ReflectableOnX [a] where
	reflectOnX	= map reflectOnX

-- | An interface which data which can be transformed by reflection about the /y/-axis, may implement.
class ReflectableOnY a where
	reflectOnY	:: a -> a	-- ^ Reflect about the /y/-axis, i.e. left to right & vice-versa.

