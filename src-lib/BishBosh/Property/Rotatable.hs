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

	* An interface for data which can be transformed by rotation.

	* CAVEAT: only rotations in discrete 90-degrees steps are considered.
-}

module BishBosh.Property.Rotatable (
-- * Type-classes
	Rotatable(..)
) where

import			Prelude((.))
import qualified	BishBosh.Property.Reflectable	as Property.Reflectable

-- | An interface which data which can be transformed by rotation, may implement.
class (Property.Reflectable.ReflectableOnX a, Property.Reflectable.ReflectableOnY a) => Rotatable a where
	rotate90	:: a -> a	-- ^ Rotate anticlockwise by 90 degrees, so that @N@ becomes @W@.
	rotate90 = rotate270 . rotate180

	rotate180	:: a -> a	-- ^ Rotate by 180 degrees.
	rotate180 = Property.Reflectable.reflectOnX . Property.Reflectable.reflectOnY	-- CAVEAT: perhaps not the most efficient implementation.

	rotate270	:: a -> a	-- ^ Rotate clockwise by 90 degrees, so that @N@ becomes @E@.
	rotate270 = rotate90 . rotate180

