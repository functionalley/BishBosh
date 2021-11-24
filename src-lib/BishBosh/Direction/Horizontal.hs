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

	* Describes the direction of a rank of the board.
-}

module BishBosh.Direction.Horizontal(
-- * Types
-- ** Data-types
	Horizontal(),
-- * Constants
--	range,
	nHorizontals
) where

import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	BishBosh.Property.Reflectable		as Property.Reflectable
import qualified	BishBosh.Type.Count			as Type.Count

-- | The sum-type of orientations of all lines of constant rank.
data Horizontal	= W | E deriving (Enum, Eq, Ord, Show)

instance Read Horizontal where
	readsPrec _ ('W' : s)	= [(W, s)]
	readsPrec _ ('E' : s)	= [(E, s)]
	readsPrec _ _		= []	-- No parse.

instance Property.FixedMembership.FixedMembership Horizontal where
	members	= range

instance Property.Opposable.Opposable Horizontal where
	getOpposite W	= E
	getOpposite E	= W

instance Property.Reflectable.ReflectableOnY Horizontal where
	reflectOnY	= Property.Opposable.getOpposite

-- | Constant range of values.
range :: [Horizontal]
range	= [ toEnum 0 .. ]

-- | The number of verticals directions.
nHorizontals :: Type.Count.NDirections
nHorizontals	= fromIntegral $ length range

