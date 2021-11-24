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

	* Describes the directional components of a line parallel to an edge of the board.
-}

module BishBosh.Direction.Parallel(
-- * Types
-- ** Data-types
	Parallel(),
-- * Constants
--	range,
	nParallels
) where

import			Control.Arrow((|||), (+++))
import qualified	BishBosh.Direction.Horizontal		as Direction.Horizontal
import qualified	BishBosh.Direction.Vertical		as Direction.Vertical
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	BishBosh.Property.Orientated		as Property.Orientated
import qualified	BishBosh.Property.Reflectable		as Property.Reflectable
import qualified	BishBosh.Type.Count			as Type.Count
import qualified	Control.DeepSeq
import qualified	Data.List.Extra

-- | Directions parallel to two edges of the board; those in which a Rook can move.
newtype Parallel	= MkParallel (Either Direction.Vertical.Vertical Direction.Horizontal.Horizontal) deriving (Eq, Ord)

instance Control.DeepSeq.NFData Parallel where
	rnf (MkParallel p)	= Control.DeepSeq.rwhnf ||| Control.DeepSeq.rwhnf $ p

instance Enum Parallel where
	fromEnum (MkParallel p)	= fromEnum ||| (+ fromIntegral Direction.Vertical.nVerticals) . fromEnum $ p

	toEnum i	= MkParallel $! case i `divMod` fromIntegral Direction.Vertical.nVerticals of
		(0, remainder)	-> Left $! toEnum remainder
		(~1, remainder)	-> Right $! toEnum remainder

instance Show Parallel where
	showsPrec precedence (MkParallel p)	= showsPrec precedence ||| showsPrec precedence $ p

instance Read Parallel where
	readsPrec precedence s	= let
		s'	= Data.List.Extra.trimStart s
	 in case readsPrec precedence s' of
		[(vertical, s'')]	-> [(MkParallel $ Left vertical, s'')]
		_			-> case readsPrec precedence s' of
			[(horizontal, s'')]	-> [(MkParallel $ Right horizontal, s'')]
			_			-> []	-- No parse.

instance Property.FixedMembership.FixedMembership Parallel where
	members	= range

instance Property.Opposable.Opposable Parallel where
	getOpposite (MkParallel p)	= MkParallel $ (Property.Opposable.getOpposite +++ Property.Opposable.getOpposite) p

instance Property.Orientated.Orientated Parallel where
	isVertical (MkParallel (Left _))	= True
	isVertical _				= False

	isHorizontal (MkParallel (Right _))	= True
	isHorizontal _				= False

	isParallel	= const True

	isDiagonal	= const False

	isStraight	= const True

instance Property.Reflectable.ReflectableOnX Parallel where
	reflectOnX (MkParallel p)	= MkParallel $ (Property.Reflectable.reflectOnX +++ id) p

instance Property.Reflectable.ReflectableOnY Parallel where
	reflectOnY (MkParallel p)	= MkParallel $ (id +++ Property.Reflectable.reflectOnY) p

-- | Constant range of values.
range :: [Parallel]
range	= take (fromIntegral $ Direction.Vertical.nVerticals + Direction.Horizontal.nHorizontals) [ toEnum 0 .. ]

-- | The number of verticals directions.
nParallels :: Type.Count.NDirections
nParallels	= fromIntegral $ length range

