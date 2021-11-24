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

	* Describes the directional components of a diagonal line.
-}

module BishBosh.Direction.Diagonal(
-- * Types
-- ** Data-types
	Diagonal()
) where

import			Control.Arrow((***))
import qualified	BishBosh.Direction.Horizontal		as Direction.Horizontal
import qualified	BishBosh.Direction.Vertical		as Direction.Vertical
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	BishBosh.Property.Orientated		as Property.Orientated
import qualified	BishBosh.Property.Reflectable		as Property.Reflectable
import qualified	Control.DeepSeq
import qualified	Data.List.Extra

-- | Directions at 45 degress to all edges of the board; those in which a Bishop can move.
data Diagonal	= MkDiagonal {
	getVertical	:: Direction.Vertical.Vertical,		-- ^ The vertical component of the diagonal.
	getHorizontal	:: Direction.Horizontal.Horizontal	-- ^ The horizontal component of the diagonal.
} deriving (Eq, Ord)

instance Control.DeepSeq.NFData Diagonal where
	rnf MkDiagonal {
		getVertical	= _,
		getHorizontal	= _
	} = ()

instance Enum Diagonal where
	fromEnum MkDiagonal {
		getVertical	= v,
		getHorizontal	= h
	} = fromIntegral Direction.Vertical.nVerticals * fromEnum v + fromEnum h

	toEnum i	= uncurry MkDiagonal . (toEnum *** toEnum) $! i `divMod` fromIntegral Direction.Vertical.nVerticals

instance Show Diagonal where
	showsPrec precedence MkDiagonal {
		getVertical	= v,
		getHorizontal	= h
	} = showsPrec precedence v . showsPrec precedence h

instance Read Diagonal where
	readsPrec precedence s	= case readsPrec precedence $ Data.List.Extra.trimStart s of
		[(vertical, s')]	-> case readsPrec precedence s' of
			[(horizontal, s'')]	-> [
				(
					MkDiagonal {
						getVertical	= vertical,
						getHorizontal	= horizontal
					},
					s''
				) -- Pair.
			 ] -- Singleton.
			_			-> []	-- No parse.
		_			-> []	-- No parse.

instance Property.FixedMembership.FixedMembership Diagonal where
	members	= map MkDiagonal Property.FixedMembership.members {-vertical-} <*> Property.FixedMembership.members {-horizontal-}

instance Property.Opposable.Opposable Diagonal where
	getOpposite MkDiagonal {
		getVertical	= v,
		getHorizontal	= h
	} = MkDiagonal {
		getVertical	= Property.Opposable.getOpposite v,
		getHorizontal	= Property.Opposable.getOpposite h
	}

instance Property.Orientated.Orientated Diagonal where
	isParallel	= const False
	isDiagonal	= const True
	isStraight	= const True

instance Property.Reflectable.ReflectableOnX Diagonal where
	reflectOnX diagonal@MkDiagonal {
		getVertical	= v
	} = diagonal {
		getVertical	= Property.Reflectable.reflectOnX v
	}

instance Property.Reflectable.ReflectableOnY Diagonal where
	reflectOnY diagonal@MkDiagonal {
		getHorizontal	= h
	} = diagonal {
		getHorizontal	= Property.Reflectable.reflectOnY h
	}

