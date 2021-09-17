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

 [@DESCRIPTION@]	Qualifies a /move/ with a /move-type/.
-}

module BishBosh.Component.QualifiedMove(
-- * Types
-- ** Data-types
	QualifiedMove(
--		MkQualifiedMove,
		getMove,
		getMoveType
	),
-- * Functions
-- ** Constructors
	mkQualifiedMove
) where

import qualified	BishBosh.Attribute.MoveType	as Attribute.MoveType
import qualified	BishBosh.Component.Move		as Component.Move
import qualified	BishBosh.Property.Reflectable	as Property.Reflectable
import qualified	Control.Arrow
import qualified	Control.DeepSeq

-- | A move qualified by its /movetype/.
data QualifiedMove x y	= MkQualifiedMove {
	getMove		:: Component.Move.Move x y,
	getMoveType	:: Attribute.MoveType.MoveType
} deriving Eq

instance (Show x, Show y) => Show (QualifiedMove x y) where
	showsPrec precedence MkQualifiedMove {
		getMove		= move,
		getMoveType	= moveType
	} = showsPrec precedence (move, moveType)

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Read	x,
	Read	y
 ) => Read (QualifiedMove x y) where
	readsPrec precedence	= map (Control.Arrow.first $ uncurry MkQualifiedMove) . readsPrec precedence

instance (Control.DeepSeq.NFData x, Control.DeepSeq.NFData y) => Control.DeepSeq.NFData (QualifiedMove x y) where
	rnf MkQualifiedMove {
		getMove		= move,
		getMoveType	= moveType
	} = Control.DeepSeq.rnf (move, moveType)

instance Enum y => Property.Reflectable.ReflectableOnX (QualifiedMove x y) where
	reflectOnX qualifiedMove@MkQualifiedMove { getMove = move }	= qualifiedMove { getMove = Property.Reflectable.reflectOnX move }

-- | Constructor.
mkQualifiedMove	:: Component.Move.Move x y -> Attribute.MoveType.MoveType -> QualifiedMove x y
mkQualifiedMove	= MkQualifiedMove

