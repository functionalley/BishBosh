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
-- * Type-synonyms
	QualifiedMoveSequence,
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
data QualifiedMove	= MkQualifiedMove {
	getMove		:: Component.Move.Move,
	getMoveType	:: Attribute.MoveType.MoveType
} deriving Eq

instance Show QualifiedMove where
	showsPrec precedence MkQualifiedMove {
		getMove		= move,
		getMoveType	= moveType
	} = showsPrec precedence (move, moveType)

instance Read QualifiedMove where
	readsPrec precedence	= map (Control.Arrow.first $ uncurry MkQualifiedMove) . readsPrec precedence

instance Control.DeepSeq.NFData QualifiedMove where
	rnf MkQualifiedMove {
		getMove		= move,
		getMoveType	= moveType
	} = Control.DeepSeq.rnf (move, moveType)

instance Property.Reflectable.ReflectableOnX QualifiedMove where
	reflectOnX qualifiedMove@MkQualifiedMove { getMove = move }	= qualifiedMove { getMove = Property.Reflectable.reflectOnX move }

-- | Constructor.
mkQualifiedMove	:: Component.Move.Move -> Attribute.MoveType.MoveType -> QualifiedMove
mkQualifiedMove	= MkQualifiedMove

-- | A sequence of QualifiedMoves applied to a game.
type QualifiedMoveSequence	= [QualifiedMove]

