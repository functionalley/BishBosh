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

	* The type returned when reading a move encoded in a string, using any of a variety of notations.

	* The internal representation of a /move/ is merely a pair of coordinates, but in various chess-notations, additional information is provided.

	* The quantity of additional information is dependent of the specific notation:
		some identify castling, en-passant, promotion /rank/, taken /rank/;
		some merely define the /rank/ to which a @Pawn@ should be promoted.
-}

module BishBosh.Component.EitherQualifiedMove(
-- * Types
-- ** Data-types
	EitherQualifiedMove(
--		MkEitherQualifiedMove,
		getMove,
		getPromotionRankOrMoveType
	),
-- * Functions
-- ** Constructors
	mkPartiallyQualifiedMove,
	mkFullyQualifiedMove
) where

import qualified	BishBosh.Attribute.MoveType	as Attribute.MoveType
import qualified	BishBosh.Attribute.Rank		as Attribute.Rank
import qualified	BishBosh.Component.Move		as Component.Move

-- | As returned by 'Notation.MoveNotation.readsQualifiedMove'.
data EitherQualifiedMove x y	= MkEitherQualifiedMove {
	getMove				:: Component.Move.Move x y,
	getPromotionRankOrMoveType	:: Either (Maybe Attribute.Rank.Rank) Attribute.MoveType.MoveType	-- ^ Either the optional /rank/ to which a @Pawn@ should be promoted, or the complete /move-type/.
}

-- | Constructor for notations which don't encode sufficient information to reliably re-construct the /move-type/, but merely the /rank/ to which a @Pawn@ is to be promoted.
mkPartiallyQualifiedMove :: Component.Move.Move x y -> Maybe Attribute.Rank.Rank -> EitherQualifiedMove x y
mkPartiallyQualifiedMove move	= MkEitherQualifiedMove move . Left

-- | Constructor for notations which encode sufficient information to reliably re-construct the /move-type/.
mkFullyQualifiedMove :: Component.Move.Move x y -> Attribute.MoveType.MoveType -> EitherQualifiedMove x y
mkFullyQualifiedMove move	= MkEitherQualifiedMove move . Right

