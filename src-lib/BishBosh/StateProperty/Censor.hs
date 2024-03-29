{-# LANGUAGE CPP #-}
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

 [@DESCRIPTION@]	Maintains a census of the /piece/s on the board, without regard to their location.
-}

module BishBosh.StateProperty.Censor(
-- * Types
-- ** Type-synonyms
	NPiecesByRank,
-- * Type-classes
	Censor(..),
-- * Functions
	findInvalidity
) where

import			Control.Arrow((&&&), (***))
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Component.Piece		as Component.Piece
import qualified	BishBosh.Property.SelfValidating	as Property.SelfValidating
import qualified	BishBosh.Type.Count			as Type.Count

-- | The difference in the number of /piece/s of each /rank/ held by either side.
type NPiecesByRank	=
#ifdef UNBOX_TYPECOUNT_ARRAYS
	Attribute.Rank.UArrayByRank
#else
	Attribute.Rank.ArrayByRank
#endif
		Type.Count.NPieces

-- | An interface which may be implemented by data which can perform a census of the /piece/s on the /board/.
class Censor censor where
	countPiecesByLogicalColour	:: censor -> (Type.Count.NPieces, Type.Count.NPieces)	-- ^ The total number of /piece/s, partitioned into @Black@ & @White@ respectively.

	countPieces			:: censor -> Type.Count.NPieces				-- ^ The total number of /piece/s on the board, regardless of logical colour.
	countPieces	= uncurry (+) . countPiecesByLogicalColour	-- Default implementation.

	countPieceDifferenceByRank	:: censor -> NPiecesByRank				-- ^ Finds the difference between the number of /piece/s of each /rank/ held by each side. N.B. for this purpose, @White@ is arbitrarily considered positive & @Black@ negative.

	hasInsufficientMaterial		:: censor -> Bool					-- ^ Whether insufficient material remains on the board, to force check-mate; <https://en.wikipedia.org/wiki/Draw_(chess)>.

	hasBothKings			:: censor -> Bool					-- ^ Whether there's exactly one @King@ of each /logical colour/.

-- | Self-validate.
findInvalidity :: Censor censor => censor -> [String]
findInvalidity	= Property.SelfValidating.findErrors [
	(
		not . hasBothKings,
		"there must be exactly one King of each logical colour."
	), (
		uncurry (||) . uncurry (***) (
			id &&& id $ (> Component.Piece.nPiecesPerSide)
		) . countPiecesByLogicalColour,
		"there are too many pieces of at least one logical colour."
	)
 ]

