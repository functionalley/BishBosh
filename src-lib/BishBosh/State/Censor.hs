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

module BishBosh.State.Censor(
-- * Types
-- ** Type-synonyms
	NPiecesByRank,
-- * Type-classes
	Censor(..)
) where

import qualified	BishBosh.Attribute.Rank		as Attribute.Rank
import qualified	BishBosh.Component.Piece	as Component.Piece

-- | The difference in the number of /piece/s of each /rank/ held by either side.
type NPiecesByRank	= Attribute.Rank.ByRank Component.Piece.NPieces

-- | An interface which may be implemented by data which can perform a census of the /piece/s on the /board/.
class Censor censor where
	countPiecesByLogicalColour	:: censor -> (Component.Piece.NPieces, Component.Piece.NPieces)	-- ^ The total number of /piece/s, partitioned into @Black@ & @White@.

	countPieces			:: censor -> Component.Piece.NPieces				-- ^ The total number of /piece/s on the board.
	countPieces	= uncurry (+) . countPiecesByLogicalColour	-- Default implementation.

	countPieceDifferenceByRank	:: censor -> NPiecesByRank					-- ^ Finds the difference between the number of /piece/s of each /rank/ held by each side. N.B. for this purpose, @White@ is arbitrarily considered positive & @Black@ negative.

	hasInsufficientMaterial		:: censor -> Bool						-- ^ Whether insufficient material remains on the board, to force check-mate; <https://en.wikipedia.org/wiki/Draw_(chess)>.

	hasBothKings			:: censor -> Bool						-- ^ Whether there's exactly one @King@ of each /logical colour/.

