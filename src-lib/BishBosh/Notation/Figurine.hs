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

 [@DESCRIPTION@]	Defines the translation between /piece/s & Unicode figurines; <https://en.wikipedia.org/wiki/Chess_symbols_in_Unicode>.
-}

module BishBosh.Notation.Figurine(
-- * Types
-- ** Type-synonyms
--	Figurine,
-- * Constants
--	figurinesByPiece,
--	piecesByFigurine,
-- * Functions
	toFigurine,
	fromFigurine
) where

import			Data.Array.IArray((!))
import qualified	BishBosh.Component.Piece	as Component.Piece
import qualified	Data.Array.IArray
import qualified	Data.Map.Strict
import qualified	Data.Tuple

-- | A Unicode character depicting a piece.
type Figurine	= Char

-- | The constant Unicode by which to depict each piece.
figurinesByPiece :: Component.Piece.ArrayByPiece Figurine
figurinesByPiece	= Component.Piece.listArrayByPiece "\x265F\x265C\x265E\x265D\x265B\x265A\x2659\x2656\x2658\x2657\x2655\x2654"

-- | Returns the Unicode-character for the specified piece.
toFigurine :: Component.Piece.Piece -> Figurine
toFigurine	= (figurinesByPiece !)

-- | The constant piece corresponding to each Unicode figurine.
piecesByFigurine :: Data.Map.Strict.Map Figurine Component.Piece.Piece
piecesByFigurine	= Data.Map.Strict.fromList . map Data.Tuple.swap $ Data.Array.IArray.assocs figurinesByPiece

-- | Returns any piece which corresponds to the specified Unicode character.
fromFigurine :: Figurine -> Maybe Component.Piece.Piece
fromFigurine	= (`Data.Map.Strict.lookup` piecesByFigurine)

