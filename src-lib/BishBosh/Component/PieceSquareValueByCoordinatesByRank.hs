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

 [@DESCRIPTION@]	Defines the relative value of a specific /rank/ of piece, occupying a specific square, at a each stage in the game's life-cycle.
-}

module BishBosh.Component.PieceSquareValueByCoordinatesByRank(
-- * Types
-- ** Type-synonyms
--	PieceSquareValueByCoordinatesByEitherNPieces,
-- ** Data-types
	PieceSquareValueByCoordinatesByRank(),
-- * Constants
	gnuPlotComment,
-- * Functions
--	unify,
	formatForGNUPlot,
-- ** Accessors
	getPieceSquareValueByCoordinates,
-- ** Constructor
	mkPieceSquareValueByCoordinatesByRank
) where

import			Control.Arrow((&&&), (|||))
import			Data.Array.IArray((!))
import qualified	BishBosh.Attribute.Rank					as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa				as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates				as Cartesian.Coordinates
import qualified	BishBosh.Component.PieceSquareValueByCoordinates	as Component.PieceSquareValueByCoordinates
import qualified	BishBosh.Property.FixedMembership			as Property.FixedMembership
import qualified	BishBosh.Text.ShowList					as Text.ShowList
import qualified	BishBosh.Type.Count					as Type.Count
import qualified	BishBosh.Type.Mass					as Type.Mass
import qualified	Control.DeepSeq
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.List

-- | The piece-square value may vary with /coordinates/ & optionally also the number of pieces remaining.
type PieceSquareValueByCoordinatesByEitherNPieces	= Either Component.PieceSquareValueByCoordinates.PieceSquareValueByCoordinates (
	Data.Array.IArray.Array Type.Count.NPieces Component.PieceSquareValueByCoordinates.PieceSquareValueByCoordinates
 )

-- | The piece-square value for various /coordinates/, optionally the number of pieces remaining, & /rank/.
newtype PieceSquareValueByCoordinatesByRank	= MkPieceSquareValueByCoordinatesByRank (Attribute.Rank.ArrayByRank PieceSquareValueByCoordinatesByEitherNPieces)

instance Control.DeepSeq.NFData PieceSquareValueByCoordinatesByRank where
	rnf (MkPieceSquareValueByCoordinatesByRank pieceSquareValueByCoordinatesByEitherNPiecesByRank)	= Control.DeepSeq.rnf pieceSquareValueByCoordinatesByEitherNPiecesByRank	-- Forward.

-- | Constructor.
mkPieceSquareValueByCoordinatesByRank :: (Attribute.Rank.Rank -> PieceSquareValueByCoordinatesByEitherNPieces) -> PieceSquareValueByCoordinatesByRank
mkPieceSquareValueByCoordinatesByRank	= MkPieceSquareValueByCoordinatesByRank . Attribute.Rank.listArrayByRank . (`map` Property.FixedMembership.members)

-- | Unify the /Left/ & /Right/ sides of 'PieceSquareValueByCoordinatesByEitherNPieces'.
unify :: Type.Count.NPieces -> PieceSquareValueByCoordinatesByEitherNPieces -> Component.PieceSquareValueByCoordinates.PieceSquareValueByCoordinates
unify nPieces	= id ||| (! nPieces)

-- | Retrieve the appropriate /PieceSquareValueByCoordinates/ for the specified /rank/, @ the current stage of the game's life-cycle.
getPieceSquareValueByCoordinates
	:: PieceSquareValueByCoordinatesByRank
	-> Type.Count.NPieces	-- ^ An inverse proxy for the age of the game.
	-> Attribute.Rank.Rank	-- ^ The /piece/'s /rank/.
	-> Component.PieceSquareValueByCoordinates.PieceSquareValueByCoordinates
getPieceSquareValueByCoordinates (MkPieceSquareValueByCoordinatesByRank pieceSquareValueByCoordinatesByEitherNPiecesByRank) nPieces rank	= unify nPieces $ pieceSquareValueByCoordinatesByEitherNPiecesByRank ! rank

-- | The character used in __GNUPlot__ to denote a comment.
gnuPlotComment :: Char
gnuPlotComment	= '#'

-- | Format the data for input to __GNUPlot__.
formatForGNUPlot
	:: PieceSquareValueByCoordinatesByRank
	-> (Type.Mass.PieceSquareValue -> ShowS)	-- ^ Format a /pieceSquareValue/.
	-> ShowS					-- ^ The column-delimiter.
	-> Type.Count.NPieces				-- ^ Select from interpolated values.
	-> ShowS
formatForGNUPlot (MkPieceSquareValueByCoordinatesByRank pieceSquareValueByCoordinatesByEitherNPiecesByRank) pieceSquareValueFormatter columnDelimiter nPieces	= (
	showsRow (
		showChar gnuPlotComment . showChar 'x' : showChar 'y' : map shows Attribute.Rank.range	-- Header comment.
	) .
 ) . foldr (
	\(coordinates, byRank') showS	-> let
		(x, y)	= Cartesian.Coordinates.getX &&& Cartesian.Coordinates.getY $ coordinates
	in showsRow (
		shows x : shows y : map pieceSquareValueFormatter byRank'
	) . (
		if x == Cartesian.Abscissa.xMax
			then terminateRow	-- Separate isolines.
			else id
	) . showS
 ) id . zip Property.FixedMembership.members . Data.List.transpose . map (
	Component.PieceSquareValueByCoordinates.toList . unify nPieces
 ) $ Data.Foldable.toList {-ByRank-} pieceSquareValueByCoordinatesByEitherNPiecesByRank where
	terminateRow	= showChar '\n'
	showsRow	= Text.ShowList.showsDelimitedList columnDelimiter id terminateRow

