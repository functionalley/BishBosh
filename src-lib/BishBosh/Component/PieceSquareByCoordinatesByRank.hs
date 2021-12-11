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

 [@DESCRIPTION@]	Defines the relative value of a specific /rank/ of piece, occupying a specific square, at a each stage in the game.
-}

module BishBosh.Component.PieceSquareByCoordinatesByRank(
-- * Types
-- ** Type-synonyms
	PieceSquareValueByCoordinates,
--	PieceSquareValueByCoordinatesByEitherNPieces,
-- ** Data-types
	PieceSquareByCoordinatesByRank(),
-- * Constants
	gnuPlotComment,
-- * Functions
--	merge,
	findPieceSquareValue,
	formatForGNUPlot,
-- ** Constructor
	mkPieceSquareByCoordinatesByRank
) where

import			Control.Arrow((&&&), (|||))
import			Data.Array.IArray((!))
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa		as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Colour.LogicalColour		as Colour.LogicalColour
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Property.Reflectable		as Property.Reflectable
import qualified	BishBosh.Text.ShowList			as Text.ShowList
import qualified	BishBosh.Type.Count			as Type.Count
import qualified	BishBosh.Type.Mass			as Type.Mass
import qualified	Control.DeepSeq
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.List

-- | The piece-square value may vary with coordinates.
type PieceSquareValueByCoordinates	=
#if defined USE_UNBOXED && !(defined USE_NEWTYPE_WRAPPERS || defined USE_PRECISION)
#define UNBOX
	Cartesian.Coordinates.UArrayByCoordinates
#else
	Cartesian.Coordinates.ArrayByCoordinates
#endif
		Type.Mass.PieceSquareValue

-- | The piece-square value may vary with /coordinates/ & optionally also the number of pieces remaining.
type PieceSquareValueByCoordinatesByEitherNPieces	= Either PieceSquareValueByCoordinates (Data.Array.IArray.Array Type.Count.NPieces PieceSquareValueByCoordinates)

-- | The piece-square value for various /coordinates/, optionally the number of pieces remaining, & /rank/.
newtype PieceSquareByCoordinatesByRank	= MkPieceSquareByCoordinatesByRank (Attribute.Rank.ArrayByRank PieceSquareValueByCoordinatesByEitherNPieces) deriving (Eq, Show)

instance Control.DeepSeq.NFData PieceSquareByCoordinatesByRank where
	rnf (MkPieceSquareByCoordinatesByRank pieceSquareValueByCoordinatesByEitherNPiecesByRank)	=
#ifdef UNBOX
		Control.DeepSeq.liftRnf {-into ByRank-} (
			Control.DeepSeq.rwhnf ||| Control.DeepSeq.liftRnf {-into ByNPieces-} Control.DeepSeq.rwhnf
		)
#else
		Control.DeepSeq.rnf
#endif
			pieceSquareValueByCoordinatesByEitherNPiecesByRank

-- | Constructor.
mkPieceSquareByCoordinatesByRank :: (Attribute.Rank.Rank -> PieceSquareValueByCoordinatesByEitherNPieces) -> PieceSquareByCoordinatesByRank
mkPieceSquareByCoordinatesByRank	= MkPieceSquareByCoordinatesByRank . Attribute.Rank.listArrayByRank . (`map` Property.FixedMembership.members)

-- | Merge the /Left/ & /Right/ sides of 'PieceSquareValueByCoordinatesByEitherNPieces'.
merge :: Type.Count.NPieces -> PieceSquareValueByCoordinatesByEitherNPieces -> PieceSquareValueByCoordinates
merge nPieces	= id ||| (! nPieces)

-- | Find the piece-square value, at a stage in the game's life-cycle defined by the total number of pieces remaining, for the specified /rank/ & /coordinates/.
findPieceSquareValue
	:: PieceSquareByCoordinatesByRank
	-> Type.Count.NPieces			-- ^ A proxy for the progress through the game.
	-> Colour.LogicalColour.LogicalColour	-- ^ The /piece/'s /logical colour/.
	-> Attribute.Rank.Rank			-- ^ The /piece/'s /rank/.
	-> Cartesian.Coordinates.Coordinates	-- ^ The /piece/'s location.
	-> Type.Mass.PieceSquareValue
findPieceSquareValue (MkPieceSquareByCoordinatesByRank pieceSquareValueByCoordinatesByEitherNPiecesByRank) nPieces logicalColour rank	= (!) (
	merge nPieces $ pieceSquareValueByCoordinatesByEitherNPiecesByRank ! rank
 ) . if Colour.LogicalColour.isBlack logicalColour
	then Property.Reflectable.reflectOnX
	else id

-- | The character used in __GNUPlot__ to denote a comment.
gnuPlotComment :: Char
gnuPlotComment	= '#'

-- | Format the data for input to __GNUPlot__.
formatForGNUPlot
	:: PieceSquareByCoordinatesByRank
	-> (Type.Mass.PieceSquareValue -> ShowS)	-- ^ Format a /pieceSquareValue/.
	-> ShowS					-- ^ The column-delimiter.
	-> Type.Count.NPieces				-- ^ Select from interpolated values.
	-> ShowS
formatForGNUPlot (MkPieceSquareByCoordinatesByRank pieceSquareValueByCoordinatesByEitherNPiecesByRank) pieceSquareValueFormatter columnDelimiter nPieces	= (
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
	Data.Array.IArray.elems {-ByCoordinates-} . merge nPieces
 ) $ Data.Foldable.toList {-ByRank-} pieceSquareValueByCoordinatesByEitherNPiecesByRank where
	terminateRow	= showChar '\n'
	showsRow	= Text.ShowList.showsDelimitedList columnDelimiter id terminateRow

