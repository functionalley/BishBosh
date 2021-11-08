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

 [@DESCRIPTION@]	Defines the relative value of a specific /rank/ of piece, occupying a specific /coordinate/ on the board, at a specific stage in the game.
-}

module BishBosh.Component.PieceSquareByCoordinatesByRank(
-- * Types
-- ** Type-synonyms
--	PieceSquareValueByNPieces,
--	EitherPieceSquareValueByNPiecesByCoordinates,
-- ** Data-types
	PieceSquareByCoordinatesByRank(
--		MkPieceSquareByCoordinatesByRank,
--		deconstruct
	),
-- * Constants
	nPiecesBounds,
	gnuPlotComment,
-- * Functions
	findPieceSquareValue,
	interpolatePieceSquareValues,
	formatForGNUPlot,
-- ** Constructor
	mkPieceSquareByCoordinatesByRank
) where

import			Control.Arrow((&&&), (|||))
import			Data.Array.IArray((!))
import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa		as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Component.Piece		as Component.Piece
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Property.Reflectable		as Property.Reflectable
import qualified	BishBosh.Text.ShowList			as Text.ShowList
import qualified	BishBosh.Type.Count			as Type.Count
import qualified	BishBosh.Type.Mass			as Type.Mass
import qualified	Control.DeepSeq
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.List

#if defined(USE_UNBOXED_ARRAYS) && !(defined(USE_NEWTYPE_WRAPPERS) || defined(USE_PRECISION))
#define UNBOX
import qualified	Data.Array.Unboxed
#endif

-- | The piece-square value may vary as the game progresses.
type PieceSquareValueByNPieces =
#ifdef UNBOX
	Data.Array.Unboxed.UArray
#else
	Data.Array.IArray.Array
#endif
		Type.Count.NPieces Type.Mass.PieceSquareValue

-- | The bounds of the number of pieces on the board, at the end-game & opening-game respectively.
nPiecesBounds :: (Type.Count.NPieces, Type.Count.NPieces)
nPiecesBounds	= (
	3 {-minimum sufficient material-},
	fromIntegral Attribute.LogicalColour.nDistinctLogicalColours * Component.Piece.nPiecesPerSide
 )

-- | Self-documentation.
type EitherPieceSquareValueByNPiecesByCoordinates	= Either (
#ifdef UNBOX
	Cartesian.Coordinates.UArrayByCoordinates
#else
	Cartesian.Coordinates.ArrayByCoordinates
#endif
		Type.Mass.PieceSquareValue	-- Uninterpolated.
 ) (
	Cartesian.Coordinates.ArrayByCoordinates PieceSquareValueByNPieces	-- Interpolated.
 )

-- | The value for each type of /piece/ of occupying each coordinate, at each stage in the lifetime of the game.
newtype PieceSquareByCoordinatesByRank	= MkPieceSquareByCoordinatesByRank {
	deconstruct	:: Attribute.Rank.ArrayByRank EitherPieceSquareValueByNPiecesByCoordinates
} deriving (Eq, Show)

instance Control.DeepSeq.NFData PieceSquareByCoordinatesByRank where
	rnf
#ifdef UNBOX
		_								= ()
#else
		MkPieceSquareByCoordinatesByRank { deconstruct = byRank }	= Control.DeepSeq.rnf byRank
#endif

-- | Constructor.
mkPieceSquareByCoordinatesByRank
	:: (Attribute.Rank.Rank -> EitherPieceSquareValueByNPiecesByCoordinates)	-- ^ Convert a /rank/ into either (a /pieceSquareValue/ or a /pieceSquareValue/ which linearly varies with the number of /piece/s remaining) by /coordinates/.
	-> PieceSquareByCoordinatesByRank
mkPieceSquareByCoordinatesByRank	= MkPieceSquareByCoordinatesByRank . Attribute.Rank.listArrayByRank . (`map` Property.FixedMembership.members)

-- | Find the piece-square value, at a stage in the game's lifetime defined by the total number of pieces remaining, for the specified /rank/ & /coordinates/.
findPieceSquareValue
	:: PieceSquareByCoordinatesByRank
	-> Type.Count.NPieces				-- ^ The progress through the game.
	-> Attribute.LogicalColour.LogicalColour	-- ^ The /piece/'s /logical colour/.
	-> Attribute.Rank.Rank				-- ^ The /piece/'s /rank/.
	-> Cartesian.Coordinates.Coordinates		-- ^ The /piece/'s location.
	-> Type.Mass.PieceSquareValue
findPieceSquareValue MkPieceSquareByCoordinatesByRank { deconstruct = byRank } nPieces logicalColour rank	= (
	(!) ||| (
		\byNPiecesByCoordinates	-> (! nPieces) . (byNPiecesByCoordinates !)
	) $ byRank ! rank
 ) . if Attribute.LogicalColour.isBlack logicalColour
	then Property.Reflectable.reflectOnX
	else id

-- | Given the bounds over which two piece-square values vary as the game progresses from opening to end, return linearly interpolated values for all stages.
interpolatePieceSquareValues
	:: Type.Mass.PieceSquareValue	-- ^ Opening-game.
	-> Type.Mass.PieceSquareValue	-- ^ End-game.
	-> PieceSquareValueByNPieces
interpolatePieceSquareValues openingGame endGame	= Data.Array.IArray.listArray nPiecesBounds . map (
	fromRational . uncurry (+) . (
		(* toRational openingGame) &&& (* toRational endGame) . (1 -)
	) . (
		/ fromIntegral (
			uncurry subtract nPiecesBounds	-- N.B.: this can't reasonably be zero.
		) -- map into the closed unit-interval [0,1].
	) . fromIntegral . subtract (
		fst {-minimum-} nPiecesBounds
	)
 ) $ uncurry enumFromTo nPiecesBounds

-- | The character used in __GNUPlot__ to denote a comment.
gnuPlotComment :: Char
gnuPlotComment	= '#'

-- | Format the data for input to __GNUPlot__.
formatForGNUPlot
	:: (Type.Mass.PieceSquareValue -> ShowS)			-- ^ Format a /pieceSquareValue/.
	-> ShowS							-- ^ The column-delimiter.
	-> (PieceSquareValueByNPieces -> Type.Mass.PieceSquareValue)	-- ^ Select one /PieceSquareValue/ from interpolated values.
	-> PieceSquareByCoordinatesByRank
	-> ShowS
formatForGNUPlot pieceSquareValueFormatter columnDelimiter selector MkPieceSquareByCoordinatesByRank { deconstruct = byRank }	= (
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
 ) id . zip (
	Property.FixedMembership.members	:: [Cartesian.Coordinates.Coordinates]
 ) . Data.List.transpose . map (
	Data.Array.IArray.elems ||| map selector {-select one PieceSquareValue from interpolated values-} . Data.Foldable.toList {-ByCoordinates-}
 ) $ Data.Foldable.toList {-ByRank-} byRank where
	terminateRow	= showChar '\n'
	showsRow	= Text.ShowList.showsDelimitedList columnDelimiter id terminateRow

