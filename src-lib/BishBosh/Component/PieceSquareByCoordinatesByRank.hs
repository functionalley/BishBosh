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
	FindPieceSquareValue,
	FindPieceSquareValues,
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
	findPieceSquareValues,
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
import qualified	BishBosh.Types				as T
import qualified	Control.DeepSeq
import qualified	Data.Array.IArray
import qualified	Data.List

-- | The piece-square value may vary as the game progresses.
type PieceSquareValueByNPieces pieceSquareValue	= Data.Array.IArray.Array Type.Count.NPieces pieceSquareValue

-- | The bounds of the number of pieces on the board, at the end-game & opening-game respectively.
nPiecesBounds :: (Type.Count.NPieces, Type.Count.NPieces)
nPiecesBounds	= (
	3 {-minimum sufficient material-},
	fromIntegral Attribute.LogicalColour.nDistinctLogicalColours * Component.Piece.nPiecesPerSide
 )

-- | Self-documentation.
type EitherPieceSquareValueByNPiecesByCoordinates x y pieceSquareValue	= Either (
	Cartesian.Coordinates.ArrayByCoordinates x y pieceSquareValue	-- Uninterpolated.
 ) (
	Cartesian.Coordinates.ArrayByCoordinates x y (PieceSquareValueByNPieces pieceSquareValue)	-- Interpolated.
 )

-- | The value for each type of /piece/ of occupying each coordinate, at each stage in the lifetime of the game.
newtype PieceSquareByCoordinatesByRank x y pieceSquareValue	= MkPieceSquareByCoordinatesByRank {
	deconstruct	:: Attribute.Rank.ArrayByRank (EitherPieceSquareValueByNPiecesByCoordinates x y pieceSquareValue)
} deriving (Eq, Show)

instance (
	Control.DeepSeq.NFData	pieceSquareValue,
	Control.DeepSeq.NFData	x,
	Control.DeepSeq.NFData	y
 ) => Control.DeepSeq.NFData (PieceSquareByCoordinatesByRank x y pieceSquareValue) where
	rnf MkPieceSquareByCoordinatesByRank { deconstruct = byRank }	= Control.DeepSeq.rnf byRank

-- | Constructor.
mkPieceSquareByCoordinatesByRank
	:: (Attribute.Rank.Rank -> EitherPieceSquareValueByNPiecesByCoordinates x y pieceSquareValue)	-- ^ Convert a /rank/ into either (a /pieceSquareValue/ or a /pieceSquareValue/ which linearly varies with the number of /piece/s remaining) by /coordinates/.
	-> PieceSquareByCoordinatesByRank x y pieceSquareValue
mkPieceSquareByCoordinatesByRank	= MkPieceSquareByCoordinatesByRank . Attribute.Rank.listArrayByRank . (`map` Property.FixedMembership.members)

-- | The type of a function which can find the required piece-square value.
type FindPieceSquareValue x y pieceSquareValue
	= Attribute.LogicalColour.LogicalColour		-- ^ The /piece/'s /logical colour/.
	-> Attribute.Rank.Rank				-- ^ The /piece/'s /rank/.
	-> Cartesian.Coordinates.Coordinates x y	-- ^ The /piece/'s location.
	-> pieceSquareValue

-- | Find the piece-square value, at a stage in the game's lifetime defined by the total number of pieces remaining, for the specified /rank/ & /coordinates/.
findPieceSquareValue :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 )
	=> Type.Count.NPieces				-- ^ The progress through the game.
	-> Attribute.LogicalColour.LogicalColour	-- ^ The /piece/'s /logical colour/.
	-> Attribute.Rank.Rank				-- ^ The /piece/'s /rank/.
	-> Cartesian.Coordinates.Coordinates x y	-- ^ The /piece/'s location.
	-> PieceSquareByCoordinatesByRank x y pieceSquareValue
	-> pieceSquareValue
{-# SPECIALISE findPieceSquareValue
	:: Type.Count.NPieces
	-> Attribute.LogicalColour.LogicalColour
	-> Attribute.Rank.Rank
	-> Cartesian.Coordinates.Coordinates T.X T.Y
	-> PieceSquareByCoordinatesByRank T.X T.Y Type.Mass.PieceSquareValue
	-> Type.Mass.PieceSquareValue
 #-}
findPieceSquareValue nPieces logicalColour rank coordinates MkPieceSquareByCoordinatesByRank { deconstruct = byRank }	= (
	(!) ||| (
		\byNPiecesByCoordinates	-> (! nPieces) . (byNPiecesByCoordinates !)
	) $ byRank ! rank
 ) $ (
	if Attribute.LogicalColour.isBlack logicalColour
		then Property.Reflectable.reflectOnX
		else id
 ) coordinates

-- | The type of a function which can find the required piece-square values.
type FindPieceSquareValues x y pieceSquareValue
	= Attribute.LogicalColour.LogicalColour		-- ^ The /piece/'s /logical colour/.
	-> Attribute.Rank.Rank				-- ^ The /piece/'s /rank/.
	-> [Cartesian.Coordinates.Coordinates x y]	-- ^ The locations of interest for the /piece/.
	-> [pieceSquareValue]

-- | Find the piece-square values, at a stage in the game's lifetime defined by the total number of pieces remaining, for the specified /rank/ & list of /coordinates/.
findPieceSquareValues :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 )
	=> Type.Count.NPieces				-- ^ The progress through the game.
	-> Attribute.LogicalColour.LogicalColour	-- ^ The /piece/'s /logical colour/.
	-> Attribute.Rank.Rank				-- ^ The /piece/'s /rank/.
	-> [Cartesian.Coordinates.Coordinates x y]	-- ^ The locations of interest for the specified /piece/.
	-> PieceSquareByCoordinatesByRank x y pieceSquareValue
	-> [pieceSquareValue]
{-# SPECIALISE findPieceSquareValues
	:: Type.Count.NPieces
	-> Attribute.LogicalColour.LogicalColour
	-> Attribute.Rank.Rank
	-> [Cartesian.Coordinates.Coordinates T.X T.Y]
	-> PieceSquareByCoordinatesByRank T.X T.Y Type.Mass.PieceSquareValue
	-> [Type.Mass.PieceSquareValue]
 #-}
findPieceSquareValues nPieces logicalColour rank coordinatesList MkPieceSquareByCoordinatesByRank { deconstruct = byRank }	= (
	(!) ||| (
		\byNPiecesByCoordinates	-> (! nPieces) . (byNPiecesByCoordinates !)
	) $ byRank ! rank
 ) `map` (
	if Attribute.LogicalColour.isBlack logicalColour
		then map Property.Reflectable.reflectOnX
		else id
 ) coordinatesList

-- | Given the bounds over which two piece-square values vary as the game progresses from opening to end, return linearly interpolated values for all stages.
interpolatePieceSquareValues
	:: Fractional pieceSquareValue
	=> pieceSquareValue	-- ^ Opening-game.
	-> pieceSquareValue	-- ^ End-game.
	-> PieceSquareValueByNPieces pieceSquareValue
interpolatePieceSquareValues openingGame endGame	= Data.Array.IArray.listArray nPiecesBounds . map (
	uncurry (+) . (
		(* openingGame) &&& (* endGame) . (1 -)
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
formatForGNUPlot :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 )
	=> (pieceSquareValue -> ShowS)						-- ^ Format a /pieceSquareValue/.
	-> ShowS								-- ^ The column-delimiter.
	-> (PieceSquareValueByNPieces pieceSquareValue -> pieceSquareValue)	-- ^ Select one /pieceSquareValue/ from interpolated values.
	-> PieceSquareByCoordinatesByRank x y pieceSquareValue
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
	Property.FixedMembership.members	:: [Cartesian.Coordinates.Coordinates T.X T.Y]
 ) . Data.List.transpose . map (
	Data.Array.IArray.elems ||| map selector {-select one pieceSquareValue from interpolated values-} . Data.Array.IArray.elems {-ByCoordinates-}
 ) $ Data.Array.IArray.elems {-ByRank-} byRank where
	terminateRow	= showChar '\n'
	showsRow	= Text.ShowList.showsDelimitedList columnDelimiter id terminateRow

