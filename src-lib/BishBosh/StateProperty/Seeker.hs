{-# LANGUAGE MultiParamTypeClasses #-}
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

 [@DESCRIPTION@]	Permits discovery within a board.
-}

module BishBosh.StateProperty.Seeker(
-- * Types
-- ** Type-synonyms
--	NPiecesByFile,
	NPiecesByFileByLogicalColour,
-- * Type-classes
	Seeker(..),
-- * Functions
	accumulatePawnsByFile,
	findAllPieces,
	summariseNPawnsByLogicalColour,
	findInvalidity
) where

import			Control.Arrow((&&&), (***))
import			Data.Array.IArray((!))
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate		as Cartesian.Ordinate
import qualified	BishBosh.Colour.LogicalColour		as Colour.LogicalColour
import qualified	BishBosh.Component.Piece		as Component.Piece
import qualified	BishBosh.Property.Empty			as Property.Empty
import qualified	BishBosh.Property.SelfValidating	as Property.SelfValidating
import qualified	BishBosh.Type.Count			as Type.Count
import qualified	BishBosh.Type.Length			as Type.Length
import qualified	Control.Arrow
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.Map.Strict				as Map

-- | The number of /piece/s in each file, for each /logical colour/.
type NPiecesByFile	= Map.Map Type.Length.X Type.Count.NPieces

-- | Add a Pawn's file to the map.
accumulatePawnsByFile :: Type.Length.X -> NPiecesByFile -> NPiecesByFile
{-# INLINE accumulatePawnsByFile #-}
accumulatePawnsByFile	= flip (Map.insertWith $ const succ) 1

-- | The number of /piece/s in each file, for each /logical colour/.
type NPiecesByFileByLogicalColour	= Colour.LogicalColour.ArrayByLogicalColour NPiecesByFile

-- | An interface which may be implemented by data which can search the board.
class Seeker seeker where
	-- | Locate any @Knight@s capable of taking a /piece/ at the specified /coordinates/.
	findProximateKnights
		:: Colour.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the @Knight@ for which to search.
		-> Cartesian.Coordinates.Coordinates	-- ^ The destination to which the @Knight@ is required to be capable of jumping.
		-> seeker
		-> [Cartesian.Coordinates.Coordinates]

	-- | Locate any /piece/s satisfying the specified predicate.
	findPieces
		:: (Component.Piece.Piece -> Bool)	-- ^ Predicate.
		-> seeker
		-> [Component.Piece.LocatedPiece]

	{- |
		* Counts the number of @Pawn@s of each /logical colour/ with similar /x/-coordinates; their /y/-coordinate is irrelevant.

		* N.B.: files lacking any @Pawn@, don't feature in the results.
	-}
	countPawnsByFileByLogicalColour :: seeker -> NPiecesByFileByLogicalColour
	countPawnsByFileByLogicalColour	= (
		\(mB, mW) -> Colour.LogicalColour.listArrayByLogicalColour [mB, mW]
	 ) . foldr (
		(
			\(x, isBlack) -> (
				if isBlack then Control.Arrow.first else Control.Arrow.second	-- Select the appropriate map.
			) $ accumulatePawnsByFile x
		) . (
			Cartesian.Coordinates.getX *** Colour.LogicalColour.isBlack . Component.Piece.getLogicalColour
		)
	 ) Property.Empty.empty . findPieces Component.Piece.isPawn

-- | Locate all /piece/s on the board.
findAllPieces :: Seeker seeker => seeker -> [Component.Piece.LocatedPiece]
findAllPieces	= findPieces $ const True

-- | Resolves 'NPiecesByFileByLogicalColour' into the total number of /Pawn/s on either side.
summariseNPawnsByLogicalColour :: Seeker seeker => seeker -> Colour.LogicalColour.ArrayByLogicalColour Type.Count.NPieces
summariseNPawnsByLogicalColour	= Data.Array.IArray.amap (
	Data.Foldable.foldl' (+) 0
 ) . countPawnsByFileByLogicalColour

-- | Self-validate.
findInvalidity :: Seeker seeker => seeker -> [String]
findInvalidity	= Property.SelfValidating.findErrors [
	(
		uncurry (||) . uncurry (***) (
			id &&& id $ (> Attribute.Rank.initialAllocationByRankPerSide ! Attribute.Rank.Pawn) . fromIntegral . length
		) . Data.List.partition (
			Colour.LogicalColour.isBlack . Component.Piece.getLogicalColour
		) . map snd {-piece-} . findPieces Component.Piece.isPawn,
		"there are too many Pawns of at least one logical colour."
	), (
		any (
			uncurry (||) . uncurry (&&&) (
				(==) *** (==) $ Cartesian.Ordinate.yBounds
			) . Cartesian.Coordinates.getY . fst {-coordinates-}
		) . findPieces Component.Piece.isPawn,
		"no Pawn can exist on either of the terminal ranks."
	)
 ]
