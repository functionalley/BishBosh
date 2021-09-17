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

 [@DESCRIPTION@]	Static tests.
-}

module BishBosh.Test.HUnit.State.Board(
-- * Types
-- ** Type-synonyms
	Board,
-- * Constants
	testCases
) where

import			Control.Arrow((***))
import			Data.Array.IArray((!))
import qualified	BishBosh.Attribute.LogicalColour			as Attribute.LogicalColour
import qualified	BishBosh.Attribute.Rank					as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa				as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates				as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate				as Cartesian.Ordinate
import qualified	BishBosh.Cartesian.Vector				as Cartesian.Vector
import qualified	BishBosh.Component.Move					as Component.Move
import qualified	BishBosh.Component.Piece				as Component.Piece
import qualified	BishBosh.Property.FixedMembership			as Property.FixedMembership
import qualified	BishBosh.Property.Opposable				as Property.Opposable
import qualified	BishBosh.State.Board					as State.Board
import qualified	BishBosh.State.CoordinatesByRankByLogicalColour		as State.CoordinatesByRankByLogicalColour
import qualified	BishBosh.State.MaybePieceByCoordinates			as State.MaybePieceByCoordinates
import qualified	BishBosh.StateProperty.Mutator				as StateProperty.Mutator
import qualified	BishBosh.Test.HUnit.Cartesian.Coordinates		as Test.HUnit.Cartesian.Coordinates
import qualified	BishBosh.Type.Length					as Type.Length
import qualified	Control.Arrow
import qualified	Data.Array.IArray
import qualified	Data.Default
import qualified	Data.Foldable
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Test.HUnit
import qualified	ToolShed.Data.Foldable
import			Test.HUnit((~:), (~?=), (~?))

-- | Defines a concrete type for testing.
type Board	= State.Board.Board Type.Length.X Type.Length.Y

-- | Check the sanity of the implementation, by validating a list of static test-cases.
testCases :: Test.HUnit.Test
testCases	= Test.HUnit.test [
	"'BishBosh.Cartesian.Coordinates' failed to locate the expected pieces on a default board." ~: (
		map length . ToolShed.Data.Foldable.gather $ map (
			`State.MaybePieceByCoordinates.dereference` Data.Default.def
		) (Property.FixedMembership.members :: [Test.HUnit.Cartesian.Coordinates.Coordinates])
	) ~?= [32, 8, 2, 2, 2, 1, 1, 8, 2, 2, 2, 1, 1],
	let
		kingsColour		= Attribute.LogicalColour.Black
		destination		= Cartesian.Coordinates.mkRelativeCoordinates ((+ 3) *** (+ 3))
		directionToCoordinates	= last . (`Cartesian.Coordinates.extrapolate` destination)
		mkPiece			= Component.Piece.mkPiece $ Property.Opposable.getOpposite kingsColour

		maybeShift :: Cartesian.Vector.VectorInt -> Maybe Test.HUnit.Cartesian.Coordinates.Coordinates
		maybeShift	= Cartesian.Vector.maybeTranslate destination
	in all (
		State.Board.isKingChecked kingsColour . placePieces . (:) (Component.Piece.mkKing kingsColour, destination) . return {-to List-monad-}
	) (
		concat [
			Data.Maybe.mapMaybe (
				fmap ((,) (mkPiece Attribute.Rank.Pawn)) . maybeShift . Property.Opposable.getOpposite
			) . Cartesian.Vector.attackVectorsForPawn $ Property.Opposable.getOpposite kingsColour,
			let
				attacker	= mkPiece Attribute.Rank.Rook
			in map ((,) attacker . directionToCoordinates) $ Component.Piece.getAttackDirections attacker,
			let
				attacker	= mkPiece Attribute.Rank.Bishop
			in map ((,) attacker . directionToCoordinates) $ Component.Piece.getAttackDirections attacker,
			Data.Maybe.mapMaybe (fmap ((,) (mkPiece Attribute.Rank.Knight)) . maybeShift) Cartesian.Vector.attackVectorsForKnight,
			let
				attacker	= mkPiece Attribute.Rank.Queen
			in map ((,) attacker . directionToCoordinates) $ Component.Piece.getAttackDirections attacker
		]
	) ~? "'BishBosh.State.Board.isKingChecked' failed.",
	"'BishBosh.State.CoordinatesByRankByLogicalColour.countPassedPawns' failed, for passed Pawn adjacent to opposing Pawn of equal rank." ~: Data.Array.IArray.elems (
		Data.Array.IArray.amap length . State.CoordinatesByRankByLogicalColour.findPassedPawnCoordinatesByLogicalColour . State.Board.getCoordinatesByRankByLogicalColour . placePieces $ map (
			Component.Piece.mkPawn *** Cartesian.Coordinates.mkRelativeCoordinates
		) [
			(
				Attribute.LogicalColour.Black,
				id
			), (
				Attribute.LogicalColour.White,
				Control.Arrow.first succ
			)
		]
	) ~?= [1, 1],
	"'BishBosh.State.CoordinatesByRankByLogicalColour.countPassedPawns' failed for passed Pawns isolated from opposing Pawn." ~: Data.Array.IArray.elems (
		Data.Array.IArray.amap length . State.CoordinatesByRankByLogicalColour.findPassedPawnCoordinatesByLogicalColour . State.Board.getCoordinatesByRankByLogicalColour . placePieces $ map (
			Component.Piece.mkPawn *** Cartesian.Coordinates.mkRelativeCoordinates
		) [
			(
				Attribute.LogicalColour.Black,
				Control.Arrow.second succ
			), (
				Attribute.LogicalColour.White,
				Control.Arrow.first (+ 2)
			)
		]
	) ~?= [1, 1],
	"'BishBosh.State.CoordinatesByRankByLogicalColour.countPassedPawns' failed for un-passed Pawn approaching opposing Pawn." ~: Data.Array.IArray.elems (
		 Data.Array.IArray.amap length . State.CoordinatesByRankByLogicalColour.findPassedPawnCoordinatesByLogicalColour . State.Board.getCoordinatesByRankByLogicalColour . placePieces $ map (
			Component.Piece.mkPawn *** Cartesian.Coordinates.mkRelativeCoordinates
		) [
			(
				Attribute.LogicalColour.White,
				id
			), (
				Attribute.LogicalColour.Black,
				succ *** succ
			)
		]
	) ~?= [0, 0],
	"'BishBosh.State.CoordinatesByRankByLogicalColour.countPassedPawns' failed for un-passed Pawn sandwiched by opposing Pawns." ~: Data.Array.IArray.elems (
		 Data.Array.IArray.amap length . State.CoordinatesByRankByLogicalColour.findPassedPawnCoordinatesByLogicalColour . State.Board.getCoordinatesByRankByLogicalColour . placePieces $ map (
			Component.Piece.mkPawn *** Cartesian.Coordinates.mkRelativeCoordinates
		) [
			(
				Attribute.LogicalColour.Black,
				id
			), (
				Attribute.LogicalColour.White,
				succ *** succ
			), (
				Attribute.LogicalColour.Black,
				(+ 2) *** (+ 2)
			)
		]
	) ~?= [1, 0],
	"'BishBosh.State.CoordinatesByRankByLogicalColour.countPassedPawns' failed for un-passed Pawn between doubled opposing Pawns." ~: Data.Array.IArray.elems (
		Data.Array.IArray.amap length . State.CoordinatesByRankByLogicalColour.findPassedPawnCoordinatesByLogicalColour . State.Board.getCoordinatesByRankByLogicalColour . placePieces $ map (
			Component.Piece.mkPawn *** Cartesian.Coordinates.mkRelativeCoordinates
		) [
			(
				Attribute.LogicalColour.Black,
				id
			), (
				Attribute.LogicalColour.Black,
				Control.Arrow.second (+ 2)
			), (
				Attribute.LogicalColour.White,
				succ *** succ
			)
		]
	) ~?= [1, 0],
	let
		whitePawnsCoordinates	= Cartesian.Coordinates.mkRelativeCoordinates $ succ *** succ
	in not (
		State.Board.exposesKing Attribute.LogicalColour.White (
			Component.Move.mkMove whitePawnsCoordinates $ Cartesian.Coordinates.advance Attribute.LogicalColour.White whitePawnsCoordinates
		) $ placePieces [
			(
				Component.Piece.mkKing Attribute.LogicalColour.White,
				minBound
			), (
				Component.Piece.mkPawn Attribute.LogicalColour.White,
				whitePawnsCoordinates
			), (
				Component.Piece.mkPawn Attribute.LogicalColour.Black,
				Cartesian.Coordinates.mkRelativeCoordinates $ (+ 2) *** (+ 2)
			)
		]
	) ~? "'BishBosh.State.Board.exposesKing false positive attack by black Pawn, after moving White Pawn.",
	let
		whitePawnsCoordinates	= Cartesian.Coordinates.mkRelativeCoordinates $ succ *** succ
	in not (
		State.Board.exposesKing Attribute.LogicalColour.White (
			Component.Move.mkMove whitePawnsCoordinates $ Cartesian.Coordinates.advance Attribute.LogicalColour.White whitePawnsCoordinates
		) $ placePieces [
			(
				Component.Piece.mkKing Attribute.LogicalColour.White,
				minBound
			), (
				Component.Piece.mkPawn Attribute.LogicalColour.White,
				whitePawnsCoordinates
			), (
				Component.Piece.mkKing Attribute.LogicalColour.Black,
				Cartesian.Coordinates.mkRelativeCoordinates $ (+ 2) *** (+ 2)
			)
		]
	) ~? "'BishBosh.State.Board.exposesKing false positive attack by black King, after moving White Pawn.",
	let
		whiteBishopsCoordinates	= Cartesian.Coordinates.mkRelativeCoordinates $ succ *** succ
	in not (
		State.Board.exposesKing Attribute.LogicalColour.White (
			Component.Move.mkMove whiteBishopsCoordinates $ Cartesian.Coordinates.translate (succ *** succ) whiteBishopsCoordinates	-- Move towards Black Queen.
		) $ placePieces [
			(
				Component.Piece.mkKing Attribute.LogicalColour.White,
				minBound
			), (
				Component.Piece.mkBishop Attribute.LogicalColour.White,
				whiteBishopsCoordinates
			), (
				Component.Piece.mkQueen Attribute.LogicalColour.Black,
				maxBound
			)
		]
	) ~? "'BishBosh.State.Board.exposesKing false positive attack by black Queen, after moving White Bishop.",
	let
		whitePawnsCoordinates	= Cartesian.Coordinates.mkRelativeCoordinates $ succ *** succ
	in State.Board.exposesKing Attribute.LogicalColour.White (
		Component.Move.mkMove whitePawnsCoordinates $ Cartesian.Coordinates.advance Attribute.LogicalColour.White whitePawnsCoordinates	-- Expose attack from Black Queen.
	) (
		placePieces [
			(
				Component.Piece.mkKing Attribute.LogicalColour.White,
				minBound
			), (
				Component.Piece.mkPawn Attribute.LogicalColour.White,
				whitePawnsCoordinates
			), (
				Component.Piece.mkQueen Attribute.LogicalColour.Black,
				maxBound
			)
		]
	) ~? "'BishBosh.State.Board.exposesKing failed after advancing White Pawn.",
	let
		whiteKnightsCoordinates	= Cartesian.Coordinates.mkRelativeCoordinates $ succ *** succ
	in State.Board.exposesKing Attribute.LogicalColour.White (
		Component.Move.mkMove whiteKnightsCoordinates . Cartesian.Coordinates.mkRelativeCoordinates $ (+ 3) *** (+ 2)	-- Expose attack from Black Queen.
	) (
		placePieces [
			(
				Component.Piece.mkKing Attribute.LogicalColour.White,
				minBound
			), (
				Component.Piece.mkKnight Attribute.LogicalColour.White,
				whiteKnightsCoordinates
			), (
				Component.Piece.mkQueen Attribute.LogicalColour.Black,
				maxBound
			)
		]
	) ~? "'BishBosh.State.Board.exposesKing failed after moving White Knight.",
	let
		whiteRooksCoordinates	= Cartesian.Coordinates.mkRelativeCoordinates $ (+ 2) *** (+ 2)
	in not (
		State.Board.exposesKing Attribute.LogicalColour.White (
			Component.Move.mkMove whiteRooksCoordinates $ Cartesian.Coordinates.advance Attribute.LogicalColour.White whiteRooksCoordinates	-- Shift blocking-role to Pawn.
		) $ placePieces [
			(
				Component.Piece.mkKing Attribute.LogicalColour.White,
				minBound
			), (
				Component.Piece.mkPawn Attribute.LogicalColour.White,
				Cartesian.Coordinates.mkRelativeCoordinates $ succ *** succ
			), (
				Component.Piece.mkRook Attribute.LogicalColour.White,
				whiteRooksCoordinates
			), (
				Component.Piece.mkQueen Attribute.LogicalColour.Black,
				maxBound
			)
		]
	) ~? "'BishBosh.State.Board.exposesKing failed after moving White Rook.",
	Data.Foldable.all (
		Data.Foldable.all (== 1)
	) (
		State.CoordinatesByRankByLogicalColour.countPawnsByFileByLogicalColour $ State.Board.getCoordinatesByRankByLogicalColour (Data.Default.def :: Board)
	) ~? "'BishBosh.State.Board.countPawnsByFileByLogicalColour': failed for default board",
	(
		(== [(0, 3), (2, 2), (4, 1)]) . Data.Map.assocs . (! Attribute.LogicalColour.White) . State.CoordinatesByRankByLogicalColour.countPawnsByFileByLogicalColour . State.Board.getCoordinatesByRankByLogicalColour . placePieces $ map (
			(,) (Component.Piece.mkPawn Attribute.LogicalColour.White) . Cartesian.Coordinates.mkRelativeCoordinates
		) [
			Control.Arrow.second succ,
			Control.Arrow.second (+ 3),
			Control.Arrow.second (+ 5),
			(+ 2) *** (+ 2),
			(+ 2) *** (+ 4),
			(+ 4) *** succ
		]
	) ~? "'BishBosh.State.Board.countPawnsByFileByLogicalColour': failed",
	(
		Data.Map.unions (
			Data.Array.IArray.elems $ State.Board.countDefendersByCoordinatesByLogicalColour (Data.Default.def :: Board)
		) == foldr (
			Data.Map.delete . Cartesian.Coordinates.kingsStartingCoordinates
		) (
			Data.Map.fromList $ zip [
				Cartesian.Coordinates.mkCoordinates x y |
					y	<- [
						Cartesian.Ordinate.yMax,
						Cartesian.Ordinate.pawnsFirstRank Attribute.LogicalColour.Black,
						Cartesian.Ordinate.pawnsFirstRank Attribute.LogicalColour.White,
						Cartesian.Ordinate.yMin
					],
					x	<- Cartesian.Abscissa.xRange
			] [
				0, 1, 1, 1, 1, 1, 1, 0,	-- Black Pieces.
				1, 1, 1, 4, 4, 1, 1, 1,	-- Black Pawns.
				1, 1, 1, 4, 4, 1, 1, 1,	-- White Pawns.
				0, 1, 1, 1, 1, 1, 1, 0	-- White Pieces.
			]
		) Property.FixedMembership.members
	) ~? "'BoshBosh.State.Board.countDefendersByCoordinatesByLogicalColour': failed"
 ] where
	placePieces :: [(Component.Piece.Piece, Test.HUnit.Cartesian.Coordinates.Coordinates)] -> Board
	placePieces	= StateProperty.Mutator.placeAllPieces

