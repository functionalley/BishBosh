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
-- * Constants
	testCases
) where

import			Control.Arrow((***))
import			Data.Array.IArray((!))
import qualified	BishBosh.Attribute.Rank				as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa			as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates			as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate			as Cartesian.Ordinate
import qualified	BishBosh.Cartesian.Vector			as Cartesian.Vector
import qualified	BishBosh.Component.Move				as Component.Move
import qualified	BishBosh.Component.Piece			as Component.Piece
import qualified	BishBosh.Property.FixedMembership		as Property.FixedMembership
import qualified	BishBosh.Property.Opposable			as Property.Opposable
import qualified	BishBosh.State.Board				as State.Board
import qualified	BishBosh.State.CoordinatesByRankByLogicalColour	as State.CoordinatesByRankByLogicalColour
import qualified	BishBosh.State.MaybePieceByCoordinates		as State.MaybePieceByCoordinates
import qualified	BishBosh.StateProperty.Mutator			as StateProperty.Mutator
import qualified	BishBosh.StateProperty.Seeker			as StateProperty.Seeker
import qualified	Control.Arrow
import qualified	Data.Array.IArray
import qualified	Data.Default
import qualified	Data.Foldable
import qualified	Data.Map.Strict					as Map
import qualified	Data.Maybe
import qualified	Test.HUnit
import qualified	ToolShed.Data.Foldable
import			Test.HUnit((~:), (~?=), (~?))

-- | Check the sanity of the implementation, by validating a list of static test-cases.
testCases :: Test.HUnit.Test
testCases	= Test.HUnit.test [
	"'BishBosh.Cartesian.Coordinates' failed to locate the expected pieces on a default board." ~: (
		map length . ToolShed.Data.Foldable.gather $ map (
			State.MaybePieceByCoordinates.dereference Data.Default.def
		) (Property.FixedMembership.members :: [Cartesian.Coordinates.Coordinates])
	) ~?= [32, 8, 2, 2, 2, 1, 1, 8, 2, 2, 2, 1, 1],
	let
		kingsColour		= minBound
		destination		= Cartesian.Coordinates.mkRelativeCoordinates ((+ 3) *** (+ 3))
		directionToCoordinates	= last . Cartesian.Coordinates.extrapolate destination
		mkPiece			= Component.Piece.mkPiece $ Property.Opposable.getOpposite kingsColour
		maybeShift		= (`Cartesian.Vector.maybeTranslate` destination)
	in all (
		(`State.Board.isKingChecked` kingsColour) . placePieces . (:) (Component.Piece.mkKing kingsColour, destination) . return {-to List-monad-}
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
	"'BishBosh.State.CoordinatesByRankByLogicalColour.countPassedPawns' failed for adjacent Pawns." ~: Data.Foldable.toList (
		Data.Array.IArray.amap length . State.CoordinatesByRankByLogicalColour.findPassedPawnCoordinatesByLogicalColour . State.Board.getCoordinatesByRankByLogicalColour . placePieces $ map (
			Component.Piece.mkPawn *** Cartesian.Coordinates.mkRelativeCoordinates
		) [
			(
				minBound,
				id
			), (
				maxBound,
				Control.Arrow.first succ
			)
		]
	) ~?= [1, 1],
	"'BishBosh.State.CoordinatesByRankByLogicalColour.countPassedPawns' failed for isolated Pawns." ~: Data.Foldable.toList (
		Data.Array.IArray.amap length . State.CoordinatesByRankByLogicalColour.findPassedPawnCoordinatesByLogicalColour . State.Board.getCoordinatesByRankByLogicalColour . placePieces $ map (
			Component.Piece.mkPawn *** Cartesian.Coordinates.mkRelativeCoordinates
		) [
			(
				minBound,
				Control.Arrow.second succ
			), (
				maxBound,
				Control.Arrow.first (+ 2)
			)
		]
	) ~?= [1, 1],
	"'BishBosh.State.CoordinatesByRankByLogicalColour.countPassedPawns' failed for doubled Pawns." ~: Data.Foldable.toList (
		Data.Array.IArray.amap length . State.CoordinatesByRankByLogicalColour.findPassedPawnCoordinatesByLogicalColour . State.Board.getCoordinatesByRankByLogicalColour . placePieces $ map (
			Component.Piece.mkPawn *** Cartesian.Coordinates.mkRelativeCoordinates
		) $ map ((,) minBound) [
			(+ 4) *** (+ 3),
			(+ 4) *** (+ 4)
		] ++ map ((,) maxBound) [
			(+ 5) *** (+ 4),
			(+ 5) *** (+ 5)
		]
	) ~?= [2, 2],
	"'BishBosh.State.CoordinatesByRankByLogicalColour.countPassedPawns' failed for asymmetric Pawns." ~: Data.Foldable.toList (
		Data.Array.IArray.amap length . State.CoordinatesByRankByLogicalColour.findPassedPawnCoordinatesByLogicalColour . State.Board.getCoordinatesByRankByLogicalColour . placePieces $ map (
			Component.Piece.mkPawn *** Cartesian.Coordinates.mkRelativeCoordinates
		) $ (minBound, (+ 4) *** (+ 4)) : map ((,) maxBound) [
			(+ 5) *** (+ 4),
			(+ 5) *** (+ 5)
		]
	) ~?= [1, 2],
	"'BishBosh.State.CoordinatesByRankByLogicalColour.countPassedPawns' failed for un-passed diagonally adjacent Pawns." ~: Data.Foldable.toList (
		 Data.Array.IArray.amap length . State.CoordinatesByRankByLogicalColour.findPassedPawnCoordinatesByLogicalColour . State.Board.getCoordinatesByRankByLogicalColour . placePieces $ map (
			Component.Piece.mkPawn *** Cartesian.Coordinates.mkRelativeCoordinates
		) [
			(
				maxBound,
				id
			), (
				minBound,
				succ *** succ
			)
		]
	) ~?= [0, 0],
	"'BishBosh.State.CoordinatesByRankByLogicalColour.countPassedPawns' failed for un-passed Pawn sandwiched by opposing Pawns." ~: Data.Foldable.toList (
		 Data.Array.IArray.amap length . State.CoordinatesByRankByLogicalColour.findPassedPawnCoordinatesByLogicalColour . State.Board.getCoordinatesByRankByLogicalColour . placePieces $ map (
			Component.Piece.mkPawn *** Cartesian.Coordinates.mkRelativeCoordinates
		) [
			(
				minBound,
				id
			), (
				maxBound,
				succ *** succ
			), (
				minBound,
				(+ 2) *** (+ 2)
			)
		]
	) ~?= [1, 0],
	"'BishBosh.State.CoordinatesByRankByLogicalColour.countPassedPawns' failed for un-passed Pawn between doubled opposing Pawns." ~: Data.Foldable.toList (
		Data.Array.IArray.amap length . State.CoordinatesByRankByLogicalColour.findPassedPawnCoordinatesByLogicalColour . State.Board.getCoordinatesByRankByLogicalColour . placePieces $ map (
			Component.Piece.mkPawn *** Cartesian.Coordinates.mkRelativeCoordinates
		) [
			(
				minBound,
				id
			), (
				minBound,
				Control.Arrow.second (+ 2)
			), (
				maxBound,
				succ *** succ
			)
		]
	) ~?= [1, 0],
	let
		whitePawnsCoordinates	= Cartesian.Coordinates.mkRelativeCoordinates $ succ *** succ
	in not (
		State.Board.exposesKing (
			placePieces [
				(
					Component.Piece.mkKing maxBound,
					minBound
				), (
					Component.Piece.mkPawn maxBound,
					whitePawnsCoordinates
				), (
					Component.Piece.mkPawn minBound,
					Cartesian.Coordinates.mkRelativeCoordinates $ (+ 2) *** (+ 2)
				)
			]
		) maxBound . Component.Move.mkMove whitePawnsCoordinates $ Cartesian.Coordinates.advance maxBound whitePawnsCoordinates
	) ~? "'BishBosh.State.Board.exposesKing false positive attack by black Pawn, after moving White Pawn.",
	let
		whitePawnsCoordinates	= Cartesian.Coordinates.mkRelativeCoordinates $ succ *** succ
	in not (
		State.Board.exposesKing (
			placePieces [
				(
					Component.Piece.mkKing maxBound,
					minBound
				), (
					Component.Piece.mkPawn maxBound,
					whitePawnsCoordinates
				), (
					Component.Piece.mkKing minBound,
					Cartesian.Coordinates.mkRelativeCoordinates $ (+ 2) *** (+ 2)
				)
			]
		) maxBound . Component.Move.mkMove whitePawnsCoordinates $ Cartesian.Coordinates.advance maxBound whitePawnsCoordinates
	) ~? "'BishBosh.State.Board.exposesKing false positive attack by black King, after moving White Pawn.",
	let
		whiteBishopsCoordinates	= Cartesian.Coordinates.mkRelativeCoordinates $ succ *** succ
	in not (
		State.Board.exposesKing (
			placePieces [
				(
					Component.Piece.mkKing maxBound,
					minBound
				), (
					Component.Piece.mkBishop maxBound,
					whiteBishopsCoordinates
				), (
					Component.Piece.mkQueen minBound,
					maxBound
				)
			]
		) maxBound . Component.Move.mkMove whiteBishopsCoordinates $ Cartesian.Coordinates.translate (succ *** succ) whiteBishopsCoordinates	-- Move towards Black Queen.
	) ~? "'BishBosh.State.Board.exposesKing false positive attack by black Queen, after moving White Bishop.",
	let
		whitePawnsCoordinates	= Cartesian.Coordinates.mkRelativeCoordinates $ succ *** succ
	in (
		State.Board.exposesKing (
			placePieces [
				(
					Component.Piece.mkKing maxBound,
					minBound
				), (
					Component.Piece.mkPawn maxBound,
					whitePawnsCoordinates
				), (
					Component.Piece.mkQueen minBound,
					maxBound
				)
			]
		) maxBound . Component.Move.mkMove whitePawnsCoordinates $ Cartesian.Coordinates.advance maxBound whitePawnsCoordinates	-- Expose attack from Black Queen.
	) ~? "'BishBosh.State.Board.exposesKing failed after advancing White Pawn.",
	let
		whiteKnightsCoordinates	= Cartesian.Coordinates.mkRelativeCoordinates $ succ *** succ
	in (
		State.Board.exposesKing (
			placePieces [
				(
					Component.Piece.mkKing maxBound,
					minBound
				), (
					Component.Piece.mkKnight maxBound,
					whiteKnightsCoordinates
				), (
					Component.Piece.mkQueen minBound,
					maxBound
				)
			]
		) maxBound . Component.Move.mkMove whiteKnightsCoordinates . Cartesian.Coordinates.mkRelativeCoordinates $ (+ 3) *** (+ 2)	-- Expose attack from Black Queen.
	) ~? "'BishBosh.State.Board.exposesKing failed after moving White Knight.",
	let
		whiteRooksCoordinates	= Cartesian.Coordinates.mkRelativeCoordinates $ (+ 2) *** (+ 2)
	in not (
		State.Board.exposesKing (
			placePieces [
				(
					Component.Piece.mkKing maxBound,
					minBound
				), (
					Component.Piece.mkPawn maxBound,
					Cartesian.Coordinates.mkRelativeCoordinates $ succ *** succ
				), (
					Component.Piece.mkRook maxBound,
					whiteRooksCoordinates
				), (
					Component.Piece.mkQueen minBound,
					maxBound
				)
			]
		) maxBound . Component.Move.mkMove whiteRooksCoordinates $ Cartesian.Coordinates.advance maxBound whiteRooksCoordinates	-- Shift blocking-role to Pawn.
	) ~? "'BishBosh.State.Board.exposesKing failed after moving White Rook.",
	Data.Foldable.all (
		Data.Foldable.all (== 1)
	) (
		StateProperty.Seeker.countPawnsByFileByLogicalColour $ State.Board.getCoordinatesByRankByLogicalColour (Data.Default.def :: State.Board.Board)
	) ~? "'BishBosh.State.Board.countPawnsByFileByLogicalColour': failed for default board",
	(
		(
			== map (Control.Arrow.first (Cartesian.Abscissa.xMin +)) [(0, 3), (2, 2), (4, 1)]
		) . Map.toList . (! maxBound) . StateProperty.Seeker.countPawnsByFileByLogicalColour . State.Board.getCoordinatesByRankByLogicalColour . placePieces $ map (
			(,) (Component.Piece.mkPawn maxBound) . Cartesian.Coordinates.mkRelativeCoordinates
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
		Map.unions (
			Data.Foldable.toList $ State.Board.countDefendersByCoordinatesByLogicalColour (Data.Default.def :: State.Board.Board)
		) == foldr (
			Map.delete . Cartesian.Coordinates.kingsStartingCoordinates
		) (
			Map.fromList $ zip [
				Cartesian.Coordinates.mkCoordinates x y |
					y	<- [
						Cartesian.Ordinate.yMax,
						Cartesian.Ordinate.pawnsFirstRank minBound,
						Cartesian.Ordinate.pawnsFirstRank maxBound,
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
	placePieces :: [(Component.Piece.Piece, Cartesian.Coordinates.Coordinates)] -> State.Board.Board
	placePieces	= StateProperty.Mutator.placeAllPieces

