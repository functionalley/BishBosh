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

module BishBosh.Test.HUnit.Component.Piece(
-- * Constants
	testCases
) where

import			Control.Arrow((&&&))
import			Data.Map((!))
import qualified	BishBosh.Attribute.Direction			as Attribute.Direction
import qualified	BishBosh.Attribute.LogicalColour		as Attribute.LogicalColour
import qualified	BishBosh.Attribute.Rank				as Attribute.Rank
import qualified	BishBosh.Cartesian.Vector			as Cartesian.Vector
import qualified	BishBosh.Component.Move				as Component.Move
import qualified	BishBosh.Component.Piece			as Component.Piece
import qualified	BishBosh.Component.QualifiedMove		as Component.QualifiedMove
import qualified	BishBosh.Notation.Smith				as Notation.Smith
import qualified	BishBosh.Test.HUnit.Cartesian.Coordinates	as Test.HUnit.Cartesian.Coordinates
import qualified	Data.List
import qualified	Test.HUnit
import			Test.HUnit((~?), (~?=), (~:))

-- | Check the sanity of the implementation, by validating a list of static test-cases.
testCases :: Test.HUnit.Test
testCases	= Test.HUnit.test [
	Component.Piece.range == Data.List.sort Component.Piece.range ~? "BishBosh.Component.Piece.range is misordered.",
	all (
		== Data.List.sort (Component.Piece.attackDirectionsByPiece ! Component.Piece.mkKing Attribute.LogicalColour.Black)
	) [
		Data.List.sort $ Component.Piece.attackDirectionsByPiece ! Component.Piece.mkQueen Attribute.LogicalColour.Black,
		Data.List.sort $ (
			Component.Piece.attackDirectionsByPiece ! Component.Piece.mkBishop Attribute.LogicalColour.Black
		) ++ (
			Component.Piece.attackDirectionsByPiece ! Component.Piece.mkRook Attribute.LogicalColour.Black
		)
	] ~? "'BishBosh.Component.Piece.attackDirectionsByPiece' failed to show Queen's moves to be union of Bishop & Rook.",
	"'BishBosh.Cartesian.Vector.attackVectorsForKing'." ~: (
		(
			length	:: [Cartesian.Vector.VectorInt] -> Int
		) Cartesian.Vector.attackVectorsForKing ~?= length (Component.Piece.attackDirectionsByPiece ! Component.Piece.mkKing Attribute.LogicalColour.Black)
	),
	"'BishBosh.Component.Piece.attackDirectionsByPiece' failed for King." ~: Data.List.sort (
		Component.Piece.attackDirectionsByPiece ! Component.Piece.mkKing Attribute.LogicalColour.Black {-arbitrarily-}
	) ~?= Data.List.sort Attribute.Direction.range,
	all (
		\((s, logicalColour), rank) -> let
			source, destination :: Test.HUnit.Cartesian.Coordinates.Coordinates
			(source, destination)	= (Component.Move.getSource &&& Component.Move.getDestination) . Component.QualifiedMove.getMove . Notation.Smith.getQualifiedMove $ read s
		in Component.Piece.canAttackAlong source destination $ Component.Piece.mkPiece logicalColour rank
	) (
		concat [
			[
				(
					"a2b3",	Attribute.LogicalColour.White
				), (
					"b3a4",	Attribute.LogicalColour.White
				), (
					"a7b6",	Attribute.LogicalColour.Black
				), (
					"b6a5",	Attribute.LogicalColour.Black
				)
			] `zip` repeat Attribute.Rank.Pawn,
			(
				[
					"a1c1",
					"h1f1"
				] `zip` repeat undefined
			) `zip` repeat Attribute.Rank.Rook,
			(
				[
					"a1h8",
					"h1a8",
					"h8a1",
					"a8h1"
				] `zip` repeat undefined
			) `zip` repeat Attribute.Rank.Bishop,
			[
				((source ++ destination, undefined), Attribute.Rank.Queen) |
					abscissaSource		<- "ah",
					ordinateSource		<- "18",
					let source	= [abscissaSource, ordinateSource],
					abscissaDestination	<- "ah",
					ordinateDestination	<- "18",
					let destination	= [abscissaDestination, ordinateDestination],
					source /= destination
			] {-list-comprehension-},
			[
				((source ++ destination, undefined), Attribute.Rank.King) |
					abscissaSource		<- "ab",
					ordinateSource		<- "12",
					let source	= [abscissaSource, ordinateSource],
					abscissaDestination	<- "ab",
					ordinateDestination	<- "12",
					let destination	= [abscissaDestination, ordinateDestination],
					source /= destination
			] -- List-comprehension.
		]
	) ~? "'BishBosh.Component.Piece.canAttackAlong' failed.",
	not (
		any (
			\((s, logicalColour), rank) -> let
				source, destination :: Test.HUnit.Cartesian.Coordinates.Coordinates
				(source, destination)	= (Component.Move.getSource &&& Component.Move.getDestination) . Component.QualifiedMove.getMove . Notation.Smith.getQualifiedMove $ read s
			in Component.Piece.canAttackAlong source destination $ Component.Piece.mkPiece logicalColour rank
		) $ concat [
			[
				(
					"a2a3",	Attribute.LogicalColour.White
				), (
					"a2a4",	Attribute.LogicalColour.White
				), (
					"a7a6",	Attribute.LogicalColour.Black
				), (
					"a7a5",	Attribute.LogicalColour.Black
				)
			] `zip` repeat Attribute.Rank.Pawn,
			(
				[
					"b1b2",
					"b1c2",
					"b1c1"
				] `zip` repeat undefined
			) `zip` repeat Attribute.Rank.Knight,
			(
				[
					"a1b2",
					"b2a1",
					"b2a1",
					"a1b2"
				] `zip` repeat undefined
			) `zip` repeat Attribute.Rank.Rook,
			(
				[
					"a1b1",
					"a1a2"
				] `zip` repeat undefined
			) `zip` repeat Attribute.Rank.Bishop,
			[
				((source ++ destination, undefined), Attribute.Rank.King) |
					source		<- ["a1", "a3", "c1", "c3"],
					destination	<- ["a1", "a3", "c1", "c3"],
					source /= destination
			] -- List-comprehension.
		]
	) ~? "'BishBosh.Component.Piece.canAttackAlong' validated an invalid attack."
 ]

