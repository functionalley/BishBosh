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

module BishBosh.Test.HUnit.Evaluation.Fitness(
-- * Types
-- ** Type-synonyms
--	CriterionValue,
-- * Constants
	testCases
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.CriterionValue		as Attribute.CriterionValue
import qualified	BishBosh.Attribute.LogicalColour		as Attribute.LogicalColour
import qualified	BishBosh.Cartesian.Coordinates			as Cartesian.Coordinates
import qualified	BishBosh.Component.Move				as Component.Move
import qualified	BishBosh.Component.Piece			as Component.Piece
import qualified	BishBosh.Data.Exception				as Data.Exception
import qualified	BishBosh.Evaluation.Fitness			as Evaluation.Fitness
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.Notation.MoveNotation			as Notation.MoveNotation
import qualified	BishBosh.State.Board				as State.Board
import qualified	BishBosh.State.CoordinatesByRankByLogicalColour	as State.CoordinatesByRankByLogicalColour
import qualified	BishBosh.Test.HUnit.Model.Game			as Test.HUnit.Model.Game
import qualified	BishBosh.Test.HUnit.State.Board			as Test.HUnit.State.Board
import qualified	BishBosh.Text.ShowList				as Text.ShowList
import qualified	BishBosh.Types					as T
import qualified	Control.Exception
import qualified	Data.Default
import qualified	Test.HUnit
import			Test.HUnit((~:), (~?=))

-- | A suitable concrete type.
type CriterionValue	= Attribute.CriterionValue.CriterionValue T.CriterionValue

-- | Check the sanity of the implementation, by validating a list of static test-cases.
testCases :: Test.HUnit.Test
testCases	= Test.HUnit.test [
	"'BishBosh.Evaluation.Fitness.measureValueOfCastlingPotential' failed after moving King." ~: case Notation.MoveNotation.readsQualifiedMove Data.Default.def "e1e3" {-move King (illegally)-} of
		[(eitherQualifiedMove, "")]	-> Evaluation.Fitness.measureValueOfCastlingPotential (
			Model.Game.applyEitherQualifiedMove eitherQualifiedMove (
				Data.Default.def	:: Test.HUnit.Model.Game.Game
			)
		 ) ~?= (
			minBound	:: CriterionValue
		 )
		_				-> Control.Exception.throw $ Data.Exception.mkParseFailure "BishBosh.Test.HUnit.Evaluation.Fitness.testCases:\tfailed to parse move.",
	"'BishBosh.Evaluation.Fitness.measureValueOfCastlingPotential' failed after moving Rooks." ~: Evaluation.Fitness.measureValueOfCastlingPotential (
		Model.Game.fromBoard . foldr (
			\sourceCoordinates -> State.Board.movePiece (
				Component.Move.mkMove sourceCoordinates $ Cartesian.Coordinates.translateY (
					+ 2	-- N.B. an illegal move.
				) sourceCoordinates	-- Construct two White moves.
			) Data.Default.def {-moveType-}
		) (
			Data.Default.def	:: Test.HUnit.State.Board.Board
		) $ Cartesian.Coordinates.rooksStartingCoordinates Attribute.LogicalColour.White
	) ~?= (
		maxBound	:: CriterionValue
	),
	"'BishBosh.Evaluation.Fitness.measureValueOfCastlingPotential' failed after moving White Queen's Rook." ~: case Notation.MoveNotation.readsQualifiedMove Data.Default.def "a1a3" {-move Queen's Rook (illegally)-} of
		[(eitherQualifiedMove, "")]	-> Evaluation.Fitness.measureValueOfCastlingPotential (
			Model.Game.applyEitherQualifiedMove eitherQualifiedMove (
				Data.Default.def	:: Test.HUnit.Model.Game.Game
			)
		 ) ~?= (
			Attribute.CriterionValue.mkCriterionValue . negate $ recip 2	:: CriterionValue	-- Moving Queen's Rook still permits castling on the King's side.
		 )
		_				-> Control.Exception.throw $ Data.Exception.mkParseFailure "BishBosh.Test.HUnit.Evaluation.Fitness.testCases:\tfailed to parse move.",
	"'BishBosh.Evaluation.Fitness.measureValueOfCastlingPotential' failed after taking Black Queen's Rook." ~: case Notation.MoveNotation.readsQualifiedMove Data.Default.def "a2a8" {-take Queen's Rook (illegally)-} of
		[(eitherQualifiedMove, "")]	-> Evaluation.Fitness.measureValueOfCastlingPotential (
			Model.Game.applyEitherQualifiedMove eitherQualifiedMove (
				Data.Default.def	:: Test.HUnit.Model.Game.Game
			)
		 ) ~?= (
			Attribute.CriterionValue.mkCriterionValue $ recip 2	:: CriterionValue	-- Taking Queen's Rook still permits castling on the King's side.
		 )
		_				-> Control.Exception.throw $ Data.Exception.mkParseFailure "BishBosh.Test.HUnit.Evaluation.Fitness.testCases:\tfailed to parse move.",
	"'BishBosh.Evaluation.Fitness.measureValueOfCastlingPotential' failed after moving one Rook from either side." ~: either (
		\(moveString, errorMessage)	-> Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Test.HUnit.Evaluation.Fitness.testCases:\tfailed for " . showString Component.Move.tag . Text.ShowList.showsAssociation . shows moveString . showString "; " $ showString errorMessage "."
	) (
		\game -> Evaluation.Fitness.measureValueOfCastlingPotential game ~?= (Attribute.CriterionValue.zero :: CriterionValue)
	) $ Test.HUnit.Model.Game.applyMoves [
		"a2a3",
		"g8f6",
		"a1a2",
		"h8g8"
	],
	"'BishBosh.Evaluation.Fitness.measureValueOfDoubledPawns' failed." ~: Evaluation.Fitness.measureValueOfDoubledPawns (
		Model.Game.fromBoard (
			read "4k3/p6p/8/3PP3/3PP3/3PP3/3PP3/4K3"	:: Test.HUnit.State.Board.Board
		)
	) ~?= (
		maxBound	:: CriterionValue	-- White is the next player, so Black is assumed to have just moved to an optimal position.
	),
	"'BishBosh.Evaluation.Fitness.measureValueOfIsolatedPawns' failed for four columns of two rows of Pawns." ~: Evaluation.Fitness.measureValueOfIsolatedPawns (
		Model.Game.fromBoard (
			read "4k3/pppppppp/8/8/P1P1P1P1/8/P1P1P1P1/4K3"	:: Test.HUnit.State.Board.Board
		)
	) ~?= (
		maxBound	:: CriterionValue	-- White is the next player, so Black is assumed to have just moved to an optimal position.
	),
	"'BishBosh.Evaluation.Fitness.measureValueOfIsolatedPawns' failed for two columns of two four of Pawns." ~: Evaluation.Fitness.measureValueOfIsolatedPawns (
		Model.Game.fromBoard (
			read "4k3/pppppppp/8/2P2P2/2P2P2/2P2P2/2P2P2/4K3"	:: Test.HUnit.State.Board.Board
		)
	) ~?= (
		maxBound	:: CriterionValue	-- White is the next player, so Black is assumed to have just moved to an optimal position.
	),
	"'BishBosh.Evaluation.Fitness.measureValueOfPassedPawns' failed." ~: Evaluation.Fitness.measureValueOfPassedPawns (
		Model.Game.fromBoard (
			read "4k3/8/8/8/8/8/pppppppp/4K3"	:: Test.HUnit.State.Board.Board
		)
	) ~?= (
		maxBound	:: CriterionValue	-- White is the next player, so Black is assumed to have just moved to an optimal position.
	),
	"'BishBosh.Evaluation.Fitness.measureValueOfPassedPawns' failed." ~: Evaluation.Fitness.measureValueOfPassedPawns (
		Model.Game.fromBoard (
			read "8/PPPPPPPP/8/8/8/8/8/k3K3"	:: Test.HUnit.State.Board.Board
		)
	) ~?= (
		minBound	:: CriterionValue	-- White is the next player, so Black is assumed to have just moved to the worst possible position.
	),
	"'BishBosh.Evaluation.Fitness.measureValueOfDefence' failed for default board." ~: Evaluation.Fitness.measureValueOfDefence (
		Model.Game.fromBoard . uncurry (
			foldr $ \(coordinates, _) -> State.Board.removePiece coordinates
		) $ (
			id &&& State.CoordinatesByRankByLogicalColour.findPieces (
				not . uncurry (||) . (Component.Piece.isBlack &&& Component.Piece.isKing)
			) . State.Board.getCoordinatesByRankByLogicalColour
		) (
			Data.Default.def	:: Test.HUnit.State.Board.Board
		)
	) ~?= Attribute.CriterionValue.mkCriterionValue (
		19 / fromIntegral Evaluation.Fitness.maximumDefended	:: T.CriterionValue
	),
	"'BishBosh.Evaluation.Fitness.measureValueOfDefence' failed after Pawn-advance." ~: case Notation.MoveNotation.readsQualifiedMove Data.Default.def "g2g3" {-advance King's Knight's Pawn-} of
		[(eitherQualifiedMove, "")]	-> Evaluation.Fitness.measureValueOfDefence (
			Model.Game.applyEitherQualifiedMove eitherQualifiedMove (
				Data.Default.def	:: Test.HUnit.Model.Game.Game
			)
		 ) ~?= Attribute.CriterionValue.mkCriterionValue (recip $ fromIntegral Evaluation.Fitness.maximumDefended :: T.CriterionValue)
		_				-> Control.Exception.throw $ Data.Exception.mkParseFailure "BishBosh.Test.HUnit.Evaluation.Fitness.testCases:\tfailed to parse move.",
	"'BishBosh.Evaluation.Fitness.measureValueOfDefence' failed after Pawn advance & Knight move." ~: either (
		\(moveString, errorMessage)	-> Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Test.HUnit.Evaluation.Fitness.testCases:\tfailed for " . showString Component.Move.tag . Text.ShowList.showsAssociation . shows moveString . showString "; " $ showString errorMessage "."
	) (
		\game -> Evaluation.Fitness.measureValueOfDefence game ~?= Attribute.CriterionValue.mkCriterionValue (3 / fromIntegral Evaluation.Fitness.maximumDefended :: T.CriterionValue)
	) $ Test.HUnit.Model.Game.applyMoves [
		"g2g3",	-- Advance King's Knight's Pawn.
		"b8c6"	-- Advance Queen's Knight.
	]
 ]
