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
-- * Constants
	testCases
) where

import			Control.Arrow((&&&), (|||))
import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Component.Piece		as Component.Piece
import qualified	BishBosh.Data.Exception			as Data.Exception
import qualified	BishBosh.Evaluation.Fitness		as Evaluation.Fitness
import qualified	BishBosh.Model.Game			as Model.Game
import qualified	BishBosh.Notation.MoveNotation		as Notation.MoveNotation
import qualified	BishBosh.State.Board			as State.Board
import qualified	BishBosh.StateProperty.Mutator		as StateProperty.Mutator
import qualified	BishBosh.StateProperty.Seeker		as StateProperty.Seeker
import qualified	BishBosh.Test.HUnit.Model.Game		as Test.HUnit.Model.Game
import qualified	BishBosh.Test.HUnit.State.Board		as Test.HUnit.State.Board
import qualified	BishBosh.Text.ShowList			as Text.ShowList
import qualified	Control.Exception
import qualified	Data.Default
import qualified	Test.HUnit
import			Test.HUnit((~:), (~?=))

-- | Check the sanity of the implementation, by validating a list of static test-cases.
testCases :: Test.HUnit.Test
testCases	= Test.HUnit.test [
	"'BishBosh.Evaluation.Fitness.measureValueOfCastlingPotential' failed after moving King." ~: case Notation.MoveNotation.readsQualifiedMove Data.Default.def "e1e3" {-move King (illegally)-} of
		[(eitherQualifiedMove, "")]	-> Evaluation.Fitness.measureValueOfCastlingPotential (
			Model.Game.applyEitherQualifiedMove eitherQualifiedMove (
				Data.Default.def	:: Test.HUnit.Model.Game.Game
			)
		 ) ~?= negate 1
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
	) ~?= 1,
	"'BishBosh.Evaluation.Fitness.measureValueOfCastlingPotential' failed after moving White Queen's Rook." ~: case Notation.MoveNotation.readsQualifiedMove Data.Default.def "a1a3" {-move Queen's Rook (illegally)-} of
		[(eitherQualifiedMove, "")]	-> Evaluation.Fitness.measureValueOfCastlingPotential (
			Model.Game.applyEitherQualifiedMove eitherQualifiedMove (
				Data.Default.def	:: Test.HUnit.Model.Game.Game
			)
		 ) ~?= negate (
			fromRational $ recip 2	-- Moving Queen's Rook still permits castling on the King's side.
		 )
		_				-> Control.Exception.throw $ Data.Exception.mkParseFailure "BishBosh.Test.HUnit.Evaluation.Fitness.testCases:\tfailed to parse move.",
	"'BishBosh.Evaluation.Fitness.measureValueOfCastlingPotential' failed after taking Black Queen's Rook." ~: case Notation.MoveNotation.readsQualifiedMove Data.Default.def "a2a8" {-take Queen's Rook (illegally)-} of
		[(eitherQualifiedMove, "")]	-> Evaluation.Fitness.measureValueOfCastlingPotential (
			Model.Game.applyEitherQualifiedMove eitherQualifiedMove (
				Data.Default.def	:: Test.HUnit.Model.Game.Game
			)
		 ) ~?= fromRational (
			recip 2	-- Taking Queen's Rook still permits castling on the King's side.
		 )
		_				-> Control.Exception.throw $ Data.Exception.mkParseFailure "BishBosh.Test.HUnit.Evaluation.Fitness.testCases:\tfailed to parse move.",
	"'BishBosh.Evaluation.Fitness.measureValueOfCastlingPotential' failed after moving one Rook from either side." ~: (
		\(moveString, errorMessage)	-> Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Test.HUnit.Evaluation.Fitness.testCases:\tfailed for " . showString Component.Move.tag . Text.ShowList.showsAssociation . shows moveString . showString "; " $ showString errorMessage "."
	) ||| (
		\game -> Evaluation.Fitness.measureValueOfCastlingPotential game ~?= 0
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
	) ~?= 1,	-- White is the next player, so Black is assumed to have just moved to an optimal position.
	"'BishBosh.Evaluation.Fitness.measureValueOfIsolatedPawns' failed for four columns of two rows of Pawns." ~: Evaluation.Fitness.measureValueOfIsolatedPawns (
		Model.Game.fromBoard (
			read "4k3/pppppppp/8/8/P1P1P1P1/8/P1P1P1P1/4K3"	:: Test.HUnit.State.Board.Board
		)
	) ~?= 1,	-- White is the next player, so Black is assumed to have just moved to an optimal position.
	"'BishBosh.Evaluation.Fitness.measureValueOfIsolatedPawns' failed for two columns of two four of Pawns." ~: Evaluation.Fitness.measureValueOfIsolatedPawns (
		Model.Game.fromBoard (
			read "4k3/pppppppp/8/2P2P2/2P2P2/2P2P2/2P2P2/4K3"	:: Test.HUnit.State.Board.Board
		)
	) ~?= 1,	-- White is the next player, so Black is assumed to have just moved to an optimal position.
	"'BishBosh.Evaluation.Fitness.measureValueOfPassedPawns' failed." ~: Evaluation.Fitness.measureValueOfPassedPawns (
		Model.Game.fromBoard (
			read "4k3/8/8/8/8/8/pppppppp/4K3"	:: Test.HUnit.State.Board.Board
		)
	) ~?= 1,	-- White is the next player, so Black is assumed to have just moved to an optimal position.
	"'BishBosh.Evaluation.Fitness.measureValueOfPassedPawns' failed." ~: Evaluation.Fitness.measureValueOfPassedPawns (
		Model.Game.fromBoard (
			read "8/PPPPPPPP/8/8/8/8/8/k3K3"	:: Test.HUnit.State.Board.Board
		)
	) ~?= negate 1,	-- White is the next player, so Black is assumed to have just moved to the worst possible position.
	"'BishBosh.Evaluation.Fitness.measureValueOfDefence' failed for default board." ~: Evaluation.Fitness.measureValueOfDefence (
		Model.Game.fromBoard . uncurry (
			foldr $ \(coordinates, _) -> StateProperty.Mutator.removePiece coordinates
		) $ (
			id &&& StateProperty.Seeker.findPieces (
				not . uncurry (||) . (Component.Piece.isBlack &&& Component.Piece.isKing)
			) . State.Board.getCoordinatesByRankByLogicalColour
		) (
			Data.Default.def	:: Test.HUnit.State.Board.Board
		)
	) ~?= fromRational (
		19 / fromIntegral {-NPieces-} Evaluation.Fitness.maximumDefended
	),
	"'BishBosh.Evaluation.Fitness.measureValueOfDefence' failed after Pawn-advance." ~: case Notation.MoveNotation.readsQualifiedMove Data.Default.def "g2g3" {-advance King's Knight's Pawn-} of
		[(eitherQualifiedMove, "")]	-> Evaluation.Fitness.measureValueOfDefence (
			Model.Game.applyEitherQualifiedMove eitherQualifiedMove (
				Data.Default.def	:: Test.HUnit.Model.Game.Game
			)
		 ) ~?= fromRational (
			recip $ fromIntegral {-NPieces-} Evaluation.Fitness.maximumDefended
		 )
		_				-> Control.Exception.throw $ Data.Exception.mkParseFailure "BishBosh.Test.HUnit.Evaluation.Fitness.testCases:\tfailed to parse move.",
	"'BishBosh.Evaluation.Fitness.measureValueOfDefence' failed after Pawn advance & Knight move." ~: (
		\(moveString, errorMessage)	-> Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Test.HUnit.Evaluation.Fitness.testCases:\tfailed for " . showString Component.Move.tag . Text.ShowList.showsAssociation . shows moveString . showString "; " $ showString errorMessage "."
	) ||| (
		\game -> Evaluation.Fitness.measureValueOfDefence game ~?= fromRational (
			3 / fromIntegral {-NPieces-} Evaluation.Fitness.maximumDefended
		)
	) $ Test.HUnit.Model.Game.applyMoves [
		"g2g3",	-- Advance King's Knight's Pawn.
		"b8c6"	-- Advance Queen's Knight.
	],
	"'BishBosh.Evaluation.Fitness.measureValueOfDefence' failed." ~: Evaluation.Fitness.measureValueOfDefence (
		Model.Game.fromBoard (
			read "k7/8/2p5/8/4RQQB/4QQQN/4NQQK/4BQQR"	:: Test.HUnit.State.Board.Board
		)
	) ~?= negate 1	-- White is the next player.
 ]

