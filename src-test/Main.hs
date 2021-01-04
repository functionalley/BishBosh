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

 [@DESCRIPTION@]	The entry-point to the application's test-suite.
-}

module Main(main) where

import qualified	BishBosh.Test.HUnit.Attribute.Direction						as Test.HUnit.Attribute.Direction
import qualified	BishBosh.Test.HUnit.Attribute.LogicalColour					as Test.HUnit.Attribute.LogicalColour
import qualified	BishBosh.Test.HUnit.Attribute.Rank						as Test.HUnit.Attribute.Rank
import qualified	BishBosh.Test.HUnit.Cartesian.Coordinates					as Test.HUnit.Cartesian.Coordinates
import qualified	BishBosh.Test.HUnit.Cartesian.Vector						as Test.HUnit.Cartesian.Vector
import qualified	BishBosh.Test.HUnit.Component.Move						as Test.HUnit.Component.Move
import qualified	BishBosh.Test.HUnit.Component.Piece						as Test.HUnit.Component.Piece
import qualified	BishBosh.Test.HUnit.Component.Zobrist						as Test.HUnit.Component.Zobrist
import qualified	BishBosh.Test.HUnit.ContextualNotation.PGN					as Test.HUnit.ContextualNotation.PGN
import qualified	BishBosh.Test.HUnit.ContextualNotation.PositionHashQualifiedMoveTree		as Test.HUnit.ContextualNotation.PositionHashQualifiedMoveTree
import qualified	BishBosh.Test.HUnit.ContextualNotation.StandardAlgebraic			as Test.HUnit.ContextualNotation.StandardAlgebraic
import qualified	BishBosh.Test.HUnit.Evaluation.Fitness						as Test.HUnit.Evaluation.Fitness
import qualified	BishBosh.Test.HUnit.Input.Options						as Test.HUnit.Input.Options
import qualified	BishBosh.Test.HUnit.Model.Game							as Test.HUnit.Model.Game
import qualified	BishBosh.Test.HUnit.Model.GameTree						as Test.HUnit.Model.GameTree
import qualified	BishBosh.Test.HUnit.Model.PositionHashTree					as Test.HUnit.Model.PositionHashTree
-- import qualified	BishBosh.Test.HUnit.Search.Search						as Test.HUnit.Search.Search
import qualified	BishBosh.Test.HUnit.State.Board							as Test.HUnit.State.Board
import qualified	BishBosh.Test.QuickCheck.Attribute.Direction					as Test.QuickCheck.Attribute.Direction
import qualified	BishBosh.Test.QuickCheck.Attribute.LogicalColour				as Test.QuickCheck.Attribute.LogicalColour
import qualified	BishBosh.Test.QuickCheck.Attribute.MoveType					as Test.QuickCheck.Attribute.MoveType
import qualified	BishBosh.Test.QuickCheck.Attribute.Rank						as Test.QuickCheck.Attribute.Rank
import qualified	BishBosh.Test.QuickCheck.Cartesian.Coordinates					as Test.QuickCheck.Cartesian.Coordinates
import qualified	BishBosh.Test.QuickCheck.Cartesian.Vector					as Test.QuickCheck.Cartesian.Vector
import qualified	BishBosh.Test.QuickCheck.Component.Move						as Test.QuickCheck.Component.Move
import qualified	BishBosh.Test.QuickCheck.Component.Piece					as Test.QuickCheck.Component.Piece
import qualified	BishBosh.Test.QuickCheck.Component.QualifiedMove				as Test.QuickCheck.Component.QualifiedMove
import qualified	BishBosh.Test.QuickCheck.Component.Turn						as Test.QuickCheck.Component.Turn
import qualified	BishBosh.Test.QuickCheck.Component.Zobrist					as Test.QuickCheck.Component.Zobrist
import qualified	BishBosh.Test.QuickCheck.ContextualNotation.PGN					as Test.QuickCheck.ContextualNotation.PGN
import qualified	BishBosh.Test.QuickCheck.ContextualNotation.PGNComment				as Test.QuickCheck.ContextualNotation.PGNComment
import qualified	BishBosh.Test.QuickCheck.ContextualNotation.PositionHashQualifiedMoveTree	as Test.QuickCheck.ContextualNotation.PositionHashQualifiedMoveTree
import qualified	BishBosh.Test.QuickCheck.ContextualNotation.QualifiedMoveForest			as Test.QuickCheck.ContextualNotation.QualifiedMoveForest
import qualified	BishBosh.Test.QuickCheck.ContextualNotation.StandardAlgebraic			as Test.QuickCheck.ContextualNotation.StandardAlgebraic
import qualified	BishBosh.Test.QuickCheck.Data.Integral						as Test.QuickCheck.Data.Integral
import qualified	BishBosh.Test.QuickCheck.Evaluation.PositionHashQuantifiedGameTree		as Test.QuickCheck.Evaluation.PositionHashQuantifiedGameTree
import qualified	BishBosh.Test.QuickCheck.Input.CriteriaWeights					as Test.QuickCheck.Input.CriteriaWeights
import qualified	BishBosh.Test.QuickCheck.Input.EvaluationOptions				as Test.QuickCheck.Input.EvaluationOptions
import qualified	BishBosh.Test.QuickCheck.Input.Options						as Test.QuickCheck.Input.Options
import qualified	BishBosh.Test.QuickCheck.Model.Game						as Test.QuickCheck.Model.Game
import qualified	BishBosh.Test.QuickCheck.Model.GameTerminationReason				as Test.QuickCheck.Model.GameTerminationReason
import qualified	BishBosh.Test.QuickCheck.Model.GameTree						as Test.QuickCheck.Model.GameTree
import qualified	BishBosh.Test.QuickCheck.Model.MoveFrequency					as Test.QuickCheck.Model.MoveFrequency
import qualified	BishBosh.Test.QuickCheck.Model.Result						as Test.QuickCheck.Model.Result
import qualified	BishBosh.Test.QuickCheck.Notation.ICCFNumeric					as Test.QuickCheck.Notation.ICCFNumeric
import qualified	BishBosh.Test.QuickCheck.Notation.PureCoordinate				as Test.QuickCheck.Notation.PureCoordinate
import qualified	BishBosh.Test.QuickCheck.Notation.Smith						as Test.QuickCheck.Notation.Smith
import qualified	BishBosh.Test.QuickCheck.Search.KillerMoves					as Test.QuickCheck.Search.KillerMoves
-- import qualified	BishBosh.Test.QuickCheck.Search.Search						as Test.QuickCheck.Search.Search
import qualified	BishBosh.Test.QuickCheck.State.Board						as Test.QuickCheck.State.Board
import qualified	BishBosh.Test.QuickCheck.State.CastleableRooksByLogicalColour			as Test.QuickCheck.State.CastleableRooksByLogicalColour
import qualified	BishBosh.Test.QuickCheck.State.Censor						as Test.QuickCheck.State.Censor
import qualified	BishBosh.Test.QuickCheck.State.InstancesByPosition				as Test.QuickCheck.State.InstancesByPosition
import qualified	BishBosh.Test.QuickCheck.State.Position						as Test.QuickCheck.State.Position
import qualified	BishBosh.Test.QuickCheck.State.TurnsByLogicalColour				as Test.QuickCheck.State.TurnsByLogicalColour
import qualified	BishBosh.Test.QuickCheck.Text.Encoding						as Test.QuickCheck.Text.Encoding
import qualified	BishBosh.Test.QuickCheck.UI.Command						as Test.QuickCheck.UI.Command
import qualified	BishBosh.Test.QuickCheck.UI.PrintObject						as Test.QuickCheck.UI.PrintObject
import qualified	BishBosh.Test.QuickCheck.UI.SetObject						as Test.QuickCheck.UI.SetObject
import qualified	Control.Monad
import qualified	System.Exit
import qualified	Test.HUnit
import qualified	ToolShed.Test.QuickCheck.Result

-- | Entry-point to the test-suite.
main :: IO ()
main	= mapM_ Test.HUnit.runTestTT [
	Test.HUnit.Attribute.Direction.testCases,
	Test.HUnit.Attribute.LogicalColour.testCases,
	Test.HUnit.Attribute.Rank.testCases,
	Test.HUnit.Cartesian.Coordinates.testCases,
	Test.HUnit.Cartesian.Vector.testCases,
	Test.HUnit.Component.Piece.testCases,
	Test.HUnit.Component.Move.testCases,
	Test.HUnit.Component.Zobrist.testCases,
	Test.HUnit.State.Board.testCases,
	Test.HUnit.Model.Game.testCases,
	Test.HUnit.Model.GameTree.testCases,
	Test.HUnit.Model.PositionHashTree.testCases,
	Test.HUnit.ContextualNotation.StandardAlgebraic.testCases,
	Test.HUnit.ContextualNotation.PGN.testCases,
	Test.HUnit.ContextualNotation.PositionHashQualifiedMoveTree.testCases,
	Test.HUnit.Input.Options.testCases,
	Test.HUnit.Evaluation.Fitness.testCases
--	Test.HUnit.Search.Search.testCases
 ] >> mapM_ (
	(`Control.Monad.unless` System.Exit.exitFailure) . all ToolShed.Test.QuickCheck.Result.isSuccessful =<<
 ) [
	Test.QuickCheck.Data.Integral.results,
	Test.QuickCheck.Text.Encoding.results,
	Test.QuickCheck.Attribute.LogicalColour.results,
	Test.QuickCheck.Attribute.Direction.results,
	Test.QuickCheck.Attribute.Rank.results,
	Test.QuickCheck.Attribute.MoveType.results,
	Test.QuickCheck.Input.CriteriaWeights.results,
	Test.QuickCheck.Input.EvaluationOptions.results,
	Test.QuickCheck.Input.Options.results,
	Test.QuickCheck.Cartesian.Coordinates.results,
	Test.QuickCheck.Cartesian.Vector.results,
	Test.QuickCheck.Component.Piece.results,
	Test.QuickCheck.Component.Move.results,
	Test.QuickCheck.Component.QualifiedMove.results,
	Test.QuickCheck.Component.Turn.results,
	Test.QuickCheck.Notation.ICCFNumeric.results,
	Test.QuickCheck.Notation.PureCoordinate.results,
	Test.QuickCheck.Notation.Smith.results,
	Test.QuickCheck.Model.GameTerminationReason.results,
	Test.QuickCheck.Model.Result.results,
	Test.QuickCheck.State.CastleableRooksByLogicalColour.results,
	Test.QuickCheck.State.Position.results,
	Test.QuickCheck.State.InstancesByPosition.results,
	Test.QuickCheck.State.Board.results,
	Test.QuickCheck.State.Censor.results,
	Test.QuickCheck.State.TurnsByLogicalColour.results,
	Test.QuickCheck.Model.Game.results,
	Test.QuickCheck.Model.GameTree.results,
	Test.QuickCheck.Model.MoveFrequency.results,
	Test.QuickCheck.Component.Zobrist.results,
	Test.QuickCheck.ContextualNotation.PositionHashQualifiedMoveTree.results,
	Test.QuickCheck.ContextualNotation.StandardAlgebraic.results,
	Test.QuickCheck.ContextualNotation.PGNComment.results,
	Test.QuickCheck.ContextualNotation.PGN.results,
	Test.QuickCheck.ContextualNotation.QualifiedMoveForest.results,
	Test.QuickCheck.Evaluation.PositionHashQuantifiedGameTree.results,
	Test.QuickCheck.Search.KillerMoves.results,
	Test.QuickCheck.UI.PrintObject.results,
	Test.QuickCheck.UI.SetObject.results,
	Test.QuickCheck.UI.Command.results
--	Test.QuickCheck.Search.Search.results
 ]

