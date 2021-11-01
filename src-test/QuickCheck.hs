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

 [@DESCRIPTION@]	The entry-point to the application's QuickCheck test-suite.
-}

module Main(main) where

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
import qualified	BishBosh.Test.QuickCheck.ContextualNotation.PGN					as Test.QuickCheck.ContextualNotation.PGN
import qualified	BishBosh.Test.QuickCheck.ContextualNotation.PGNComment				as Test.QuickCheck.ContextualNotation.PGNComment
import qualified	BishBosh.Test.QuickCheck.ContextualNotation.PositionHashQualifiedMoveTree	as Test.QuickCheck.ContextualNotation.PositionHashQualifiedMoveTree
import qualified	BishBosh.Test.QuickCheck.ContextualNotation.QualifiedMoveForest			as Test.QuickCheck.ContextualNotation.QualifiedMoveForest
import qualified	BishBosh.Test.QuickCheck.ContextualNotation.StandardAlgebraic			as Test.QuickCheck.ContextualNotation.StandardAlgebraic
import qualified	BishBosh.Test.QuickCheck.Data.Foldable						as Test.QuickCheck.Data.Foldable
import qualified	BishBosh.Test.QuickCheck.Data.Integral						as Test.QuickCheck.Data.Integral
import qualified	BishBosh.Test.QuickCheck.Evaluation.PositionHashQuantifiedGameTree		as Test.QuickCheck.Evaluation.PositionHashQuantifiedGameTree
import qualified	BishBosh.Test.QuickCheck.Input.CriteriaWeights					as Test.QuickCheck.Input.CriteriaWeights
import qualified	BishBosh.Test.QuickCheck.Input.EvaluationOptions				as Test.QuickCheck.Input.EvaluationOptions
import qualified	BishBosh.Test.QuickCheck.Input.Options						as Test.QuickCheck.Input.Options
import qualified	BishBosh.Test.QuickCheck.Input.PieceSquareTable					as Test.QuickCheck.Input.PieceSquareTable
import qualified	BishBosh.Test.QuickCheck.Metric.RankValue					as Test.QuickCheck.Metric.RankValue
import qualified	BishBosh.Test.QuickCheck.Model.Game						as Test.QuickCheck.Model.Game
import qualified	BishBosh.Test.QuickCheck.Model.GameTree						as Test.QuickCheck.Model.GameTree
import qualified	BishBosh.Test.QuickCheck.Model.MoveFrequency					as Test.QuickCheck.Model.MoveFrequency
import qualified	BishBosh.Test.QuickCheck.Notation.Figurine					as Test.QuickCheck.Notation.Figurine
import qualified	BishBosh.Test.QuickCheck.Notation.ICCFNumeric					as Test.QuickCheck.Notation.ICCFNumeric
import qualified	BishBosh.Test.QuickCheck.Notation.MoveNotation					as Test.QuickCheck.Notation.MoveNotation
import qualified	BishBosh.Test.QuickCheck.Notation.PureCoordinate				as Test.QuickCheck.Notation.PureCoordinate
import qualified	BishBosh.Test.QuickCheck.Notation.Smith						as Test.QuickCheck.Notation.Smith
import qualified	BishBosh.Test.QuickCheck.Rule.GameTerminationReason				as Test.QuickCheck.Rule.GameTerminationReason
import qualified	BishBosh.Test.QuickCheck.Rule.Result						as Test.QuickCheck.Rule.Result
import qualified	BishBosh.Test.QuickCheck.Search.KillerMoves					as Test.QuickCheck.Search.KillerMoves
-- import qualified	BishBosh.Test.QuickCheck.Search.Search						as Test.QuickCheck.Search.Search
import qualified	BishBosh.Test.QuickCheck.State.Board						as Test.QuickCheck.State.Board
import qualified	BishBosh.Test.QuickCheck.State.CastleableRooksByLogicalColour			as Test.QuickCheck.State.CastleableRooksByLogicalColour
import qualified	BishBosh.Test.QuickCheck.State.InstancesByPosition				as Test.QuickCheck.State.InstancesByPosition
import qualified	BishBosh.Test.QuickCheck.State.Position						as Test.QuickCheck.State.Position
import qualified	BishBosh.Test.QuickCheck.StateProperty.Censor					as Test.QuickCheck.StateProperty.Censor
import qualified	BishBosh.Test.QuickCheck.StateProperty.Hashable					as Test.QuickCheck.StateProperty.Hashable
import qualified	BishBosh.Test.QuickCheck.StateProperty.Seeker					as Test.QuickCheck.StateProperty.Seeker
import qualified	BishBosh.Test.QuickCheck.State.TurnsByLogicalColour				as Test.QuickCheck.State.TurnsByLogicalColour
import qualified	BishBosh.Test.QuickCheck.Text.Encoding						as Test.QuickCheck.Text.Encoding
import qualified	BishBosh.Test.QuickCheck.UI.Command						as Test.QuickCheck.UI.Command
import qualified	BishBosh.Test.QuickCheck.UI.PrintObject						as Test.QuickCheck.UI.PrintObject
import qualified	BishBosh.Test.QuickCheck.UI.SetObject						as Test.QuickCheck.UI.SetObject
import qualified	Control.Monad
import qualified	System.Exit
import qualified	ToolShed.Test.QuickCheck.Result

-- | Entry-point to the test-suite.
main :: IO ()
main	= mapM_ (
	(`Control.Monad.unless` System.Exit.exitFailure) . all ToolShed.Test.QuickCheck.Result.isSuccessful =<<
 ) [
	Test.QuickCheck.Data.Foldable.results,
	Test.QuickCheck.Data.Integral.results,
	Test.QuickCheck.Text.Encoding.results,
	Test.QuickCheck.Attribute.LogicalColour.results,
	Test.QuickCheck.Attribute.Direction.results,
	Test.QuickCheck.Attribute.Rank.results,
	Test.QuickCheck.Attribute.MoveType.results,
	Test.QuickCheck.Input.EvaluationOptions.results,
	Test.QuickCheck.Input.Options.results,
	Test.QuickCheck.Input.PieceSquareTable.results,
	Test.QuickCheck.Cartesian.Coordinates.results,
	Test.QuickCheck.Cartesian.Vector.results,
	Test.QuickCheck.Component.Piece.results,
	Test.QuickCheck.Component.Move.results,
	Test.QuickCheck.Component.QualifiedMove.results,
	Test.QuickCheck.Component.Turn.results,
	Test.QuickCheck.Rule.GameTerminationReason.results,
	Test.QuickCheck.Rule.Result.results,
	Test.QuickCheck.Notation.Figurine.results,
	Test.QuickCheck.Notation.ICCFNumeric.results,
	Test.QuickCheck.Notation.PureCoordinate.results,
	Test.QuickCheck.Notation.Smith.results,
	Test.QuickCheck.Notation.MoveNotation.results,
	Test.QuickCheck.State.CastleableRooksByLogicalColour.results,
	Test.QuickCheck.State.Position.results,
	Test.QuickCheck.State.InstancesByPosition.results,
	Test.QuickCheck.State.Board.results,
	Test.QuickCheck.State.TurnsByLogicalColour.results,
	Test.QuickCheck.StateProperty.Censor.results,
	Test.QuickCheck.StateProperty.Hashable.results,
	Test.QuickCheck.StateProperty.Seeker.results,
	Test.QuickCheck.Model.Game.results,
	Test.QuickCheck.Model.GameTree.results,
	Test.QuickCheck.Model.MoveFrequency.results,
	Test.QuickCheck.ContextualNotation.PositionHashQualifiedMoveTree.results,
	Test.QuickCheck.ContextualNotation.StandardAlgebraic.results,
	Test.QuickCheck.ContextualNotation.PGNComment.results,
	Test.QuickCheck.ContextualNotation.PGN.results,
	Test.QuickCheck.ContextualNotation.QualifiedMoveForest.results,
	Test.QuickCheck.Input.CriteriaWeights.results,
	Test.QuickCheck.Metric.RankValue.results,
	Test.QuickCheck.Evaluation.PositionHashQuantifiedGameTree.results,
	Test.QuickCheck.Search.KillerMoves.results,
	Test.QuickCheck.UI.PrintObject.results,
	Test.QuickCheck.UI.SetObject.results,
	Test.QuickCheck.UI.Command.results
--	Test.QuickCheck.Search.Search.results
 ]

