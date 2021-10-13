{-# LANGUAGE CPP #-}
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

 [@DESCRIPTION@]	The entry-point to the application's HUnit test-suite.
-}

module Main(main) where

import qualified	BishBosh.Test.HUnit.Attribute.Direction					as Test.HUnit.Attribute.Direction
import qualified	BishBosh.Test.HUnit.Attribute.LogicalColour				as Test.HUnit.Attribute.LogicalColour
import qualified	BishBosh.Test.HUnit.Attribute.Rank					as Test.HUnit.Attribute.Rank
import qualified	BishBosh.Test.HUnit.Cartesian.Coordinates				as Test.HUnit.Cartesian.Coordinates
import qualified	BishBosh.Test.HUnit.Cartesian.Vector					as Test.HUnit.Cartesian.Vector
import qualified	BishBosh.Test.HUnit.Component.Move					as Test.HUnit.Component.Move
import qualified	BishBosh.Test.HUnit.Component.CastlingMove				as Test.HUnit.Component.CastlingMove
import qualified	BishBosh.Test.HUnit.Component.Piece					as Test.HUnit.Component.Piece
import qualified	BishBosh.Test.HUnit.Component.Zobrist					as Test.HUnit.Component.Zobrist
import qualified	BishBosh.Test.HUnit.ContextualNotation.PGN				as Test.HUnit.ContextualNotation.PGN
import qualified	BishBosh.Test.HUnit.ContextualNotation.PositionHashQualifiedMoveTree	as Test.HUnit.ContextualNotation.PositionHashQualifiedMoveTree
import qualified	BishBosh.Test.HUnit.ContextualNotation.StandardAlgebraic		as Test.HUnit.ContextualNotation.StandardAlgebraic
import qualified	BishBosh.Test.HUnit.Evaluation.Fitness					as Test.HUnit.Evaluation.Fitness
import qualified	BishBosh.Test.HUnit.Input.Options					as Test.HUnit.Input.Options
import qualified	BishBosh.Test.HUnit.Model.Game						as Test.HUnit.Model.Game
import qualified	BishBosh.Test.HUnit.Model.GameTree					as Test.HUnit.Model.GameTree
import qualified	BishBosh.Test.HUnit.Model.PositionHashTree				as Test.HUnit.Model.PositionHashTree
import qualified	BishBosh.Test.HUnit.State.Board						as Test.HUnit.State.Board
import qualified	BishBosh.Test.HUnit.Text.AutoComplete					as Test.HUnit.Text.AutoComplete
import qualified	BishBosh.Test.HUnit.Time.GameClock					as Test.HUnit.Time.GameClock
import qualified	BishBosh.Test.HUnit.Time.StopWatch					as Test.HUnit.Time.StopWatch
import qualified	Test.HUnit

#ifdef USE_SEARCH
import qualified	BishBosh.Test.HUnit.Search.Search					as Test.HUnit.Search.Search
#endif

-- | Entry-point to the test-suite.
main :: IO ()
main	= mapM_ Test.HUnit.runTestTT [
	Test.HUnit.Text.AutoComplete.testCases,
	Test.HUnit.Attribute.Direction.testCases,
	Test.HUnit.Attribute.LogicalColour.testCases,
	Test.HUnit.Attribute.Rank.testCases,
	Test.HUnit.Cartesian.Coordinates.testCases,
	Test.HUnit.Cartesian.Vector.testCases,
	Test.HUnit.Component.Piece.testCases,
	Test.HUnit.Component.Move.testCases,
	Test.HUnit.Component.CastlingMove.testCases,
	Test.HUnit.Component.Zobrist.testCases,
	Test.HUnit.Time.StopWatch.testCases,
	Test.HUnit.Time.GameClock.testCases,
	Test.HUnit.State.Board.testCases,
	Test.HUnit.Model.Game.testCases,
	Test.HUnit.Model.GameTree.testCases,
	Test.HUnit.Model.PositionHashTree.testCases,
	Test.HUnit.ContextualNotation.StandardAlgebraic.testCases,
	Test.HUnit.ContextualNotation.PGN.testCases,
	Test.HUnit.ContextualNotation.PositionHashQualifiedMoveTree.testCases,
	Test.HUnit.Input.Options.testCases,
#ifdef USE_SEARCH
	Test.HUnit.Search.Search.testCases,
#endif
	Test.HUnit.Evaluation.Fitness.testCases

 ]

