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
	along with BishBosh.  If not, see <https://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]

	* Static tests.
-}

module BishBosh.Test.HUnit.Model.GameTree(
-- * Constants
	testCases
) where

import qualified	BishBosh.Attribute.CaptureMoveSortAlgorithm	as Attribute.CaptureMoveSortAlgorithm
import qualified	BishBosh.Attribute.Rank				as Attribute.Rank
import qualified	BishBosh.Attribute.RankValues			as Attribute.RankValues
import qualified	BishBosh.Component.Turn				as Component.Turn
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.Model.GameTree				as Model.GameTree
import qualified	BishBosh.Notation.MoveNotation			as Notation.MoveNotation
import qualified	BishBosh.Property.Empty				as Property.Empty
import qualified	BishBosh.Property.ForsythEdwards		as Property.ForsythEdwards
import qualified	BishBosh.Types					as T
import qualified	Data.Default
import qualified	Data.Maybe
import qualified	Data.Tree
import qualified	Test.HUnit
import			Test.HUnit((~:), (~?=))

-- | Check the sanity of the implementation, by validating a list of static test-cases.
testCases :: Test.HUnit.Test
testCases	= Test.HUnit.test [
	let
		maxDepth	= 4
	in "'BishBosh.Model.GameTree.countGames' failed" ~: map Model.GameTree.countGames [0 .. maxDepth] ~?= take (succ maxDepth) [1, 20, 400, 8902, 197281, 4865609],	-- <https://oeis.org/A048987>
	"'BishBosh.Model.GameTree.countMoves' failed" ~: map (
		\searchDepth -> Model.GameTree.countMoves searchDepth ~?= foldr (
			(+) . Model.GameTree.countGames
		) 0 [1 .. searchDepth]
	) [1 .. 4],
	"'BishBosh.Model.GameTree.sortGameTree/MVVLVA' failed" ~: map (
		\turn -> Notation.MoveNotation.showNotation Data.Default.def (turn :: Component.Turn.Turn T.X T.Y)
	) (
		sortAvailableMoves (Just Attribute.CaptureMoveSortAlgorithm.MVVLVA) testString
	) ~?= words "c4d5p e4d5p c3d5p d1d5p c1e3 c1f4 d1d4 g1e2 d1d3 c1d2 c1g5 a1b1 d1d2 d1e2 e1d2 e1e2 a2a4 g2g4 h2h4 g1h3 a2a3 g2g3 h2h3 c1b2 b3b4 c1a3 c1h6 c4c5 e4e5 e1f1 c3b5 c3e2 f3e2 f3g4 c3a4 f3h5 c3b1",
	"'BishBosh.Model.GameTree.sortGameTree/SEE' failed" ~: map (
		\turn -> Notation.MoveNotation.showNotation Data.Default.def (turn :: Component.Turn.Turn T.X T.Y)
	) (
		sortAvailableMoves (Just Attribute.CaptureMoveSortAlgorithm.SEE) testString
	) ~?= words "c4d5p e4d5p c1e3 c1f4 d1d4 d1d5p g1e2 d1d3 c1d2 c1g5 a1b1 d1d2 d1e2 e1d2 e1e2 a2a4 g2g4 h2h4 g1h3 a2a3 g2g3 h2h3 c3d5p c1b2 b3b4 c1a3 c1h6 c4c5 e4e5 e1f1 c3b5 c3e2 f3e2 f3g4 c3a4 f3h5 c3b1"
 ] where
	sortAvailableMoves :: (
		Integral	x,
		Integral	y,
		Read		x,
		Read		y,
		Show		x,
		Show		y
	 ) => Maybe Attribute.CaptureMoveSortAlgorithm.CaptureMoveSortAlgorithm -> String -> [Component.Turn.Turn x y]
	sortAvailableMoves maybeSortAlgorithm	= Data.Maybe.mapMaybe (
		Model.Game.maybeLastTurn . Data.Tree.rootLabel
	 ) . Data.Tree.subForest . Model.GameTree.deconstruct . Model.GameTree.sortGameTree True {-PreferMovesTowardsCentre-} maybeSortAlgorithm (
		`Attribute.RankValues.findRankValue` Attribute.RankValues.fromAssocs (
			zip Attribute.Rank.range $ map (/ 10) [
				1	:: T.RankValue,
				5,	-- R
				3,	-- N
				7 / 2,	-- B
				9,	-- Q
				0	-- K
			]
		)
	 ) Property.Empty.empty {-MoveFrequency-} . Model.GameTree.fromGame . Property.ForsythEdwards.readFEN

	testString	= "rn1qkb1r/pp2pppp/2p1bn2/3p4/2P1P3/1PN2B2/P1P2PPP/R1BQK1NR w KQkq - 0 5"

