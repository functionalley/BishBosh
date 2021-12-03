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

module BishBosh.Test.HUnit.ContextualNotation.PositionHashQualifiedMoveTree(
-- * Constants
	testCases,
-- * Functions
--	mkGame,
--	checkMatch
) where

import qualified	BishBosh.Component.EitherQualifiedMove				as Component.EitherQualifiedMove
import qualified	BishBosh.Component.Move						as Component.Move
import qualified	BishBosh.Component.QualifiedMove				as Component.QualifiedMove
import qualified	BishBosh.Component.Turn						as Component.Turn
import qualified	BishBosh.Component.Zobrist					as Component.Zobrist
import qualified	BishBosh.ContextualNotation.PGN					as ContextualNotation.PGN
import qualified	BishBosh.ContextualNotation.PositionHashQualifiedMoveTree	as ContextualNotation.PositionHashQualifiedMoveTree
import qualified	BishBosh.ContextualNotation.QualifiedMoveForest			as ContextualNotation.QualifiedMoveForest
import qualified	BishBosh.Data.Exception						as Data.Exception
import qualified	BishBosh.Model.Game						as Model.Game
import qualified	BishBosh.Notation.MoveNotation					as Notation.MoveNotation
import qualified	BishBosh.Property.Reflectable					as Property.Reflectable
import qualified	BishBosh.Test.HUnit.Model.Game					as Test.HUnit.Model.Game
import qualified	BishBosh.Text.ShowList						as Text.ShowList
import qualified	BishBosh.Type.Crypto						as Type.Crypto
import qualified	Control.Exception
import qualified	Data.Default
import qualified	Test.HUnit
import			Test.HUnit((~:), (~?=))

mkGame :: [String] -> Model.Game.Game
mkGame	= either (
	\(moveString, errorMessage)	-> Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Test.HUnit.ContextualNotation.PositionHashQualifiedMoveTree.mkGame:\tfailed for " . showString Component.Move.tag . Text.ShowList.showsAssociation . shows moveString . showString "; " $ showString errorMessage "."
 ) id . Test.HUnit.Model.Game.applyMoves

checkMatch
	:: ContextualNotation.PositionHashQualifiedMoveTree.MatchSwitches
	-> Model.Game.Game	-- ^ Standard opening.
	-> [String]		-- ^ Move-sequence required to construct test-game.
	-> Component.Move.Move	-- ^ Expected move.
	-> Test.HUnit.Test
checkMatch matchSwitches game moveStrings expectedMove	= map (
	Component.QualifiedMove.getMove . fst {-qualifiedMove-}
 ) (
	ContextualNotation.PositionHashQualifiedMoveTree.findNextOnymousQualifiedMoves matchSwitches (
		ContextualNotation.PositionHashQualifiedMoveTree.fromQualifiedMoveForest False {-incrementalEvaluation-} (
			Data.Default.def	:: Component.Zobrist.Zobrist Type.Crypto.PositionHash
		) $ ContextualNotation.QualifiedMoveForest.fromPGNDatabase [
			ContextualNotation.PGN.mkPGN' [] [
				(ContextualNotation.PGN.dateTag, "2018.01.01")
			] game
		]
	) $ mkGame moveStrings
 ) ~?= [expectedMove]

-- | Check the sanity of the implementation, by validating a list of static test-cases.
testCases :: Test.HUnit.Test
testCases	= Test.HUnit.test [
	"ContextualNotation.PositionHashQualifiedMoveTree.findNextOnymousQualifiedMoves" ~: checkMatch (True, False, False) game2 (init moveStrings2) finalMove2,	-- Exact match for standard-opening.
	"ContextualNotation.PositionHashQualifiedMoveTree.findNextOnymousQualifiedMoves/transposition" ~: checkMatch (False, False, False) game1 (words "e2e4 c7c5 d2d4 c5d4p g1f3 d7d6 f3d4p g8f6 b1c3") finalMove1,	-- White's 2nd & 3rd moves have been transposed & Black's 2nd & 3rd move also have been transposed.
	"ContextualNotation.PositionHashQualifiedMoveTree.findNextOnymousQualifiedMoves/transposition" ~: checkMatch (False, False, False) game2 (words "g1f3 d7d6 e2e4 c7c5 d2d4 c5d4p f3d4p g8f6 b1c3 a7a6 c1g5 e7e6 f2f4 d8b6 d1d2 b6b2p a1b1 b2a3 f4f5 b8c6 f5e6p f7e6p d4c6n b7c6n e4e5 d6e5p g5f6n g7f6b c3e4 f8e7 f1e2 h7h5 b1b3 a3a4") finalMove2,	-- White & also Black's 1st & 2nd moves have been transposed.
	"ContextualNotation.PositionHashQualifiedMoveTree.findNextOnymousQualifiedMoves/join" ~: checkMatch (False, True {-TryToMatchViaJoiningMove-}, False) game1 (words "e2e4 c7c5 g1f3 d7d6 d2d4 c5d4p b1c3 g8f6") (
		moves1 !! 6
	), -- White still has to make the 2nd last move.
	"ContextualNotation.PositionHashQualifiedMoveTree.findNextOnymousQualifiedMoves/join" ~: checkMatch (False, True {-TryToMatchViaJoiningMove-}, False) game2 (words "e2e4 c7c5 g1f3 d7d6 d2d4 c5d4p f3d4p g8f6 b1c3 a7a6 c1g5 e7e6 f2f4 d8b6 d1d2 b6b2p a1b1 b2a3 f4f5 b8c6 f5e6p f7e6p d4c6n b7c6n e4e5 d6e5p g5f6n g7f6b c3e4 f8e7 f1e2 a3a4 b1b3") (
		moves2 !! 31
	), -- White still has to make the 2nd last move.
	"ContextualNotation.PositionHashQualifiedMoveTree.findNextOnymousQualifiedMoves/colourFlip" ~: checkMatch (False, False, True {-TryToMatchColourFlippedPosition-}) game1 (words "c2c3 e7e5 c3c4 g8f6 d2d3 d7d5 c4d5p f6d5p g1f3 b8c6") (
		Property.Reflectable.reflectOnX finalMove1
	), -- White's first two moves are achievable in only one.
	"ContextualNotation.PositionHashQualifiedMoveTree.findNextOnymousQualifiedMoves/colourFlip/transposition" ~: checkMatch (False, False, True {-TryToMatchColourFlippedPosition-}) game1 (words "c2c3 g8f6 c3c4 e7e5 d2d3 d7d5 c4d5p f6d5p g1f3 b8c6") (
		Property.Reflectable.reflectOnX finalMove1
	), -- White's first two moves are achievable in only one & Black's first two moves have been transposed.
	"ContextualNotation.PositionHashQualifiedMoveTree.findNextOnymousQualifiedMoves/colourFlip/join" ~: checkMatch (False, True, True) (
		mkGame $ moveStrings1 ++ ["f2f3"]	-- Append a move after the match.
	) (words "c2c3 g8f6 c3c4 e7e5 d2d3 d7d5 c4d5p f6d5p g2g3 b8c6") . Component.EitherQualifiedMove.getMove . fst . head $ Notation.MoveNotation.readsQualifiedMove Data.Default.def "g1f3"
 ] where
	moveStrings1, moveStrings2 :: [String]
	moveStrings1	= words "e2e4 c7c5 g1f3 d7d6 d2d4 c5d4p f3d4p g8f6 b1c3 g7g6"
	moveStrings2	= words "e2e4 c7c5 g1f3 d7d6 d2d4 c5d4p f3d4p g8f6 b1c3 a7a6 c1g5 e7e6 f2f4 d8b6 d1d2 b6b2p a1b1 b2a3 f4f5 b8c6 f5e6p f7e6p d4c6n b7c6n e4e5 d6e5p g5f6n g7f6b c3e4 f8e7 f1e2 h7h5 b1b3 a3a4 c2c4"	-- ECO B97x.

	game1, game2 :: Model.Game.Game
	game1	= mkGame moveStrings1
	game2	= mkGame moveStrings2

	moves1, moves2 :: [Component.Move.Move]
	[moves1, moves2]	= map (
		map (Component.QualifiedMove.getMove . Component.Turn.getQualifiedMove) . Model.Game.listTurnsChronologically
	 ) [game1, game2]

	finalMove1, finalMove2 :: Component.Move.Move
	finalMove1	= last moves1
	finalMove2	= last moves2

