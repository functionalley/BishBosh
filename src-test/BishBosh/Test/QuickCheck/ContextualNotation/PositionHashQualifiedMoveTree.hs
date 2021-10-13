{-# OPTIONS_GHC -fno-warn-orphans #-}
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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties.
-}

module BishBosh.Test.QuickCheck.ContextualNotation.PositionHashQualifiedMoveTree(
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.ContextualNotation.QualifiedMoveForest()
import qualified	BishBosh.Component.QualifiedMove				as Component.QualifiedMove
import qualified	BishBosh.Component.Turn						as Component.Turn
import qualified	BishBosh.ContextualNotation.PositionHashQualifiedMoveTree	as ContextualNotation.PositionHashQualifiedMoveTree
import qualified	BishBosh.ContextualNotation.QualifiedMoveForest			as ContextualNotation.QualifiedMoveForest
import qualified	BishBosh.Input.EvaluationOptions				as Input.EvaluationOptions
import qualified	BishBosh.Input.StandardOpeningOptions				as Input.StandardOpeningOptions
import qualified	BishBosh.Model.Game						as Model.Game
import qualified	BishBosh.Model.GameTree						as Model.GameTree
import qualified	BishBosh.Property.Null						as Property.Null
import qualified	BishBosh.Test.QuickCheck.Component.Zobrist			as Test.QuickCheck.Component.Zobrist
import qualified	BishBosh.Type.Crypto						as Type.Crypto
import qualified	Data.Bits
import qualified	Data.List
import qualified	Data.Maybe
import qualified	Data.Tree
import qualified	System.Random
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

instance (
	Data.Bits.FiniteBits	positionHash,
	Num			positionHash,
	System.Random.Random	positionHash
 ) => Test.QuickCheck.Arbitrary (ContextualNotation.PositionHashQualifiedMoveTree.PositionHashQualifiedMoveTree positionHash) where
	{-# SPECIALISE instance Test.QuickCheck.Arbitrary (ContextualNotation.PositionHashQualifiedMoveTree.PositionHashQualifiedMoveTree Type.Crypto.PositionHash) #-}
	arbitrary	= ContextualNotation.PositionHashQualifiedMoveTree.fromQualifiedMoveForest <$> Test.QuickCheck.arbitrary {-IncrementalEvaluation-} <*> Test.QuickCheck.arbitrary {-Zobrist-} <*> Test.QuickCheck.arbitrary {-QualifiedMoveForest-}

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Input.EvaluationOptions.IncrementalEvaluation -> Input.StandardOpeningOptions.MatchSwitches -> Test.QuickCheck.Component.Zobrist.Zobrist -> ContextualNotation.QualifiedMoveForest.QualifiedMoveForest -> Test.QuickCheck.Property
		f incrementalEvaluation tryToMatchSwitches zobrist qualifiedMoveForest	= not (Property.Null.isNull qualifiedMoveForest) ==> Test.QuickCheck.label "PositionHashQualifiedMoveTree.prop_findNextOnymousQualifiedMoves" $ all (
			(
				\(previousGame, lastTurn)	-> Data.Maybe.isJust . Data.List.find (
					(
						== Component.QualifiedMove.getMove (Component.Turn.getQualifiedMove lastTurn)
					) . Component.QualifiedMove.getMove . fst {-qualifiedMove-}
				) . ContextualNotation.PositionHashQualifiedMoveTree.findNextOnymousQualifiedMoves tryToMatchSwitches previousGame $ ContextualNotation.PositionHashQualifiedMoveTree.fromQualifiedMoveForest incrementalEvaluation zobrist qualifiedMoveForest
			) . head . Model.Game.rollBack
		 ) . tail {-drop the default game-} . Data.Tree.flatten . Model.GameTree.deconstruct $ ContextualNotation.QualifiedMoveForest.toGameTree qualifiedMoveForest
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 8 } f
 ]

