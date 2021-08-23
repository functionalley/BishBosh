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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties.
-}

module BishBosh.Test.QuickCheck.Evaluation.PositionHashQuantifiedGameTree(
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Input.SearchOptions()
import qualified	BishBosh.Evaluation.PositionHashQuantifiedGameTree	as Evaluation.PositionHashQuantifiedGameTree
import qualified	BishBosh.Evaluation.QuantifiedGame			as Evaluation.QuantifiedGame
-- import qualified	BishBosh.Input.EvaluationOptions			as Input.EvaluationOptions
import qualified	BishBosh.Input.SearchOptions				as Input.SearchOptions
import qualified	BishBosh.Model.Game					as Model.Game
-- import qualified	BishBosh.Property.Arboreal				as Property.Arboreal
import qualified	BishBosh.Property.Empty					as Property.Empty
import qualified	BishBosh.State.Position					as State.Position
import qualified	BishBosh.Test.QuickCheck.Component.Zobrist		as Test.QuickCheck.Component.Zobrist
import qualified	BishBosh.Test.QuickCheck.Input.EvaluationOptions	as Test.QuickCheck.Input.EvaluationOptions
import qualified	BishBosh.Test.QuickCheck.Model.Game			as Test.QuickCheck.Model.Game
import qualified	BishBosh.Types						as T
import qualified	Data.List.Extra
import qualified	Data.Ord
import qualified	Data.Tree
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
{-
	let
		f
			:: Test.QuickCheck.Input.EvaluationOptions.EvaluationOptions
			-> Input.SearchOptions.SearchOptions
			-> Test.QuickCheck.Component.Zobrist.Zobrist
			-> Test.QuickCheck.Model.Game.Game
			-> Test.QuickCheck.Property
		f evaluationOptions searchOptions zobrist game	= Test.QuickCheck.label "PositionHashQuantifiedGameTree.prop_mkPositionHashQuantifiedGameTree" $ prune (
			Evaluation.PositionHashQuantifiedGameTree.mkPositionHashQuantifiedGameTree evaluationOptions {
				Input.EvaluationOptions.getIncrementalEvaluation	= False
			} searchOptions zobrist Property.Empty.empty {-MoveFrequency-} game	:: Evaluation.PositionHashQuantifiedGameTree.PositionHashQuantifiedGameTree T.X T.Y T.PositionHash T.CriterionValue T.WeightedMean
		 ) == prune (
			Evaluation.PositionHashQuantifiedGameTree.mkPositionHashQuantifiedGameTree evaluationOptions {
				Input.EvaluationOptions.getIncrementalEvaluation	= True	-- CAVEAT: unrealistic given rounding-errors in incremental calculation of pieceSquareValue.
			} searchOptions zobrist Property.Empty.empty {-MoveFrequency-} game
		 ) where
			prune	= Property.Arboreal.prune 3
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 4 } f,
-}
	let
		f
			:: Test.QuickCheck.Input.EvaluationOptions.EvaluationOptions
			-> Input.SearchOptions.SearchOptions
			-> Test.QuickCheck.Component.Zobrist.Zobrist
			-> Test.QuickCheck.Model.Game.Game
			-> Test.QuickCheck.Property
		f evaluationOptions searchOptions zobrist game	= not (null matches) ==> Test.QuickCheck.label "PositionHashQuantifiedGameTree.prop_hashCollisions" $ all (
			Data.List.Extra.allSame . map (
				Model.Game.mkPosition . Evaluation.QuantifiedGame.getGame . Evaluation.PositionHashQuantifiedGameTree.getQuantifiedGame	:: Evaluation.PositionHashQuantifiedGameTree.NodeLabel T.X T.Y T.PositionHash T.CriterionValue T.WeightedMean -> State.Position.Position T.X T.Y
			)
		 ) matches where
			matches	= filter (
				(/= 1) . length
			 ) . Data.List.Extra.groupSortBy (
				Data.Ord.comparing Evaluation.PositionHashQuantifiedGameTree.getPositionHash
			 ) . take 10000 . Data.Tree.flatten . Evaluation.PositionHashQuantifiedGameTree.deconstruct $ Evaluation.PositionHashQuantifiedGameTree.mkPositionHashQuantifiedGameTree evaluationOptions searchOptions zobrist Property.Empty.empty {-MoveFrequency-} game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 32 } f,
	let
		f
			:: Test.QuickCheck.Input.EvaluationOptions.EvaluationOptions
			-> Input.SearchOptions.SearchOptions
			-> Test.QuickCheck.Component.Zobrist.Zobrist
			-> Test.QuickCheck.Model.Game.Game
			-> Test.QuickCheck.Property
		f evaluationOptions searchOptions zobrist	= Test.QuickCheck.label "PositionHashQuantifiedGameTree.prop_fitness" . all (
			(<= 1) . abs
		 ) . take 10000 . map (
			Evaluation.QuantifiedGame.getFitness . Evaluation.PositionHashQuantifiedGameTree.getQuantifiedGame	:: Evaluation.PositionHashQuantifiedGameTree.NodeLabel T.X T.Y T.PositionHash T.CriterionValue T.WeightedMean -> T.WeightedMean
		 ) . Data.Tree.flatten . Evaluation.PositionHashQuantifiedGameTree.deconstruct . Evaluation.PositionHashQuantifiedGameTree.mkPositionHashQuantifiedGameTree evaluationOptions searchOptions zobrist Property.Empty.empty {-MoveFrequency-}
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 32 } f
 ]
