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

module BishBosh.Test.QuickCheck.Input.EvaluationOptions(
-- * Types
-- ** Type-synonyms
	EvaluationOptions,
-- * Constants
	results
) where

import			BishBosh.Data.Bool()
import			BishBosh.Test.QuickCheck.Attribute.Rank()
import			BishBosh.Test.QuickCheck.Attribute.RankValues()
import			BishBosh.Test.QuickCheck.Input.CriteriaWeights()
import			BishBosh.Test.QuickCheck.Input.PieceSquareTable()
import			Control.Arrow((&&&))
import qualified	BishBosh.Component.PieceSquareArray		as Component.PieceSquareArray
import qualified	BishBosh.Input.CriteriaWeights			as Input.CriteriaWeights
import qualified	BishBosh.Input.EvaluationOptions		as Input.EvaluationOptions
import qualified	BishBosh.Input.PieceSquareTable			as Input.PieceSquareTable
import qualified	BishBosh.Property.Reflectable			as Property.Reflectable
import qualified	BishBosh.State.Board				as State.Board
import qualified	BishBosh.State.CoordinatesByRankByLogicalColour	as State.CoordinatesByRankByLogicalColour
import qualified	BishBosh.State.MaybePieceByCoordinates		as State.MaybePieceByCoordinates
import qualified	BishBosh.Test.QuickCheck.State.Board		as Test.QuickCheck.State.Board
import qualified	BishBosh.Types					as T
import qualified	Data.Array.IArray
import qualified	Data.List
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

-- | Defines a concrete type for testing.
type EvaluationOptions	= Input.EvaluationOptions.EvaluationOptions T.CriterionWeight T.PieceSquareValue T.RankValue T.X T.Y

instance (
	Enum				x,
	Enum				y,
	Fractional			criterionWeight,
	Fractional			pieceSquareValue,
	Fractional			rankValue,
	Ord				pieceSquareValue,
	Ord				rankValue,
	Ord				x,
	Ord				y,
	Real				criterionWeight,
	Show				criterionWeight,
	Show				pieceSquareValue,
	Show				rankValue,
	Test.QuickCheck.Arbitrary	rankValue
 ) => Test.QuickCheck.Arbitrary (Input.EvaluationOptions.EvaluationOptions criterionWeight pieceSquareValue rankValue x y) where
--	{-# SPECIALISE instance Test.QuickCheck.Arbitrary EvaluationOptions #-}
	arbitrary	= do
		criteriaWeights	<- Test.QuickCheck.arbitrary

		Input.EvaluationOptions.mkEvaluationOptions <$> Test.QuickCheck.arbitrary {-RankValues-} <*> return {-to Gen-monad-} criteriaWeights <*> Test.QuickCheck.arbitrary {-incrementalEvaluation-} <*> if Input.CriteriaWeights.getWeightOfPieceSquareValue criteriaWeights == minBound
			then return {-to Gen-monad-} Nothing
			else do
				(pieceSquareTable, pieceSquareTable', ranks)	<- Test.QuickCheck.arbitrary

				return {-to Gen-monad-} $ Just (
					pieceSquareTable,
					pieceSquareTable' {
						Input.PieceSquareTable.getByRank	= foldr Data.Map.delete (Input.PieceSquareTable.getByRank pieceSquareTable') $ Data.List.nub ranks	-- Delete arbitrary ranks from the end-game table.
					}
				 ) -- Pair.

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: EvaluationOptions -> Test.QuickCheck.State.Board.Board -> Test.QuickCheck.Property
		f evaluationOptions board	= Data.Maybe.isJust maybePieceSquareArray ==> Test.QuickCheck.label "EvaluationOptions.prop_sumPieceSquareValueByLogicalColour/reflectOnX" . uncurry (==) $ (
			Data.Array.IArray.elems . sumPieceSquareValueByLogicalColour &&& reverse . Data.Array.IArray.elems . sumPieceSquareValueByLogicalColour . Property.Reflectable.reflectOnX
		 ) board where
			maybePieceSquareArray			= Input.EvaluationOptions.getMaybePieceSquareArray evaluationOptions
			sumPieceSquareValueByLogicalColour	= State.Board.sumPieceSquareValueByLogicalColour $ Data.Maybe.fromJust maybePieceSquareArray
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 32 } f,
	let
		f :: Input.EvaluationOptions.EvaluationOptions T.CriterionWeight Rational {-precision is required-} T.RankValue T.X T.Y -> Test.QuickCheck.State.Board.Board -> Test.QuickCheck.Property
		f evaluationOptions board	= Data.Maybe.isJust maybePieceSquareArray ==> Test.QuickCheck.label "EvaluationOptions.prop_sumPieceSquareValueByLogicalColour" . uncurry (==) $ (
			State.CoordinatesByRankByLogicalColour.sumPieceSquareValueByLogicalColour findPieceSquareValue . State.Board.getCoordinatesByRankByLogicalColour &&& State.MaybePieceByCoordinates.sumPieceSquareValueByLogicalColour findPieceSquareValue . State.Board.getMaybePieceByCoordinates
		 ) board where
			maybePieceSquareArray	= Input.EvaluationOptions.getMaybePieceSquareArray evaluationOptions
			nPieces			= State.Board.getNPieces board
			findPieceSquareValue logicalColour rank coordinates	= Component.PieceSquareArray.findPieceSquareValue nPieces logicalColour rank coordinates $ Data.Maybe.fromJust maybePieceSquareArray
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f
 ]

