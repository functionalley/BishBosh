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
import			BishBosh.Test.QuickCheck.Input.PieceSquareTable()
import			BishBosh.Test.QuickCheck.Input.RankValues()
import			BishBosh.Test.QuickCheck.Metric.CriteriaWeights()
import			Control.Arrow((&&&))
import qualified	BishBosh.Component.PieceSquareByCoordinatesByRank	as Component.PieceSquareByCoordinatesByRank
import qualified	BishBosh.Input.EvaluationOptions			as Input.EvaluationOptions
import qualified	BishBosh.Input.PieceSquareTable				as Input.PieceSquareTable
import qualified	BishBosh.Metric.CriteriaWeights				as Metric.CriteriaWeights
import qualified	BishBosh.Property.Reflectable				as Property.Reflectable
import qualified	BishBosh.State.Board					as State.Board
import qualified	BishBosh.State.CoordinatesByRankByLogicalColour		as State.CoordinatesByRankByLogicalColour
import qualified	BishBosh.State.MaybePieceByCoordinates			as State.MaybePieceByCoordinates
import qualified	BishBosh.Test.QuickCheck.State.Board			as Test.QuickCheck.State.Board
import qualified	BishBosh.Type.Length					as Type.Length
import qualified	BishBosh.Type.Mass					as Type.Mass
import qualified	Data.Array.IArray
import qualified	Data.List
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

-- | Defines a concrete type for testing.
type EvaluationOptions	= Input.EvaluationOptions.EvaluationOptions Type.Mass.PieceSquareValue Type.Length.X Type.Length.Y

instance (
	Enum		x,
	Enum		y,
	Fractional	pieceSquareValue,
	Ord		pieceSquareValue,
	Ord		x,
	Ord		y,
	Show		pieceSquareValue
 ) => Test.QuickCheck.Arbitrary (Input.EvaluationOptions.EvaluationOptions pieceSquareValue x y) where
--	{-# SPECIALISE instance Test.QuickCheck.Arbitrary EvaluationOptions #-}
	arbitrary	= do
		criteriaWeights	<- Test.QuickCheck.arbitrary

		Input.EvaluationOptions.mkEvaluationOptions <$> Test.QuickCheck.arbitrary {-RankValues-} <*> return {-to Gen-monad-} criteriaWeights <*> Test.QuickCheck.arbitrary {-incrementalEvaluation-} <*> if Metric.CriteriaWeights.getWeightOfPieceSquareValue criteriaWeights == minBound
			then return {-to Gen-monad-} Nothing
			else do
				(pieceSquareTable, pieceSquareTable', ranks)	<- Test.QuickCheck.arbitrary

				return {-to Gen-monad-} $ Just (
					pieceSquareTable,
					pieceSquareTable' {
						Input.PieceSquareTable.getPieceSquareValueByCoordinatesByRank	= foldr Data.Map.delete (Input.PieceSquareTable.getPieceSquareValueByCoordinatesByRank pieceSquareTable') $ Data.List.nub ranks	-- Delete arbitrary ranks from the end-game table.
					}
				 ) -- Pair.

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: EvaluationOptions -> Test.QuickCheck.State.Board.Board -> Test.QuickCheck.Property
		f evaluationOptions board	= Data.Maybe.isJust maybePieceSquareByCoordinatesByRank ==> Test.QuickCheck.label "EvaluationOptions.prop_sumPieceSquareValueByLogicalColour/reflectOnX" . uncurry (==) $ (
			Data.Array.IArray.elems . sumPieceSquareValueByLogicalColour &&& reverse . Data.Array.IArray.elems . sumPieceSquareValueByLogicalColour . Property.Reflectable.reflectOnX
		 ) board where
			maybePieceSquareByCoordinatesByRank	= Input.EvaluationOptions.getMaybePieceSquareByCoordinatesByRank evaluationOptions
			sumPieceSquareValueByLogicalColour	= State.Board.sumPieceSquareValueByLogicalColour $ Data.Maybe.fromJust maybePieceSquareByCoordinatesByRank
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 32 } f,
	let
		f :: Input.EvaluationOptions.EvaluationOptions Rational {-precision is required-} Type.Length.X Type.Length.Y -> Test.QuickCheck.State.Board.Board -> Test.QuickCheck.Property
		f evaluationOptions board	= Data.Maybe.isJust maybePieceSquareByCoordinatesByRank ==> Test.QuickCheck.label "EvaluationOptions.prop_sumPieceSquareValueByLogicalColour" . uncurry (==) $ (
			State.CoordinatesByRankByLogicalColour.sumPieceSquareValueByLogicalColour (
				\logicalColour rank coordinatesList	-> Component.PieceSquareByCoordinatesByRank.findPieceSquareValues nPieces logicalColour rank coordinatesList pieceSquareByCoordinatesByRank
			) . State.Board.getCoordinatesByRankByLogicalColour &&& State.MaybePieceByCoordinates.sumPieceSquareValueByLogicalColour (
				\logicalColour rank coordinates		-> Component.PieceSquareByCoordinatesByRank.findPieceSquareValue nPieces logicalColour rank coordinates pieceSquareByCoordinatesByRank
			) . State.Board.getMaybePieceByCoordinates
		 ) board where
			maybePieceSquareByCoordinatesByRank	= Input.EvaluationOptions.getMaybePieceSquareByCoordinatesByRank evaluationOptions
			pieceSquareByCoordinatesByRank		= Data.Maybe.fromJust maybePieceSquareByCoordinatesByRank
			nPieces					= State.Board.getNPieces board
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f
 ]

