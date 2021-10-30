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

import			BishBosh.Test.QuickCheck.Input.CriteriaWeights()
import			BishBosh.Test.QuickCheck.Input.PieceSquareTable()
import			BishBosh.Test.QuickCheck.Input.RankValues()
import			BishBosh.Test.QuickCheck.State.Board()
import			Control.Arrow((&&&))
import qualified	BishBosh.Component.Accountant		as Component.Accountant
import qualified	BishBosh.Input.CriteriaWeights		as Input.CriteriaWeights
import qualified	BishBosh.Input.EvaluationOptions	as Input.EvaluationOptions
import qualified	BishBosh.Input.PieceSquareTable		as Input.PieceSquareTable
import qualified	BishBosh.Input.RankValues		as Input.RankValues
import qualified	BishBosh.Property.Reflectable		as Property.Reflectable
import qualified	BishBosh.State.Board			as State.Board
import qualified	BishBosh.Type.Mass			as Type.Mass
import qualified	Data.Array.IArray
import qualified	Data.List
import qualified	Data.Map.Strict				as Map
import qualified	Data.Maybe
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

-- | Defines a concrete type for testing.
type EvaluationOptions	= Input.EvaluationOptions.EvaluationOptions Type.Mass.PieceSquareValue

instance (
	Fractional	pieceSquareValue,
	Ord		pieceSquareValue,
	Show		pieceSquareValue
 ) => Test.QuickCheck.Arbitrary (Input.EvaluationOptions.EvaluationOptions pieceSquareValue) where
	{-# SPECIALISE instance Test.QuickCheck.Arbitrary EvaluationOptions #-}
	arbitrary	= do
		rankValues	<- Test.QuickCheck.arbitrary
		criteriaWeights	<- Test.QuickCheck.suchThat Test.QuickCheck.arbitrary $ \c -> Input.RankValues.calculateMaximumTotalValue rankValues /= 0 || Input.CriteriaWeights.getWeightOfMaterial c == 0

		Input.EvaluationOptions.mkEvaluationOptions rankValues criteriaWeights <$> Test.QuickCheck.arbitrary {-incrementalEvaluation-} <*> if Input.CriteriaWeights.getWeightOfPieceSquareValue criteriaWeights == minBound
			then return {-to Gen-monad-} Nothing
			else do
				(pieceSquareTable, pieceSquareTable', ranks)	<- Test.QuickCheck.arbitrary

				return {-to Gen-monad-} $ Just (
					pieceSquareTable,
					pieceSquareTable' {
						Input.PieceSquareTable.getPieceSquareValueByCoordinatesByRank	= foldr Map.delete (Input.PieceSquareTable.getPieceSquareValueByCoordinatesByRank pieceSquareTable') $ Data.List.nub ranks	-- Delete arbitrary ranks from the end-game table.
					}
				 ) -- Pair.

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: EvaluationOptions -> State.Board.Board -> Test.QuickCheck.Property
		f evaluationOptions board	= Data.Maybe.isJust maybePieceSquareByCoordinatesByRank ==> Test.QuickCheck.label "EvaluationOptions.prop_sumPieceSquareValueByLogicalColour/reflectOnX" . uncurry (==) $ (
			Data.Array.IArray.elems . sumPieceSquareValueByLogicalColour &&& reverse . Data.Array.IArray.elems . sumPieceSquareValueByLogicalColour . Property.Reflectable.reflectOnX
		 ) board where
			maybePieceSquareByCoordinatesByRank	= Input.EvaluationOptions.getMaybePieceSquareByCoordinatesByRank evaluationOptions
			sumPieceSquareValueByLogicalColour	= State.Board.sumPieceSquareValueByLogicalColour $ Data.Maybe.fromJust maybePieceSquareByCoordinatesByRank
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 32 } f,
	let
		f :: Input.EvaluationOptions.EvaluationOptions Rational {-precision is required-} -> State.Board.Board -> Test.QuickCheck.Property
		f evaluationOptions board	= Data.Maybe.isJust maybePieceSquareByCoordinatesByRank ==> Test.QuickCheck.label "EvaluationOptions.prop_sumPieceSquareValueByLogicalColour" . uncurry (==) $ (
			Component.Accountant.sumPieceSquareValueByLogicalColour nPieces pieceSquareByCoordinatesByRank . State.Board.getCoordinatesByRankByLogicalColour &&& Component.Accountant.sumPieceSquareValueByLogicalColour nPieces pieceSquareByCoordinatesByRank . State.Board.getMaybePieceByCoordinates
		 ) board where
			maybePieceSquareByCoordinatesByRank	= Input.EvaluationOptions.getMaybePieceSquareByCoordinatesByRank evaluationOptions
			pieceSquareByCoordinatesByRank		= Data.Maybe.fromJust maybePieceSquareByCoordinatesByRank
			nPieces					= State.Board.getNPieces board
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f
 ]

