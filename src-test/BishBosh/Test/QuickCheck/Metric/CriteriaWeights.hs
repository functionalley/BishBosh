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

module BishBosh.Test.QuickCheck.Metric.CriteriaWeights(
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Metric.CriterionWeight()
import qualified	BishBosh.Metric.CriteriaWeights	as Metric.CriteriaWeights
import qualified	Test.QuickCheck

instance Test.QuickCheck.Arbitrary Metric.CriteriaWeights.CriteriaWeights where
	arbitrary	= Metric.CriteriaWeights.mkCriteriaWeights
		<$> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Metric.CriteriaWeights.CriteriaWeights -> Test.QuickCheck.Property
		f criteriaWeights	= Test.QuickCheck.label "CriteriaWeights.prop_normalise" . (== maxBound) . maximum $ map ($ criteriaWeights') [
			Metric.CriteriaWeights.getWeightOfMaterial,
			Metric.CriteriaWeights.getWeightOfMobility,
			Metric.CriteriaWeights.getWeightOfPieceSquareValue,
			Metric.CriteriaWeights.getWeightOfCastlingPotential,
			Metric.CriteriaWeights.getWeightOfDefence,
			Metric.CriteriaWeights.getWeightOfDoubledPawns,
			Metric.CriteriaWeights.getWeightOfIsolatedPawns,
			Metric.CriteriaWeights.getWeightOfPassedPawns
		 ] where
			criteriaWeights'	= Metric.CriteriaWeights.normalise criteriaWeights

	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 32 } f
 ]
