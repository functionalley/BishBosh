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

module BishBosh.Test.QuickCheck.Input.CriteriaWeights(
-- * Types
-- ** Type-synonyms
--	CriteriaWeights,
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Attribute.CriterionWeight()
import qualified	BishBosh.Input.CriteriaWeights	as Input.CriteriaWeights
import qualified	BishBosh.Type.Mass		as Type.Mass
import qualified	Test.QuickCheck

instance (
	Fractional	criterionWeight,
	Ord		criterionWeight,
	Show		criterionWeight
 ) => Test.QuickCheck.Arbitrary (Input.CriteriaWeights.CriteriaWeights criterionWeight) where
	arbitrary	= Input.CriteriaWeights.mkCriteriaWeights <$> Test.QuickCheck.arbitrary <*> Test.QuickCheck.arbitrary <*> Test.QuickCheck.arbitrary <*> Test.QuickCheck.arbitrary <*> Test.QuickCheck.arbitrary <*> Test.QuickCheck.arbitrary <*> Test.QuickCheck.arbitrary <*> Test.QuickCheck.arbitrary

-- | Defines a concrete type for testing.
type CriteriaWeights	= Input.CriteriaWeights.CriteriaWeights Type.Mass.CriterionWeight

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: CriteriaWeights -> Test.QuickCheck.Property
		f criteriaWeights	= Test.QuickCheck.label "CriteriaWeights.prop_normalise" . (== maxBound) . maximum $ map ($ criteriaWeights') [
			Input.CriteriaWeights.getWeightOfMaterial,
			Input.CriteriaWeights.getWeightOfMobility,
			Input.CriteriaWeights.getWeightOfPieceSquareValue,
			Input.CriteriaWeights.getWeightOfCastlingPotential,
			Input.CriteriaWeights.getWeightOfDefence,
			Input.CriteriaWeights.getWeightOfDoubledPawns,
			Input.CriteriaWeights.getWeightOfIsolatedPawns,
			Input.CriteriaWeights.getWeightOfPassedPawns
		 ] where
			criteriaWeights'	= Input.CriteriaWeights.normalise criteriaWeights

	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 32 } f
 ]
