{-# LANGUAGE CPP #-}
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

 [@DESCRIPTION@]

	* Defines the value of a single /criterion/, which quantifies the significance of some concept;
	many such criteria may exist, & their weighted-mean drives automated selection of /move/s.

	* Each /criterion-value/ is normalised into the /signed closed unit-interval/.

 [@CAVEAT@]

	* While this data-type could implement the classes 'Num', 'Fractional' & 'Real', these interfaces would allow one to construct invalid instances.
-}

module BishBosh.Attribute.CriterionValue(
-- * Types
-- ** Data-types
	CriterionValue(),
-- * Constants
	zero,
-- * Functions
	calculateWeightedMean,
-- ** Constructor
	mkCriterionValue
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.CriterionWeight			as Attribute.CriterionWeight
import qualified	BishBosh.Attribute.WeightedMeanAndCriterionValues	as Attribute.WeightedMeanAndCriterionValues
import qualified	BishBosh.Type.Mass					as Type.Mass
import qualified	Control.Exception
import qualified	Factory.Math.Statistics

#ifdef USE_PARALLEL
import qualified	Control.DeepSeq
import qualified	Control.Parallel.Strategies
#endif

-- | Quantifies some criterion; the larger the signed value, the better.
newtype CriterionValue criterionValue	= MkCriterionValue criterionValue deriving (Eq, Show)

instance Num criterionValue => Bounded (CriterionValue criterionValue) where
	minBound	= MkCriterionValue $ negate 1
	maxBound	= MkCriterionValue 1

-- | Smart constructor.
mkCriterionValue :: (
	Num	criterionValue,
	Ord	criterionValue
 ) => criterionValue -> CriterionValue criterionValue
mkCriterionValue criterionValue	= Control.Exception.assert (abs criterionValue <= 1) $ MkCriterionValue criterionValue

-- | Constant.
zero :: Num criterionValue => CriterionValue criterionValue
zero	= MkCriterionValue 0

{- |
	* Calculates the /weighted mean/ of the specified 'CriterionValue's using the corresponding /criterion-weight/s.

	* Also writes individual unweighted 'CriterionValue's, to facilitate post-analysis;
	if the corresponding weight is @0@, evaluation of the criterion is, for efficiency, avoided.

	* CAVEAT: if all weights are @0@, then the result is indeterminate.
-}
calculateWeightedMean :: (
#ifdef USE_PARALLEL
	Control.DeepSeq.NFData	criterionValue,
#endif
	Fractional		weightedMean,
	Real			criterionValue,
	Real			criterionWeight
 ) => [(CriterionValue criterionValue, Attribute.CriterionWeight.CriterionWeight criterionWeight)] -> Attribute.WeightedMeanAndCriterionValues.WeightedMeanAndCriterionValues weightedMean criterionValue
{-# INLINABLE calculateWeightedMean #-}
{-# SPECIALISE calculateWeightedMean :: [(CriterionValue Type.Mass.CriterionValue, Attribute.CriterionWeight.CriterionWeight Type.Mass.CriterionWeight)] -> Attribute.WeightedMeanAndCriterionValues.WeightedMeanAndCriterionValues Type.Mass.WeightedMean Type.Mass.CriterionValue #-}
calculateWeightedMean assocs	= uncurry Attribute.WeightedMeanAndCriterionValues.mkWeightedMeanAndCriterionValues $ (
	Factory.Math.Statistics.getWeightedMean &&& map fst
 )
#ifdef USE_PARALLEL
	$ Control.Parallel.Strategies.withStrategy (
		Control.Parallel.Strategies.parList $ Control.Parallel.Strategies.evalTuple2 Control.Parallel.Strategies.rdeepseq Control.Parallel.Strategies.r0
	)
#endif
	[
		(bareCriterionValue, bareCriterionWeight) |
			(MkCriterionValue bareCriterionValue, criterionWeight)	<- assocs,
			let bareCriterionWeight	= Attribute.CriterionWeight.deconstruct criterionWeight,
			bareCriterionWeight /= 0
	] -- List-comprehension.

