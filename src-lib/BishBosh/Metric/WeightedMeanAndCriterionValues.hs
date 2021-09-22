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

 [@DESCRIPTION@]	The unweighted values of each criterion used to assess the fitness of a position, & the resulting weighted mean.
-}

module BishBosh.Metric.WeightedMeanAndCriterionValues(
-- * Types
-- ** Data-types
	WeightedMeanAndCriterionValues(
--		MkWeightedMeanAndCriterionValues,
		getWeightedMean,
		getCriterionValues
	),
-- * Constants
	criterionValuesTag,
	weightedMeanTag,
-- * Functions
	negateWeightedMean,
	calculateWeightedMean,
-- ** Constructor
	mkWeightedMeanAndCriterionValues
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Metric.CriterionValue	as Metric.CriterionValue
import qualified	BishBosh.Metric.CriterionWeight	as Metric.CriterionWeight
import qualified	BishBosh.Property.ShowFloat	as Property.ShowFloat
import qualified	BishBosh.Text.ShowList		as Text.ShowList
import qualified	BishBosh.Type.Mass		as Type.Mass
import qualified	Control.DeepSeq
import qualified	Factory.Math.Statistics

#ifdef USE_PARALLEL
import qualified	Control.Parallel.Strategies
#endif

-- | Qualifies output.
criterionValuesTag :: String
criterionValuesTag	= "criterion-values"

-- | Qualifies output.
weightedMeanTag :: String
weightedMeanTag		= "weighted-mean"

-- | A /weighted mean/ & the individual unweighted criterion-values from which it was composed.
data WeightedMeanAndCriterionValues	= MkWeightedMeanAndCriterionValues {
	getWeightedMean		:: Type.Mass.WeightedMean,			-- ^ The weighted mean of a list of criterion-values.
	getCriterionValues	:: [Metric.CriterionValue.CriterionValue]	-- ^ The unweighted /CriterionValue/s from which the weighted mean was composed.
} deriving (Eq, Show)

instance Control.DeepSeq.NFData WeightedMeanAndCriterionValues where
	rnf MkWeightedMeanAndCriterionValues { getWeightedMean = weightedMean }	= Control.DeepSeq.rnf weightedMean	-- The other field is a prerequisite.

instance Property.ShowFloat.ShowFloat WeightedMeanAndCriterionValues where
	showsFloat fromDouble MkWeightedMeanAndCriterionValues {
		getWeightedMean		= weightedMean,
		getCriterionValues	= criterionValues
	} = Text.ShowList.showsAssociationList' [
		(weightedMeanTag, fromDouble $ realToFrac weightedMean),
		(criterionValuesTag, Text.ShowList.showsFormattedList' (fromDouble . realToFrac) criterionValues)
	 ]

-- | Constructor.
mkWeightedMeanAndCriterionValues
	:: Type.Mass.WeightedMean
	-> [Metric.CriterionValue.CriterionValue]
	-> WeightedMeanAndCriterionValues
mkWeightedMeanAndCriterionValues	= MkWeightedMeanAndCriterionValues

{- |
	* Negate the /weightedMean/, but leave the criterion-values unaltered.

	* This can be used to assess the fitness of a position from the perspective of one's opponent.
-}
negateWeightedMean :: WeightedMeanAndCriterionValues -> WeightedMeanAndCriterionValues
negateWeightedMean weightedMeanAndCriterionValues@MkWeightedMeanAndCriterionValues { getWeightedMean = weightedMean }	= weightedMeanAndCriterionValues { getWeightedMean = negate weightedMean }

{- |
	* Calculates the /weighted mean/ of the specified /criterion-value/s using the corresponding /criterion-weight/s.

	* Also writes individual unweighted /criterionValue/s, to facilitate post-analysis;
	if the corresponding weight is @0@, evaluation of the criterion is avoided, for efficiency.

	* CAVEAT: if all weights are @0@, then the result is indeterminate.
-}
calculateWeightedMean :: [(Metric.CriterionValue.CriterionValue, Metric.CriterionWeight.CriterionWeight)] -> WeightedMeanAndCriterionValues
{-# INLINABLE calculateWeightedMean #-}
calculateWeightedMean	= uncurry mkWeightedMeanAndCriterionValues . (
	Factory.Math.Statistics.getWeightedMean &&& map (realToFrac . fst)
 )
#ifdef USE_PARALLEL
	. Control.Parallel.Strategies.withStrategy (
		Control.Parallel.Strategies.parList $ Control.Parallel.Strategies.evalTuple2 Control.Parallel.Strategies.rdeepseq Control.Parallel.Strategies.r0
	)
#endif
	. map (
		\(criterionValue, criterionWeight) -> (realToFrac criterionValue, realToFrac criterionWeight) :: (Type.Mass.CriterionValue, Type.Mass.CriterionWeight)
	) . filter (
		(/= 0) . snd {-criterion-weight-}	-- Avoid unnecessaily evaluating criterion-values.
	)

