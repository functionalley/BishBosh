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

module BishBosh.Attribute.WeightedMeanAndCriterionValues(
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
-- ** Constructor
	mkWeightedMeanAndCriterionValues
) where

import qualified	BishBosh.Property.ShowFloat	as Property.ShowFloat
import qualified	BishBosh.Text.ShowList		as Text.ShowList
import qualified	Control.DeepSeq

-- | Qualifies output.
criterionValuesTag :: String
criterionValuesTag	= "criterion-values"

-- | Qualifies output.
weightedMeanTag :: String
weightedMeanTag		= "weighted-mean"

-- | A /weighted mean/ & the individual unweighted criterion-value from which it was composed.
data WeightedMeanAndCriterionValues weightedMean criterionValue	= MkWeightedMeanAndCriterionValues {
	getWeightedMean		:: weightedMean,	-- ^ The weighted mean of a list of criterion-values.
	getCriterionValues	:: [criterionValue]	-- ^ The unweighted 'CriterionValue's from which the weighted mean was composed.
} deriving (Eq, Show)

instance Control.DeepSeq.NFData weightedMean => Control.DeepSeq.NFData (WeightedMeanAndCriterionValues weightedMean criterionValue) where
	rnf MkWeightedMeanAndCriterionValues { getWeightedMean = weightedMean }	= Control.DeepSeq.rnf weightedMean	-- The other field is a prerequisite.

instance (Real criterionValue, Real weightedMean) => Property.ShowFloat.ShowFloat (WeightedMeanAndCriterionValues weightedMean criterionValue) where
	showsFloat fromDouble MkWeightedMeanAndCriterionValues {
		getWeightedMean		= weightedMean,
		getCriterionValues	= criterionValues
	} = Text.ShowList.showsAssociationList' [
		(weightedMeanTag, fromDouble $ realToFrac weightedMean),
		(criterionValuesTag, Text.ShowList.showsFormattedList' (fromDouble . realToFrac) criterionValues)
	 ]

-- | Constructor
mkWeightedMeanAndCriterionValues :: weightedMean -> [criterionValue] -> WeightedMeanAndCriterionValues weightedMean criterionValue
mkWeightedMeanAndCriterionValues	= MkWeightedMeanAndCriterionValues

{- |
	* Negate the weightedMean, but leave the criterion-values unaltered.

	* This can be used to assess the fitness of a position from the perspective of one's opponent.
-}
negateWeightedMean :: Num weightedMean => WeightedMeanAndCriterionValues weightedMean criterionValue -> WeightedMeanAndCriterionValues weightedMean criterionValue
negateWeightedMean weightedMeanAndCriterionValues@MkWeightedMeanAndCriterionValues { getWeightedMean = weightedMean }	= weightedMeanAndCriterionValues { getWeightedMean = negate weightedMean }

