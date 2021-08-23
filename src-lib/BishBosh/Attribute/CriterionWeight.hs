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

	* Defines the weight associated with some 'Attribute.CriterionValue.CriterionValue'.

	* Each weighting is constrained to the unsigned /closed unit-interval/; negative values aren't permitted.

	* If the /criterion-value/ is considered unimportant, then its weight can be set to @0@, whilst concepts of great significance can be set to @1@.

 [@CAVEAT@]

	* While this data-type could implement the classes 'Functor', 'Num', 'Fractional' & 'Real', these interfaces allow one to construct invalid instances.
-}

module BishBosh.Attribute.CriterionWeight(
-- * Types
-- ** Data-types
	CriterionWeight(
--		MkCriterionWeight,
		deconstruct
	),
-- * Functions
-- ** Constructor
	mkCriterionWeight
) where

import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Data.Num		as Data.Num
import qualified	BishBosh.Text.ShowList		as Text.ShowList
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Default
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT

-- | Quantifies the relative significance, of a criterion-value; the larger the weight, the more significant the criterion is relative to other criteria.
newtype CriterionWeight criterionWeight	= MkCriterionWeight {
	deconstruct	:: criterionWeight
} deriving (Eq, Ord)

instance Show criterionWeight => Show (CriterionWeight criterionWeight) where
	showsPrec _ (MkCriterionWeight criterionWeight)	= shows criterionWeight

instance Num criterionWeight => Bounded (CriterionWeight criterionWeight) where
	minBound	= MkCriterionWeight 0
	maxBound	= MkCriterionWeight 1

instance Num criterionWeight => Data.Default.Default (CriterionWeight criterionWeight) where
	def	= minBound

instance Control.DeepSeq.NFData criterionWeight => Control.DeepSeq.NFData (CriterionWeight criterionWeight) where
	rnf (MkCriterionWeight criterionWeight)	= Control.DeepSeq.rnf criterionWeight

instance (
	HXT.XmlPickler	criterionWeight,
	Num		criterionWeight,
	Ord		criterionWeight,
	Show		criterionWeight
 ) => HXT.XmlPickler (CriterionWeight criterionWeight) where
	xpickle	= HXT.xpWrap (mkCriterionWeight, deconstruct) HXT.xpickle

-- | Smart constructor.
mkCriterionWeight :: (
	Num	criterionWeight,
	Ord	criterionWeight,
	Show	criterionWeight
 ) => criterionWeight -> CriterionWeight criterionWeight
mkCriterionWeight criterionWeight
	| Data.Num.inClosedUnitInterval criterionWeight	= MkCriterionWeight criterionWeight
	| otherwise					= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Attribute.CriterionWeight.mkCriterionWeight:\tweight" . Text.ShowList.showsAssociation $ shows criterionWeight " must be within the closed unit-interval, '[0,1]'."

