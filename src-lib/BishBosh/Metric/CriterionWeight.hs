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

	* Defines the weight associated with some 'Metric.CriterionValue.CriterionValue'.

	* Each weighting is constrained to the unsigned /closed unit-interval/; negative values aren't permitted.

	* If the /criterion-value/ is considered unimportant, then its weight can be set to @0@, whilst concepts of great significance can be set to @1@.
-}

module BishBosh.Metric.CriterionWeight(
-- * Types
-- ** Data-types
	CriterionWeight(
--		MkCriterionWeight,
--		deconstruct
	)
-- * Functions
-- ** Constructor
--	mkCriterionWeight
) where

import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Data.Num		as Data.Num
import qualified	BishBosh.Type.Mass		as Type.Mass
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Default
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT

{- |
	* Quantifies the relative significance of a /criterion-value/; the larger the weight, the more significant the criterion is relative to other criteria.

	* N.B.: the type-constructor is a peg on which to hang class-instances & its data-constructor guards the permissible bounds.
-}
newtype CriterionWeight	= MkCriterionWeight {
	deconstruct	:: Type.Mass.CriterionWeight
} deriving (Eq, Ord)

instance Show CriterionWeight where
	showsPrec precedence (MkCriterionWeight criterionWeight)	= showsPrec precedence criterionWeight

instance Num CriterionWeight where
	MkCriterionWeight l + MkCriterionWeight r	= mkCriterionWeight $ l + r
	MkCriterionWeight l * MkCriterionWeight r	= MkCriterionWeight $ l * r
	abs (MkCriterionWeight criterionWeight)		= MkCriterionWeight $ abs criterionWeight	-- N.B.: if the operand is valid, then this is equivalent to 'id'.
	signum (MkCriterionWeight criterionWeight)	= MkCriterionWeight $ signum criterionWeight
	fromInteger					= mkCriterionWeight . fromInteger
	negate (MkCriterionWeight criterionWeight)	= mkCriterionWeight $ negate criterionWeight	-- CAVEAT: only valid for '0'.

instance Fractional CriterionWeight where
	MkCriterionWeight l / MkCriterionWeight r	= mkCriterionWeight $ l / r	-- CAVEAT: it's hard to concoct a scenario in which neither the numerator, denominator nor result are invalid.
	fromRational					= mkCriterionWeight . fromRational

instance Real CriterionWeight where
	toRational (MkCriterionWeight criterionWeight)	= toRational criterionWeight

instance Bounded CriterionWeight where
	minBound	= MkCriterionWeight 0
	maxBound	= MkCriterionWeight 1

instance Data.Default.Default CriterionWeight where
	def	= minBound

instance Control.DeepSeq.NFData CriterionWeight where
	rnf (MkCriterionWeight criterionWeight)	= Control.DeepSeq.rnf criterionWeight

instance HXT.XmlPickler CriterionWeight where
	xpickle	= HXT.xpWrap (mkCriterionWeight, deconstruct) HXT.xpickle

-- | Smart constructor.
mkCriterionWeight :: Type.Mass.CriterionWeight -> CriterionWeight
mkCriterionWeight criterionWeight
	| Data.Num.inClosedUnitInterval criterionWeight	= MkCriterionWeight criterionWeight
	| otherwise					= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Metric.CriterionWeight.mkCriterionWeight:\t" $ shows criterionWeight " must be within the closed unit-interval '[0,1]'."

