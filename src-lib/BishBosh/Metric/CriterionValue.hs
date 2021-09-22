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
-}

module BishBosh.Metric.CriterionValue(
-- * Types
-- ** Data-types
	CriterionValue(),
-- ** Constructor
--	mkCriterionValue
) where

import qualified	BishBosh.Type.Mass	as Type.Mass

#ifdef USE_NEWTYPE_WRAPPERS
import qualified	BishBosh.Data.Exception	as Data.Exception
import qualified	Control.Exception

{- |
	* Quantifies some criterion; the larger the signed value, the better.

	* N.B.: the type-constructor is a peg on which to hang class-instances & its data-constructor guards the permissible bounds.
-}
newtype CriterionValue	= MkCriterionValue Type.Mass.CriterionValue deriving (Eq, Ord)

instance Show CriterionValue where
	showsPrec precedence (MkCriterionValue criterionValue)	= showsPrec precedence criterionValue

instance Num CriterionValue where
	MkCriterionValue l + MkCriterionValue r		= mkCriterionValue $ l + r
	MkCriterionValue l * MkCriterionValue r		= MkCriterionValue $ l * r
	abs (MkCriterionValue criterionValue)		= MkCriterionValue $ abs criterionValue
	signum (MkCriterionValue criterionValue)	= MkCriterionValue $ signum criterionValue
	fromInteger					= mkCriterionValue . fromInteger
	negate (MkCriterionValue criterionValue)	= MkCriterionValue $ negate criterionValue

instance Fractional CriterionValue where
	MkCriterionValue l / MkCriterionValue r	= mkCriterionValue $ l / r	-- CAVEAT: it's hard to concoct a scenario in which neither the numerator, denominator nor result are invalid.
	fromRational				= mkCriterionValue . fromRational

instance Real CriterionValue where
	toRational (MkCriterionValue criterionValue)	= toRational criterionValue

-- | Smart constructor.
mkCriterionValue :: Type.Mass.CriterionValue -> CriterionValue
mkCriterionValue criterionValue
	| abs criterionValue > 1	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Metric.CriterionValue.mkCriterionValue:\t" $ shows criterionValue " must be within the closed interval '[-1,1]'."
	| otherwise			= MkCriterionValue criterionValue
#else
-- | Quantifies some criterion; the larger the signed value, the better.
type CriterionValue	= Type.Mass.CriterionValue
#endif
