{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
	Copyright (C) 2021 Dr. Alistair Ward

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

	* Defines distinct types for various mass (uncountable) quantities.

	* These prevent accidental conflation between conceptually different quantities.
-}

module BishBosh.Type.Mass(
-- * Types
-- ** Type-synonyms
	CriterionWeight,
	CriterionValue,
	WeightedMean,
	RankValue,
	PieceSquareValue
) where

#ifdef USE_PRECISION
import			BishBosh.Data.Ratio()
#else
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
#endif

-- | The preferred type by which to weight criteria.
type CriterionWeight	=
#ifdef USE_PRECISION
	Rational
#else /* Floating-point */
#	ifdef USE_NARROW_NUMBERS
	Float

instance HXT.XmlPickler Float where
	xpickle	= HXT.xpPrim
#	else
	Double

instance HXT.XmlPickler Double where
	xpickle	= HXT.xpPrim
#	endif
#endif

-- | The preferred type by which to value criteria.
type CriterionValue	= CriterionWeight

-- | The preferred type by which to represent fitness.
type WeightedMean	= CriterionValue

-- | The preferred type by which to represent the value of a /rank/.
type RankValue		= CriterionValue

{- |
	* The preferred type by which to represent a /piece-square/ value.

	* CAVEAT: performance of 'BishBosh.Evaluation.Fitness.interpolatePieceSquareValues' suffers from use of 'Rational'.
-}
type PieceSquareValue	= CriterionValue

