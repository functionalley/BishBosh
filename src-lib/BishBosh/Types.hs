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

	* Defines suitable concrete types with which to specialise type-parameters.

	* CAVEAT: use of the narrow set of numeric types, results in marginally slower performance without any reduction in space-requirements.
-}

module BishBosh.Types(
-- * Types
-- ** Type-synonyms
	CriterionWeight,
	CriterionValue,
	WeightedMean,
	X,
	Y,
	Distance,
	RankValue,
	PieceSquareValue,
	PositionHash
) where

import qualified	Data.Word

#ifdef USE_NARROW_NUMBERS
import qualified	Data.Int
#endif

-- | The preferred type by which to weight criteria.
type CriterionWeight	=
#ifdef USE_NARROW_NUMBERS
	Float
#else
	Double	-- N.B.: 'Rational' is more accurate, but slower alternative.
#endif

-- | The preferred type by which to value criteria.
type CriterionValue	= CriterionWeight

-- | The preferred type by which to represent fitness.
type WeightedMean	= CriterionValue

-- | The preferred type by which to represent the abscissa. CAVEAT: while conceptually unsigned, various unguarded calls to 'pred' prevent this.
type X	=
#ifdef USE_NARROW_NUMBERS
	Data.Int.Int8
#else
	Int
#endif

-- | The preferred type by which to represent the ordinate.
type Y	= X	-- N.B.: it can be independent of 'X'.

{- |
	* The preferred type by which to represent the signed distance of a move.

	* N.B.: since /distance/ is used to represent only the horizontal or vertical component of a move, rather than a diagonal length, it can be represented by an integral value.
-}
type Distance	= X	-- N.B.: conceptually independent of both 'X' & 'Y' which could be unsigned.

-- | The preferred type by which to represent the value of a /rank/.
type RankValue	= CriterionValue

{- |
	* The preferred type by which to represent a /piece-square/ value.

	* CAVEAT: performance of 'BishBosh.Evaluation.Fitness.interpolatePieceSquareValues' suffers from use of 'Rational'.
-}
type PieceSquareValue	= CriterionValue

-- | The type of the hash used to uniquely represent a /position/.
type PositionHash	=
#ifdef USE_NARROW_NUMBERS
	Data.Word.Word32	-- CAVEAT: hash-collisions become almost inevitable after @ sqrt bits @ trials.
#else
	Data.Word.Word
#endif

