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
	Base,
	CriterionWeight,
	CriterionValue,
	WeightedMean,
	RankValue,
	PieceSquareValue(),
-- * Functions
-- ** Constructors
--	mkPieceSquareValue
) where

#ifdef USE_NEWTYPE_WRAPPERS
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Data.Num		as Data.Num
import qualified	Control.DeepSeq
import qualified	Control.Exception
#endif

#ifdef USE_PRECISE_NUMBERS
import			BishBosh.Data.Ratio()
#else
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
#endif

-- | The underling type.
type Base	=
#ifdef USE_PRECISE_NUMBERS
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

-- | The type by which to weight criteria.
type CriterionWeight	= Base

-- | The type by which to value criteria.
type CriterionValue	= CriterionWeight

-- | The type by which to represent fitness.
type WeightedMean	= CriterionValue

-- | The type by which to represent the value of a /rank/.
type RankValue		= Base

-- | The type by which to represent a /piece-square/ value.
#ifdef USE_NEWTYPE_WRAPPERS
newtype PieceSquareValue = MkPieceSquareValue Base deriving (Eq, Ord)

instance Show PieceSquareValue where
	showsPrec precision (MkPieceSquareValue pieceSquareValue)	= showsPrec precision pieceSquareValue

instance Num PieceSquareValue where
	MkPieceSquareValue l + MkPieceSquareValue r	= mkPieceSquareValue $! l + r
	MkPieceSquareValue l * MkPieceSquareValue r	= MkPieceSquareValue $! l * r
	abs (MkPieceSquareValue pieceSquareValue)	= MkPieceSquareValue $! abs pieceSquareValue	-- N.B.: if the operand is valid, then this is equivalent to 'id'.
	signum (MkPieceSquareValue pieceSquareValue)	= MkPieceSquareValue $! signum pieceSquareValue
	fromInteger					= mkPieceSquareValue . fromInteger
	negate (MkPieceSquareValue pieceSquareValue)	= mkPieceSquareValue $! negate pieceSquareValue	-- CAVEAT: only valid for '0'.

instance Fractional PieceSquareValue where
	MkPieceSquareValue l / MkPieceSquareValue r	= mkPieceSquareValue $! l / r	-- CAVEAT: it's hard to concoct a scenario in which neither the numerator, denominator nor result are invalid.
	fromRational					= mkPieceSquareValue . fromRational

instance Real PieceSquareValue where
	toRational (MkPieceSquareValue pieceSquareValue)	= toRational pieceSquareValue

instance Control.DeepSeq.NFData PieceSquareValue where
	rnf (MkPieceSquareValue pieceSquareValue)	= Control.DeepSeq.rnf pieceSquareValue

-- | Smart constructor.
mkPieceSquareValue :: Base -> PieceSquareValue
mkPieceSquareValue pieceSquareValue
	| Data.Num.inClosedUnitInterval pieceSquareValue	= MkPieceSquareValue pieceSquareValue
	| otherwise						= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Type.Mass.mkPieceSquareValue:\t" $ shows pieceSquareValue " must be within the closed unit-interval [0,1]."
#else
type PieceSquareValue	= Base
#endif

