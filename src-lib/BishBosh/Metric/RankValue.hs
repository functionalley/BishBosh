{-# LANGUAGE CPP #-}
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

	* The relative value of a /rank/ of chess-piece.

	* <https://en.wikipedia.org/wiki/Chess_piece_relative_value#Hans_Berliner.27s_system%20Chess-piece%20relative%20values>

	* CAVEAT: this isn't measured in conventional centi-pawns units; values are constrained to the closed unit interval, & there's no requirement /Pawn/'s value to be @1@.
-}

module BishBosh.Metric.RankValue(
-- * Types
#ifdef USE_NEWTYPE_WRAPPERS
-- ** Data-types
	RankValue(
--		MkRankValue,
--		deconstruct
	),
-- * Functions
-- ** Constructor
--	mkRankValue,
#else
-- ** Type-synonyms
	RankValue,
#endif
-- * Constants
	tag
) where

import qualified	BishBosh.Type.Mass		as Type.Mass

#ifdef USE_NEWTYPE_WRAPPERS
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Data.Num		as Data.Num
import qualified	BishBosh.Property.ShowFloat	as Property.ShowFloat
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.List.Extra
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
#endif

-- | Used to qualify XML.
tag :: String
tag	= "rankValue"

-- | The constant value associated with a /rank/; the higher, the more valuable it is considered to be.
#ifdef USE_NEWTYPE_WRAPPERS
newtype RankValue = MkRankValue {
	deconstruct	:: Type.Mass.RankValue
} deriving (Eq, Ord)

instance Show RankValue where
	showsPrec precision MkRankValue { deconstruct = rankValue }	= showsPrec precision rankValue

instance Read RankValue where
	readsPrec precision	= map (Control.Arrow.first mkRankValue) . readsPrec precision . Data.List.Extra.trimStart

instance Property.ShowFloat.ShowFloat RankValue where
	showsFloat fromDouble MkRankValue { deconstruct = rankValue }	= fromDouble $! realToFrac rankValue

instance Num RankValue where
	MkRankValue { deconstruct = l } + MkRankValue { deconstruct = r }	= mkRankValue $! l + r
	MkRankValue { deconstruct = l } * MkRankValue { deconstruct = r }	= MkRankValue $! l * r
	abs MkRankValue { deconstruct = rankValue }				= MkRankValue $! abs rankValue		-- N.B.: if the operand is valid, then this is equivalent to 'id'.
	signum MkRankValue { deconstruct = rankValue }				= MkRankValue $! signum rankValue
	fromInteger								= mkRankValue . fromInteger
	negate MkRankValue { deconstruct = rankValue }				= mkRankValue $! negate rankValue	-- CAVEAT: only valid for '0'.

instance Fractional RankValue where
	MkRankValue { deconstruct = l } / MkRankValue { deconstruct = r }	= mkRankValue $! l / r	-- CAVEAT: it's hard to concoct a scenario in which neither the numerator, denominator nor result are invalid.
	fromRational								= mkRankValue . fromRational

instance Real RankValue where
	toRational MkRankValue { deconstruct = rankValue }	= toRational rankValue

instance Control.DeepSeq.NFData RankValue where
	rnf MkRankValue { deconstruct = rankValue }	= Control.DeepSeq.rnf rankValue

instance HXT.XmlPickler RankValue where
	xpickle	= HXT.xpWrap (mkRankValue, deconstruct) $! HXT.xpAttr tag HXT.xpickle

-- | Smart constructor.
mkRankValue :: Type.Mass.RankValue -> RankValue
mkRankValue rankValue
	| Data.Num.inClosedUnitInterval rankValue	= MkRankValue rankValue
	| otherwise					= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Metric.RankValue.mkRankValue:\t" $ shows rankValue " must be within the closed unit-interval [0,1]."
#else
type RankValue	 = Type.Mass.Base
#endif

