{-# OPTIONS_GHC -fno-warn-orphans #-}
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

 [@DESCRIPTION@]	Defines a /pickler/ for 'Data.Ratio.Ratio'.

 [@CAVEAT@]

	'Rational' numbers are converted imprecisely to floating-point for representation as XML.
	To remedy the loss of precision on reading from XML, they're rounded to the /epsilon/ for IEEE double-precision <https://en.wikipedia.org/wiki/Machine_epsilon>.
-}

module BishBosh.Data.Ratio(
--	round'
) where

import qualified	Data.Ratio
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT

-- | Truncate the rounding-error introduced by conversion to a 'Rational' number.
round' :: RealFrac r => r -> Rational
round'	= (`Data.Ratio.approxRational` doublePrecisionEpsilon) where
	doublePrecisionEpsilon	= recip 2 ^ floatDigits (undefined :: Double)

instance Integral i => HXT.XmlPickler (Data.Ratio.Ratio i) where
	xpickle	= HXT.xpWrap (
		fromRational . round',		-- Construct from a Double.
		\x -> realToFrac x :: Double	-- Deconstruct to a Double.
	 ) HXT.xpPrim

