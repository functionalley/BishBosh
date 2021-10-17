{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
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

	* Defines distinct types for various conceptually different length-related quantities to prevent accidental conflation.

	* Nothing but the type is exported, facilitating reversion to unwrapped types.

	* CAVEAT: use of narrow numeric types, results in marginally slower performance without any reduction in space-requirements.
-}

module BishBosh.Type.Length(
-- * Types
-- ** Type-synonyms
--	Base,
-- ** Data-types
	X,
	Y,
	Row,
	Column
) where

#if defined(USE_NARROW_NUMBERS) || defined(USE_NEWTYPE_WRAPPERS)
#	ifdef USE_NARROW_NUMBERS
import qualified	Data.Int
#	endif

#	ifdef USE_NEWTYPE_WRAPPERS
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Data.Array.IArray
#	endif
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
#endif

-- | The private type which is wrapped by various length-related data-types.
type Base	=
#ifdef USE_NARROW_NUMBERS
	Data.Int.Int8

instance HXT.XmlPickler Data.Int.Int8 where
	xpickle	= HXT.xpPrim
#else
	Int
#endif

-- | The board-abscissa.
#ifdef USE_NEWTYPE_WRAPPERS
newtype X	= MkX Base deriving (Control.DeepSeq.NFData, Data.Array.IArray.Ix, Enum, Eq, Integral, Num, Ord, Real)

instance Read X where
	readsPrec precision	= map (Control.Arrow.first MkX) . readsPrec precision

instance Show X where
	showsPrec precision (MkX x)	= showsPrec precision x
#else
type X		= Base
#endif

-- | The board-ordinate; independent of /X/.
#ifdef USE_NEWTYPE_WRAPPERS
newtype Y	= MkY Base deriving (Control.DeepSeq.NFData, Enum, Eq, Integral, Num, Ord, Real)

instance Read Y where
	readsPrec precision	= map (Control.Arrow.first MkY) . readsPrec precision

instance Show Y where
	showsPrec precision (MkY y)	= showsPrec precision y
#else
type Y		= Base
#endif

-- | Indexes screen-coordinates in the vertical direction.
#ifdef USE_NEWTYPE_WRAPPERS
newtype Row	= MkRow Base deriving (Control.DeepSeq.NFData, Enum, Eq, HXT.XmlPickler, Integral, Num, Ord, Real)

instance Show Row where
	showsPrec precision (MkRow row)	= showsPrec precision row
#else
type Row	= Base
#endif

-- | Indexes screen-coordinates in the horizontal direction.
#ifdef USE_NEWTYPE_WRAPPERS
newtype Column	= MkColumn Base deriving (Control.DeepSeq.NFData, Enum, Eq, HXT.XmlPickler, Integral, Num, Ord, Real)

instance Show Column where
	showsPrec precision (MkColumn column)	= showsPrec precision column
#else
type Column	= Base
#endif

