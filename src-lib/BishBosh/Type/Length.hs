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

	* Defines suitable concrete types with which to specialise length-related type-parameters.

	* CAVEAT: use of narrow numeric types, results in marginally slower performance without any reduction in space-requirements.
-}

module BishBosh.Type.Length(
-- * Types
-- ** Type-synonyms
--	Base,
	Distance,
	X,
	Y,
	Row(),
	Column()
) where

#if defined(USE_NARROW_NUMBERS) || defined(USE_NEWTYPE_WRAPPERS)
#	ifdef USE_NARROW_NUMBERS
import qualified	Data.Int
#	endif

#	ifdef USE_NEWTYPE_WRAPPERS
import qualified	Control.DeepSeq
#	endif
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
#endif

-- | The preferred type by which to represent the abscissa. CAVEAT: while conceptually unsigned, various unguarded calls to 'pred' prevent this.
type Base	=
#ifdef USE_NARROW_NUMBERS
	Data.Int.Int8

instance HXT.XmlPickler Data.Int.Int8 where
	xpickle	= HXT.xpPrim
#else
	Int
#endif

{- |
	* The preferred type by which to represent the signed distance of a move.

	* N.B.: since /distance/ is used to represent only the horizontal or vertical component of a move, rather than a diagonal length, it can be represented by an integral value.
-}
type Distance	= Base	-- N.B.: conceptually independent of both 'X' & 'Y' which could be unsigned.

-- | The distance along the abscissa.
type X	= Base

-- | The distance along the ordinate.
type Y	= Base	-- N.B.: it can be independent of 'X'.

-- | Indexes screen-coordinates in the vertical direction.
#ifdef USE_NEWTYPE_WRAPPERS
newtype Row	= MkRow Y deriving (Control.DeepSeq.NFData, Enum, Eq, HXT.XmlPickler, Integral, Num, Ord, Real)

instance Show Row where
	showsPrec precision (MkRow row)	= showsPrec precision row
#else
type Row	= Y
#endif

-- | Indexes screen-coordinates in the horizontal direction.
#ifdef USE_NEWTYPE_WRAPPERS
newtype Column	= MkColumn X deriving (Control.DeepSeq.NFData, Enum, Eq, HXT.XmlPickler, Integral, Num, Ord, Real)

instance Show Column where
	showsPrec precision (MkColumn column)	= showsPrec precision column
#else
type Column	= X
#endif

