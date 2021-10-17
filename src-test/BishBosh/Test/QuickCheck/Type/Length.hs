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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary'
-}

module BishBosh.Test.QuickCheck.Type.Length() where

#ifdef USE_NEWTYPE_WRAPPERS
import qualified	BishBosh.Cartesian.Abscissa	as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Ordinate	as Cartesian.Ordinate
import qualified	BishBosh.Type.Length		as Type.Length
import qualified	Test.QuickCheck

instance Test.QuickCheck.Arbitrary Type.Length.X where
	arbitrary	= Test.QuickCheck.elements Cartesian.Abscissa.xRange

instance Test.QuickCheck.Arbitrary Type.Length.Y where
	arbitrary	= Test.QuickCheck.elements Cartesian.Ordinate.yRange
#endif

