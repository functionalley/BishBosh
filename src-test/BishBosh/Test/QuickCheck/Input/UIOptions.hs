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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary'.
-}

module BishBosh.Test.QuickCheck.Input.UIOptions() where

import			BishBosh.Test.QuickCheck.Input.CECPOptions()
import			BishBosh.Test.QuickCheck.Input.NativeUIOptions()
import			BishBosh.Test.QuickCheck.Input.Verbosity()
import			BishBosh.Test.QuickCheck.Notation.MoveNotation()
import qualified	BishBosh.Input.UIOptions	as Input.UIOptions
import qualified	BishBosh.Notation.MoveNotation	as Notation.MoveNotation
import qualified	Test.QuickCheck

instance (
	Integral	column,
	Integral	row,
	Show		column,
	Show		row
 ) => Test.QuickCheck.Arbitrary (Input.UIOptions.UIOptions row column) where
	arbitrary	= do
		eitherNativeUIOrCECPOptions	<- Test.QuickCheck.arbitrary
		moveNotation			<- const Test.QuickCheck.arbitrary `either` const (return {-to Gen-monad-} Notation.MoveNotation.coordinate) $ eitherNativeUIOrCECPOptions

		Input.UIOptions.mkUIOptions <$> return {-to Gen-monad-} moveNotation <*> fmap (
			fmap $ succ . (`mod` 3)	-- maybePrintMoveTree.
		 ) Test.QuickCheck.arbitrary <*> Test.QuickCheck.elements [1 .. 6] {-NDecimalDigits-} <*> return {-to Gen-monad-} eitherNativeUIOrCECPOptions <*> Test.QuickCheck.arbitrary {-Verbosity-}

