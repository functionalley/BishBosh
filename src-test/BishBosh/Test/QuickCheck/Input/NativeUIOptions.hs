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

module BishBosh.Test.QuickCheck.Input.NativeUIOptions() where

import			Control.Arrow((***))
import qualified	BishBosh.Input.NativeUIOptions	as Input.NativeUIOptions
import qualified	Data.Default
import qualified	Test.QuickCheck

instance Test.QuickCheck.Arbitrary Input.NativeUIOptions.NativeUIOptions where
	arbitrary	= Input.NativeUIOptions.mkNativeUIOptions <$> fmap (
		mkOddWholeNumber *** mkOddWholeNumber
	 ) Test.QuickCheck.arbitrary {-BoardMagnification-} <*> return {-to Gen-monad-} Data.Default.def {-ColourScheme-} where
		mkOddWholeNumber :: Num n => Integer -> n
		mkOddWholeNumber	= fromInteger . succ . (* 2) . (`mod` 3)

