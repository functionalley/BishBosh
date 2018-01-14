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

module BishBosh.Test.QuickCheck.Input.IOOptions() where

import			BishBosh.Test.QuickCheck.Input.PGNOptions()
import			BishBosh.Test.QuickCheck.Input.UIOptions()
import qualified	BishBosh.Input.IOOptions	as Input.IOOptions
import qualified	Control.Arrow
import qualified	Data.Maybe
import qualified	System.FilePath
import qualified	Test.QuickCheck
import			System.FilePath((</>), (<.>))

instance (
	Integral	column,
	Integral	row,
	Show		column,
	Show		row
 ) => Test.QuickCheck.Arbitrary (Input.IOOptions.IOOptions row column) where
	arbitrary	= do
		maybePGNOptions	<- Test.QuickCheck.arbitrary

		Input.IOOptions.mkIOOptions <$> Test.QuickCheck.elements (
			map (
				fmap $ showChar System.FilePath.pathSeparator	-- Make path absolute.
			) [
				Nothing,
				Just $ "dev" </> "null",
				Just $ "tmp" </> "chess" <.> "xml"
			] -- MaybeOutputConfigFilePath.
		 ) <*> (
			if Data.Maybe.isJust maybePGNOptions
				then Test.QuickCheck.elements [Nothing, Just 0, Just 10] {-maybeMaximumPGNNames-}
				else return {-to Gen-monad-} Nothing
		 ) <*> return {-to Gen-monad-} (Data.Maybe.maybeToList maybePGNOptions) <*> Test.QuickCheck.elements (
			map (
				fmap . Control.Arrow.first $ showChar System.FilePath.pathSeparator	-- Make path absolute.
			) [
				Nothing,
				Just ("dev" </> "null", False),
				Just ("tmp" </> "chess" <.> "txt", False),
				Just ("tmp" </> "chess" <.> "txt", True)
			] {-MaybePersistence-}
		 ) <*> Test.QuickCheck.arbitrary {-UI-}

