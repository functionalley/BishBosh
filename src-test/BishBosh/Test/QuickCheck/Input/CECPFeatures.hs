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

module BishBosh.Test.QuickCheck.Input.CECPFeatures() where

import			Control.Arrow((***))
import qualified	BishBosh.Input.CECPFeatures	as Input.CECPFeatures
import qualified	Data.Char
import qualified	Test.QuickCheck
import			ToolShed.Test.QuickCheck.Arbitrary.Map()

instance Test.QuickCheck.Arbitrary Input.CECPFeatures.CECPFeatures where
	arbitrary	= Input.CECPFeatures.mkCECPFeatures <$> (
		filter (
			not . null . fst {-key-}
		) . map (
			filter Data.Char.isAlpha *** either (
				Left . (`mod` 3)
			) (
				Right . filter Data.Char.isAlphaNum
			)
		) <$> Test.QuickCheck.arbitrary
	 ) <*> Test.QuickCheck.arbitrary {-done-}

