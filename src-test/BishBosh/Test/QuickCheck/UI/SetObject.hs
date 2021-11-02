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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties.
-}

module BishBosh.Test.QuickCheck.UI.SetObject (
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Model.Game()
import qualified	BishBosh.Input.SearchOptions	as Input.SearchOptions
import qualified	BishBosh.UI.SetObject		as UI.SetObject
import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO

instance Test.QuickCheck.Arbitrary UI.SetObject.SetObject where
	arbitrary	= Test.QuickCheck.oneof [
--		UI.SetObject.mkEPD <$> Test.QuickCheck.arbitrary,	-- CAVEAT: this is problematic, since move-information is irretrievably lost from the game in the conversion to EPD.
		Test.QuickCheck.elements $ map UI.SetObject.mkSearchDepth $ take 4 [Input.SearchOptions.minimumSearchDepth .. ]
	 ]

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: UI.SetObject.SetObject -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "SetObject.prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: String -> Test.QuickCheck.Property
		f garbage	= Test.QuickCheck.label "SetObject.prop_read" $ case (reads garbage :: [(UI.SetObject.SetObject, String)]) of
			[_]	-> True
			_	-> True	-- Unless the read-implementation throws an exception.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: UI.SetObject.SetObject -> String -> Test.QuickCheck.Property
		f setObject	= Test.QuickCheck.label "SetObject.prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (`elem` (".-+eEoOxX" ++ ['0' .. '9'])) setObject
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 1024} f
 ]

