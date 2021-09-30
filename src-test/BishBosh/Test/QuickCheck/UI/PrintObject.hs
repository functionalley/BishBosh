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

module BishBosh.Test.QuickCheck.UI.PrintObject (
-- * Constants
	results
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.UI.PrintObject			as UI.PrintObject
import qualified	Data.Char
import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO

instance Test.QuickCheck.Arbitrary UI.PrintObject.PrintObject where
	arbitrary	= Test.QuickCheck.elements Property.FixedMembership.members

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: UI.PrintObject.PrintObject -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "PrintObject.prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: String -> Test.QuickCheck.Property
		f garbage	= Test.QuickCheck.label "PrintObject.prop_read" $ case (reads garbage :: [(UI.PrintObject.PrintObject, String)]) of
			[_]	-> True
			_	-> True	-- Unless the read-implementation throws an exception.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: UI.PrintObject.PrintObject -> String -> Test.QuickCheck.Property
		f printObject	= Test.QuickCheck.label "PrintObject.prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (
			uncurry (||) . (Data.Char.isAlphaNum &&& (`elem` "_'"))	-- Haskell's lexer accepts these.
		 ) printObject
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 1024 } f
 ]

