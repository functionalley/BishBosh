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

module BishBosh.Test.QuickCheck.Attribute.MoveType(
-- * Constants
	results
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.MoveType		as Attribute.MoveType
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO

instance Test.QuickCheck.Arbitrary Attribute.MoveType.MoveType where
	arbitrary	= Test.QuickCheck.elements Property.FixedMembership.members

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Attribute.MoveType.MoveType -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "MoveType.prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: String -> Test.QuickCheck.Property
		f garbage	= Test.QuickCheck.label "MoveType.prop_read" $ case (reads garbage :: [(Attribute.MoveType.MoveType, String)]) of
			[_]	-> True
			_	-> True	-- Unless the read-implementation throws an exception.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Attribute.MoveType.MoveType -> String -> Test.QuickCheck.Property
		f moveType	= Test.QuickCheck.label "MoveType.prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (
			const False
		 ) moveType
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 512 } f,
	let
		f :: Attribute.MoveType.MoveType -> Test.QuickCheck.Property
		f = Test.QuickCheck.label "MoveType.prop_isQuiet" . uncurry (==) . (
			Attribute.MoveType.isQuiet &&& not . uncurry (||) . (Attribute.MoveType.isCapture &&& Attribute.MoveType.isPromotion)
		 )
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64} f
 ]

