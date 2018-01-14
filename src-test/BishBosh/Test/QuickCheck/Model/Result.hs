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

module BishBosh.Test.QuickCheck.Model.Result(
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Attribute.LogicalColour()
import			Control.Arrow((&&&))
import qualified	BishBosh.Model.Result		as Model.Result
import qualified	BishBosh.Property.Opposable	as Property.Opposable
import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO

instance Test.QuickCheck.Arbitrary Model.Result.Result where
	arbitrary	= Model.Result.mkResult `fmap` Test.QuickCheck.oneof [fmap Just Test.QuickCheck.arbitrary, return {-to Gen-monad-} Nothing]

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Model.Result.Result -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Result.prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: String -> Test.QuickCheck.Property
		f garbage	= Test.QuickCheck.label "Result.prop_read" $ case (reads garbage :: [(Model.Result.Result, String)]) of
			[_]	-> True
			_	-> True	-- Unless the read-implementation throws an exception.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Model.Result.Result -> String -> Test.QuickCheck.Property
		f result	= Test.QuickCheck.label "Result.prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (const False) result
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Model.Result.Result -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Result.prop_getOpposite" . uncurry (==) . (Property.Opposable.getOpposite . Property.Opposable.getOpposite &&& id)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 8 } f
 ]

