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

module BishBosh.Test.QuickCheck.Colour.LogicalColour(
-- * Constants
	results
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Colour.LogicalColour		as Colour.LogicalColour
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Property.ForsythEdwards	as Property.ForsythEdwards
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	Test.QuickCheck

instance Test.QuickCheck.Arbitrary Colour.LogicalColour.LogicalColour where
	arbitrary	= Test.QuickCheck.elements Property.FixedMembership.members

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Colour.LogicalColour.LogicalColour -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "LogicalColour.prop_getOpposite" . uncurry (==) . (Property.Opposable.getOpposite . Property.Opposable.getOpposite &&& id)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 4 } f,
	let
		f :: Colour.LogicalColour.LogicalColour -> Test.QuickCheck.Property
		f logicalColour	= Test.QuickCheck.label "LogicalColour.prop_fen" $ case Property.ForsythEdwards.readsFEN $ Property.ForsythEdwards.showFEN logicalColour of
			[(logicalColour', "")]	-> logicalColour' == logicalColour
			_			-> False
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 4 } f
 ]


