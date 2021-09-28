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

module BishBosh.Test.QuickCheck.Input.Options(
-- * Types
-- ** Type-synonyms
	Options,
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Input.EvaluationOptions()
import			BishBosh.Test.QuickCheck.Input.IOOptions()
import			BishBosh.Test.QuickCheck.Input.SearchOptions()
import qualified	BishBosh.Input.Options	as Input.Options
import qualified	BishBosh.Type.Length	as Type.Length
import qualified	BishBosh.Type.Mass	as Type.Mass
import qualified	Test.QuickCheck

-- | Defines a concrete type for testing.
type Options	= Input.Options.Options Type.Mass.PieceSquareValue Type.Length.X Type.Length.Y

instance (
	Enum				pieceSquareValue,
	Fractional			pieceSquareValue,
	Ord				pieceSquareValue,
	Enum				x,
	Enum				y,
	Ord				x,
	Ord				y,
	Show				pieceSquareValue,
	Test.QuickCheck.Arbitrary	pieceSquareValue
 ) => Test.QuickCheck.Arbitrary (Input.Options.Options pieceSquareValue x y) where
--	{-# SPECIALISE instance Test.QuickCheck.Arbitrary Options #-}
	arbitrary	= do
		(maybeMaximumPlies, maybeRandomSeed, evaluationOptions, searchOptions, ioOptions)	<- Test.QuickCheck.arbitrary

		return {-to Gen-monad-} $ Input.Options.mkOptions (
			fmap (fromInteger . succ . abs) maybeMaximumPlies
		 ) maybeRandomSeed evaluationOptions searchOptions ioOptions

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Options -> Test.QuickCheck.Property
		f options	= Test.QuickCheck.label "Options.prop_swapSearchDepth" $ Input.Options.swapSearchDepth (Input.Options.swapSearchDepth options) == options
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 32 } f
 ]

