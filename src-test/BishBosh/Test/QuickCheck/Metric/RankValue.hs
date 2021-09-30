{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
	Copyright (C) 2021 Dr. Alistair Ward

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

module BishBosh.Test.QuickCheck.Metric.RankValue(
-- * Constants
	results
) where

import qualified	BishBosh.Metric.RankValue	as Metric.RankValue
import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO

instance Test.QuickCheck.Arbitrary Metric.RankValue.RankValue where
	arbitrary	= Test.QuickCheck.elements $ map fromRational [0, recip 10 .. 1]

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Metric.RankValue.RankValue -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "RankValue.prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 32 } f,
	let
		f :: String -> Test.QuickCheck.Property
		f garbage	= Test.QuickCheck.label "RankValue.prop_read" $ case (reads garbage :: [(Metric.RankValue.RankValue, String)]) of
			[_]	-> True
			_	-> True	-- Unless the read-implementation throws an exception.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Metric.RankValue.RankValue -> String -> Test.QuickCheck.Property
		f rankValue	= Test.QuickCheck.label "RankValue.prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (`elem` ".-+eEoOxX" ++ ['0' .. '9']) rankValue
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f
 ]

