{-# LANGUAGE CPP #-}
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

module BishBosh.Test.QuickCheck.Input.RankValues(
-- * Constants
	results
) where

import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Input.RankValues		as Input.RankValues
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	Data.List
import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO

#ifdef USE_NEWTYPE_WRAPPERS
import			BishBosh.Test.QuickCheck.Metric.RankValue()
#endif

instance Test.QuickCheck.Arbitrary Input.RankValues.RankValues where
	arbitrary	= Input.RankValues.fromAssocs . zip Property.FixedMembership.members . Data.List.sort {-ensures Q is most valuable (except K)-} 
#ifndef USE_NEWTYPE_WRAPPERS
		. map (recip . fromInteger . succ . abs)	-- Map into the closed unit interval.
#endif
		<$> Test.QuickCheck.suchThat (
			Test.QuickCheck.vector $ fromIntegral Attribute.Rank.nDistinctRanks
		) (any (/= 0))

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Input.RankValues.RankValues -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "RankValues.prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 32 } f,
	let
		f :: String -> Test.QuickCheck.Property
		f garbage	= Test.QuickCheck.label "RankValues.prop_read" $ case (reads garbage :: [(Input.RankValues.RankValues, String)]) of
			[_]	-> True
			_	-> True	-- Unless the read-implementation throws an exception.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Input.RankValues.RankValues -> String -> Test.QuickCheck.Property
		f rankValue	= Test.QuickCheck.label "RankValues.prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (const True) rankValue
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f
 ]

