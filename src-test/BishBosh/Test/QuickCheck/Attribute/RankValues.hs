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

module BishBosh.Test.QuickCheck.Attribute.RankValues(
-- * Types
-- ** Type-synonyms
	RankValues
) where

import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Attribute.RankValues		as Attribute.RankValues
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Types				as T
import qualified	Test.QuickCheck

instance (
	Fractional	rankValue,
	Ord		rankValue,
	Show		rankValue
 ) => Test.QuickCheck.Arbitrary (Attribute.RankValues.RankValues rankValue) where
	arbitrary	= Attribute.RankValues.fromAssocs . zip Property.FixedMembership.members . map (
		recip . fromInteger . succ . (`mod` 100)	-- Normalise to the half open unit-interval (0,1].
	 ) <$> Test.QuickCheck.vector Attribute.Rank.nDistinctRanks

-- | Defines a concrete type for testing.
type RankValues	= Attribute.RankValues.RankValues T.RankValue

