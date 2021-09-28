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

module BishBosh.Test.QuickCheck.Input.RankValues() where

import			BishBosh.Test.QuickCheck.Metric.RankValue()
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Input.RankValues		as Input.RankValues
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	Data.List
import qualified	Test.QuickCheck

instance Test.QuickCheck.Arbitrary Input.RankValues.RankValues where
	arbitrary	= Input.RankValues.fromAssocs . zip Property.FixedMembership.members . Data.List.sort {-ensures Q is most valuable-} <$> Test.QuickCheck.vector (fromIntegral Attribute.Rank.nDistinctRanks)

