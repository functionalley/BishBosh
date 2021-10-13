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

module BishBosh.Test.QuickCheck.Component.Zobrist(
-- * Types
-- ** Type-synonyms
	Zobrist
) where

import qualified	BishBosh.Component.Zobrist	as Component.Zobrist
import qualified	BishBosh.Type.Crypto		as Type.Crypto
import qualified	Data.Bits
import qualified	System.Random
import qualified	Test.QuickCheck

-- | Defines a concrete type for testing.
type Zobrist	= Component.Zobrist.Zobrist Type.Crypto.PositionHash

instance (Data.Bits.FiniteBits positionHash, System.Random.Random positionHash) => Test.QuickCheck.Arbitrary (Component.Zobrist.Zobrist positionHash) where
	{-# SPECIALISE instance Test.QuickCheck.Arbitrary Zobrist #-}
	arbitrary	= Component.Zobrist.mkZobrist <$> fmap (
		fmap (
			`mod` 3	-- CAVEAT: this value is limited by the width of 'Crypto.PositionHash'.
		)
	 ) Test.QuickCheck.arbitrary {-MaybeMinimumHammingDistance-} <*> fmap System.Random.mkStdGen Test.QuickCheck.arbitrary

