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

module BishBosh.Test.QuickCheck.Search.KillerMoves(
-- * Types
-- ** Type-synonyms
--	Key,
	KillerMoves,
-- * Constants
	results
-- * Functions
--	normalise,
--	populate
) where

import			BishBosh.Test.QuickCheck.Component.Turn()
import			Control.Arrow((&&&))
import qualified	BishBosh.Property.Empty		as Property.Empty
import qualified	BishBosh.Search.DynamicMoveData	as Search.DynamicMoveData
import qualified	BishBosh.Search.EphemeralData	as Search.EphemeralData
import qualified	BishBosh.Search.KillerMoves	as Search.KillerMoves
import qualified	BishBosh.Type.Count		as Type.Count
import qualified	BishBosh.Type.Length		as Type.Length
import qualified	Test.QuickCheck

-- | Defines a concrete type for testing.
type KillerMoveKey	= Search.DynamicMoveData.KillerMoveKey Type.Length.X Type.Length.Y

instance (
	Enum				x,
	Enum				y,
	Ord				x,
	Ord				y,
	Test.QuickCheck.Arbitrary	x,
	Test.QuickCheck.Arbitrary	y
 ) => Test.QuickCheck.Arbitrary (Search.DynamicMoveData.KillerMoveKey x y) where
--	{-# SPECIALISE instance Test.QuickCheck.Arbitrary KillerMoveKey #-}
	arbitrary	= fmap Search.DynamicMoveData.mkKillerMoveKeyFromTurn Test.QuickCheck.arbitrary

-- | Map the Int-domain into a smaller number of plies.
normalise :: Int -> Type.Count.NPlies
normalise	= fromIntegral . succ . (`mod` 4)

-- |
populate :: Ord key => [(Int, key)] -> Search.KillerMoves.KillerMoves key
populate	= foldr (\(nPlies, killerMoveKey) -> Search.KillerMoves.insert (normalise nPlies) killerMoveKey) Property.Empty.empty {-KillerMoves-}

-- | Defines a concrete type for testing.
type KillerMoves	= Search.KillerMoves.KillerMoves KillerMoveKey

instance (
	Enum				key,
	Ord				key,
	Test.QuickCheck.Arbitrary	key
 ) => Test.QuickCheck.Arbitrary (Search.KillerMoves.KillerMoves key) where
	arbitrary	= fmap populate Test.QuickCheck.arbitrary

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: [(Int, KillerMoveKey)] -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "KillerMoves.prop_insert/getNMoves" . uncurry (==) . (Search.EphemeralData.getSize . populate &&& length)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f
 ]

