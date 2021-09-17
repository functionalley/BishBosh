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
	Zobrist,
-- * Constants,
	results
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Component.Zobrist		as Component.Zobrist
import qualified	BishBosh.Model.Game			as Model.Game
import qualified	BishBosh.Test.QuickCheck.Model.Game	as Test.QuickCheck.Model.Game
import qualified	BishBosh.Type.Length			as Type.Length
import qualified	BishBosh.Types				as T
import qualified	Data.Array.IArray
import qualified	Data.Bits
import qualified	Data.List
import qualified	System.Random
import qualified	Test.QuickCheck

-- | Defines a concrete type for testing.
type Zobrist	= Component.Zobrist.Zobrist Type.Length.X Type.Length.Y T.PositionHash

instance (
	Data.Array.IArray.Ix	x,
	Data.Bits.FiniteBits	positionHash,
	Enum			x,
	Enum			y,
	Num			positionHash,
	Ord			y,
	Show			positionHash,
	System.Random.Random	positionHash
 ) => Test.QuickCheck.Arbitrary (Component.Zobrist.Zobrist x y positionHash) where
--	{-# SPECIALISE instance Test.QuickCheck.Arbitrary Zobrist #-}
	arbitrary	= Component.Zobrist.mkZobrist <$> fmap (
		fmap (
			`mod` 3	-- CAVEAT: this value is limited by the width of 'T.PositionHash'.
		)
	 ) Test.QuickCheck.arbitrary <*> fmap System.Random.mkStdGen Test.QuickCheck.arbitrary

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Zobrist -> Test.QuickCheck.Model.Game.Game -> Test.QuickCheck.Property
		f zobrist game	= Test.QuickCheck.label "Zobrist.prop_hash2D(Game)/unique" . areUnique . map (
			(`Component.Zobrist.hash2D` zobrist) . (`Model.Game.applyQualifiedMove` game)
		 ) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Zobrist -> Test.QuickCheck.Model.Game.Game -> Test.QuickCheck.Property
		f zobrist game	= Test.QuickCheck.label "Zobrist.prop_hash2D(Position)/unique" . areUnique . map (
			(`Component.Zobrist.hash2D` zobrist) . Model.Game.mkPosition . (`Model.Game.applyQualifiedMove` game)
		 ) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Zobrist -> Test.QuickCheck.Model.Game.Game -> Test.QuickCheck.Property
		f zobrist game	= Test.QuickCheck.label "Zobrist.prop_(hash2D(Game) == hash2D(Position))" . all (
			uncurry (==) . (
				(
					`Component.Zobrist.hash2D` zobrist
				) &&& (
					`Component.Zobrist.hash2D` zobrist
				) . Model.Game.mkPosition
			) . (`Model.Game.applyQualifiedMove` game)
		 ) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Zobrist -> Test.QuickCheck.Model.Game.Game -> Test.QuickCheck.Property
		f zobrist game	= Test.QuickCheck.label "Zobrist.prop_incrementalEvaluation" . all (
			(
				\game' -> Component.Zobrist.hash2D game' zobrist == Model.Game.updateIncrementalPositionHash game (
					Component.Zobrist.hash2D game zobrist
				) game' zobrist
			) . (`Model.Game.applyQualifiedMove` game)
		 ) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f
 ] where
	areUnique :: Ord a => [a] -> Bool
	areUnique	= all ((== 1) . length) . Data.List.group . Data.List.sort

