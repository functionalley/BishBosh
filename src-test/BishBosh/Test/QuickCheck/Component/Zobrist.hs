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

import			BishBosh.Test.QuickCheck.Model.Game()
import			Control.Arrow((&&&))
import qualified	BishBosh.Component.Zobrist	as Component.Zobrist
import qualified	BishBosh.Model.Game		as Model.Game
import qualified	BishBosh.Type.Crypto		as Type.Crypto
import qualified	Data.Bits
import qualified	Data.List
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
	 ) Test.QuickCheck.arbitrary <*> fmap System.Random.mkStdGen Test.QuickCheck.arbitrary

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Zobrist -> Model.Game.Game -> Test.QuickCheck.Property
		f zobrist game	= Test.QuickCheck.label "Zobrist.prop_hash1D(Game)/unique" . areUnique . map (
			(`Component.Zobrist.hash` zobrist) . (`Model.Game.applyQualifiedMove` game)
		 ) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Zobrist -> Model.Game.Game -> Test.QuickCheck.Property
		f zobrist game	= Test.QuickCheck.label "Zobrist.prop_hash1D(Position)/unique" . areUnique . map (
			(`Component.Zobrist.hash` zobrist) . Model.Game.mkPosition . (`Model.Game.applyQualifiedMove` game)
		 ) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Zobrist -> Model.Game.Game -> Test.QuickCheck.Property
		f zobrist game	= Test.QuickCheck.label "Zobrist.prop_(hash1D(Game) == hash1D(Position))" . all (
			uncurry (==) . (
				(
					`Component.Zobrist.hash` zobrist
				) &&& (
					`Component.Zobrist.hash` zobrist
				) . Model.Game.mkPosition
			) . (`Model.Game.applyQualifiedMove` game)
		 ) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Zobrist -> Model.Game.Game -> Test.QuickCheck.Property
		f zobrist game	= Test.QuickCheck.label "Zobrist.prop_incrementalEvaluation" . all (
			(
				\game' -> Component.Zobrist.hash game' zobrist == Model.Game.updateIncrementalPositionHash game (
					Component.Zobrist.hash game zobrist
				) game' zobrist
			) . (`Model.Game.applyQualifiedMove` game)
		 ) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f
 ] where
	areUnique :: Ord a => [a] -> Bool
	areUnique	= all ((== 1) . length) . Data.List.group . Data.List.sort

