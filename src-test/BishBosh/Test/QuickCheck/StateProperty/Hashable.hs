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

 [@DESCRIPTION@]	Defines tests.
-}

module BishBosh.Test.QuickCheck.StateProperty.Hashable(
-- * Constants,
	results
) where

import			BishBosh.Test.QuickCheck.Model.Game()
import			Control.Arrow((&&&))
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.StateProperty.Hashable			as StateProperty.Hashable
import qualified	BishBosh.Test.QuickCheck.Component.Zobrist	as Test.QuickCheck.Component.Zobrist
import qualified	Data.List
import qualified	Test.QuickCheck

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Test.QuickCheck.Component.Zobrist.Zobrist -> Model.Game.Game -> Test.QuickCheck.Property
		f zobrist game	= Test.QuickCheck.label "Hashable.prop_hash(Game)/unique" . areUnique . map (
			StateProperty.Hashable.hash zobrist . (`Model.Game.applyQualifiedMove` game)
		 ) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Test.QuickCheck.Component.Zobrist.Zobrist -> Model.Game.Game -> Test.QuickCheck.Property
		f zobrist game	= Test.QuickCheck.label "Hashable.prop_hash(Position)/unique" . areUnique . map (
			StateProperty.Hashable.hash zobrist . Model.Game.mkPosition . (`Model.Game.applyQualifiedMove` game)
		 ) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Test.QuickCheck.Component.Zobrist.Zobrist -> Model.Game.Game -> Test.QuickCheck.Property
		f zobrist game	= Test.QuickCheck.label "Hashable.prop_(hash(Game) == hash(Position))" . all (
			uncurry (==) . (
				StateProperty.Hashable.hash zobrist &&& StateProperty.Hashable.hash zobrist . Model.Game.mkPosition
			) . (`Model.Game.applyQualifiedMove` game)
		 ) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Test.QuickCheck.Component.Zobrist.Zobrist -> Model.Game.Game -> Test.QuickCheck.Property
		f zobrist game	= Test.QuickCheck.label "Hashable.prop_incrementalEvaluation" . all (
			(
				\game' -> StateProperty.Hashable.hash zobrist game' == Model.Game.updateIncrementalPositionHash game (
					StateProperty.Hashable.hash zobrist game
				) game' zobrist
			) . (`Model.Game.applyQualifiedMove` game)
		 ) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f
 ] where
	areUnique :: Ord a => [a] -> Bool
	areUnique	= all ((== 1) . length) . Data.List.group . Data.List.sort

