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

module BishBosh.Test.QuickCheck.Model.GameTree(
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Model.Game()
import qualified	BishBosh.Model.Game		as Model.Game
import qualified	BishBosh.Model.GameTree		as Model.GameTree
import qualified	BishBosh.Property.Arboreal	as Property.Arboreal
import qualified	Data.Default
import qualified	Data.Maybe
import qualified	Test.QuickCheck

instance Test.QuickCheck.Arbitrary Model.GameTree.GameTree where
	arbitrary	= do
		depth	<- Test.QuickCheck.elements [1 .. 3]

		Property.Arboreal.prune depth . Model.GameTree.fromGame <$> Test.QuickCheck.arbitrary

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Model.Game.Game -> Test.QuickCheck.Property
		f = Test.QuickCheck.label "GameTree.prop_traceRoute" . (
			\turns -> Data.Maybe.maybe False (
				(== Just turns) . mapM Model.Game.maybeLastTurn
			) $ Model.GameTree.traceRoute Data.Default.def turns
		 ) . Model.Game.listTurnsChronologically
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f
 ]

