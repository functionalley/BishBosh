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

module BishBosh.Test.QuickCheck.Input.SearchOptions() where

import			BishBosh.Test.QuickCheck.Attribute.CaptureMoveSortAlgorithm()
import			BishBosh.Test.QuickCheck.Colour.LogicalColour()
import			BishBosh.Test.QuickCheck.Input.StandardOpeningOptions()
import			Control.Arrow((***))
import qualified	BishBosh.Input.SearchOptions	as Input.SearchOptions
import qualified	Data.Foldable
import qualified	Data.Map.Strict			as Map
import qualified	Test.QuickCheck

instance Test.QuickCheck.Arbitrary Input.SearchOptions.SearchOptions where
	arbitrary	= do
		searchDepthByLogicalColour	<- Map.map (
			(+ Input.SearchOptions.minimumSearchDepth) . fromInteger . (`mod` 4)
		 ) <$> Test.QuickCheck.arbitrary

		Input.SearchOptions.mkSearchOptions <$> Test.QuickCheck.arbitrary {-SortOnStandardOpeningMoveFrequency-} <*> Test.QuickCheck.arbitrary {-maybeCaptureMoveSortAlgorithm-} <*> (
			fmap (succ . (`mod` 3)) <$> Test.QuickCheck.arbitrary	-- maybeMinimumHammingDistance.
		 ) <*> (
			fmap (fromInteger . (`mod` 4)) <$> Test.QuickCheck.arbitrary	-- maybeRetireKillerMovesAfter.
		 ) <*> Test.QuickCheck.arbitrary {-trapRepeatedPositions-} <*> (
			if Data.Foldable.length searchDepthByLogicalColour == 1
				then Test.QuickCheck.arbitrary
				else return {-to Gen-monad-} False	-- UsePondering.
		 ) <*> (
			fmap (
				fromInteger . (`mod` 3) *** (
					if Data.Foldable.null searchDepthByLogicalColour
						then id
						else min $ Data.Foldable.maximum searchDepthByLogicalColour
				) . fromInteger . succ . (`mod` 3)
			) <$> Test.QuickCheck.arbitrary {-maybeUseTranspositions-}
		 ) <*> Test.QuickCheck.arbitrary {-standardOpeningOptions-} <*> return {-to Gen-monad-} searchDepthByLogicalColour

