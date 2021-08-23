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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties.
-}

module BishBosh.Test.QuickCheck.Search.Search(
-- * Constants
	results
) where

import qualified	BishBosh.Evaluation.PositionHashQuantifiedGameTree	as Evaluation.PositionHashQuantifiedGameTree
import qualified	BishBosh.Evaluation.QuantifiedGame			as Evaluation.QuantifiedGame
import qualified	BishBosh.Input.SearchOptions				as Input.SearchOptions
import qualified	BishBosh.Model.Game					as Model.Game
import qualified	BishBosh.Search.Search					as Search.Search
import qualified	BishBosh.Search.SearchState				as Search.SearchState
import qualified	BishBosh.Test.QuickCheck.Search.SearchState		as Test.QuickCheck.Search.SearchState
import qualified	Control.Monad.Reader
import qualified	Data.Default
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

-- | The constant test-results.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
{- CAVEAT: has failed with [
	{captureMoveSortAlgorithm = MVVLVA, minimumHammingDistance = 2, retireKillerMovesAfter = 1, sortOnStandardOpeningMoveFrequency = False, retireTranspositionsAfter = 0, minimumTranspositionSearchDepth = 2, trapRepeatedPositions = False},
	{captureMoveSortAlgorithm = SEE, retireKillerMovesAfter = 2, sortOnStandardOpeningMoveFrequency = False, retireTranspositionsAfter = 1, minimumTranspositionSearchDepth = 1, trapRepeatedPositions = False, usePondering = False}
 ]
-}
	let
		f
			:: Input.SearchOptions.SearchOptions
			-> Input.SearchOptions.SearchDepth
			-> Test.QuickCheck.Search.SearchState.SearchState
			-> Test.QuickCheck.Property
		f searchOptions searchDepth searchState	= searchOptions /= Data.Default.def && not (
			Model.Game.isTerminated . Evaluation.QuantifiedGame.getGame . Evaluation.PositionHashQuantifiedGameTree.getRootQuantifiedGame $ Search.SearchState.getPositionHashQuantifiedGameTree searchState
		 ) ==> Test.QuickCheck.label "Search.prop_search/search-options" $ getQuantifiedGames searchOptions' == getQuantifiedGames defaultSearchOptions where
			defaultSearchOptions	= Data.Default.def

			searchOptions'	= searchOptions {
				Input.SearchOptions.getUsePondering			= Input.SearchOptions.getUsePondering defaultSearchOptions,
				Input.SearchOptions.getStandardOpeningOptions		= Input.SearchOptions.getStandardOpeningOptions defaultSearchOptions,
				Input.SearchOptions.getSearchDepthByLogicalColour	= Input.SearchOptions.getSearchDepthByLogicalColour defaultSearchOptions

			}

			getQuantifiedGames	= Search.Search.getQuantifiedGames . Control.Monad.Reader.runReader (Search.Search.search (Input.SearchOptions.minimumSearchDepth + mod searchDepth 3) searchState)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 128 } f
 ]

