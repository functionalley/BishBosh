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

 [@DESCRIPTION@]	Searches for the optimal /move/ from those currently available.
-}

module BishBosh.Search.Search(
-- * Types
-- ** Data-types
	Result (
--		MkResult,
		getSearchState,
		getQuantifiedGames,
		getNMovesEvaluated
	),
-- * Constants
	showsSeparator,
-- * Functions
	search,
	calculateBranchingFactor
 ) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Component.Move					as Component.Move
import qualified	BishBosh.Data.Exception					as Data.Exception
import qualified	BishBosh.Evaluation.PositionHashQuantifiedGameTree	as Evaluation.PositionHashQuantifiedGameTree
import qualified	BishBosh.Evaluation.QuantifiedGame			as Evaluation.QuantifiedGame
import qualified	BishBosh.Input.SearchOptions				as Input.SearchOptions
import qualified	BishBosh.Model.Game					as Model.Game
import qualified	BishBosh.Notation.MoveNotation				as Notation.MoveNotation
import qualified	BishBosh.Search.AlphaBeta				as Search.AlphaBeta
import qualified	BishBosh.Search.SearchState				as Search.SearchState
import qualified	BishBosh.State.TurnsByLogicalColour			as State.TurnsByLogicalColour
import qualified	BishBosh.Text.ShowList					as Text.ShowList
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Control.Monad.Reader
import qualified	Data.Maybe

-- | The type returned by 'search'.
data Result x y positionHash criterionValue weightedMean	= MkResult {
	getSearchState		:: Search.SearchState.SearchState x y positionHash criterionValue weightedMean,
	getQuantifiedGames	:: [Evaluation.QuantifiedGame.QuantifiedGame x y criterionValue weightedMean],	-- ^ The optimal path down the /positionHashQuantifiedGameTree/.
	getNMovesEvaluated	:: Component.Move.NMoves							-- ^ The total number of nodes in the /positionHashQuantifiedGameTree/ which were analysed.
}

instance Control.DeepSeq.NFData weightedMean => Control.DeepSeq.NFData (Result x y positionHash criterionValue weightedMean) where
	rnf MkResult { getQuantifiedGames = quantifiedGames }	= Control.DeepSeq.rnf quantifiedGames	-- CAVEAT: don't evaluate the search-state, since this contains the PositionHashQuantifiedGameTree !

-- | Used to format output.
showsSeparator :: ShowS
showsSeparator	= showString " -> "

instance (Enum x, Enum y, Real criterionValue, Real weightedMean) => Notation.MoveNotation.ShowNotationFloat (Result x y positionHash criterionValue weightedMean) where
	showsNotationFloat moveNotation showsDouble result@MkResult {
		getQuantifiedGames	= quantifiedGames,
		getNMovesEvaluated	= nMovesEvaluated
	} = Text.ShowList.showsFormattedList showsSeparator (
		Notation.MoveNotation.showsNotationFloat moveNotation showsDouble
	 ) quantifiedGames . showString "; selected after analysing " . shows nMovesEvaluated . showString " moves (branching-factor" . Text.ShowList.showsAssociation . showsDouble (
		calculateBranchingFactor result
	 ) . showChar ')'

-- | Initiates the recursive function 'Search.AlphaBeta.negaMax', then unpacks the results.
search :: (
	Enum	x,
	Enum	y,
	Eq	criterionValue,
	Num	weightedMean,
	Ord	weightedMean,
	Ord	positionHash,
	Ord	x,
	Ord	y
 )
	=> Input.SearchOptions.SearchDepth	-- ^ How deep down the tree to search.
	-> Search.SearchState.SearchState x y positionHash criterionValue weightedMean
	-> Input.SearchOptions.Reader (Result x y positionHash criterionValue weightedMean)
search 0 _	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Search.Search.search:\t" . shows Input.SearchOptions.searchDepthTag . showString " must be at least " $ shows Input.SearchOptions.minimumSearchDepth "."
search searchDepth searchState
	| Just terminationReason <- Model.Game.getMaybeTerminationReason game	= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Search.Search.search:\tthe game has already terminated; " $ shows terminationReason "."
	| otherwise								= do
		(maybeRetireKillerMovesAfter, maybeRetireTranspositionsAfter)	<- Control.Monad.Reader.asks $ Input.SearchOptions.getMaybeRetireKillerMovesAfter &&& Input.SearchOptions.maybeRetireTranspositionsAfter

		let nPlies	= State.TurnsByLogicalColour.getNPlies $ Model.Game.getTurnsByLogicalColour game

		searchResult	<- Search.AlphaBeta.negaMax searchDepth $ Search.SearchState.euthanise nPlies maybeRetireKillerMovesAfter maybeRetireTranspositionsAfter searchState

		case Search.AlphaBeta.extractSelectedTurns nPlies searchResult of
			(dynamicMoveData, turns@(turn : _), nMovesEvaluated)	-> let
				isMatch turn'	= (== turn') . Evaluation.QuantifiedGame.getLastTurn . Evaluation.PositionHashQuantifiedGameTree.getQuantifiedGame
			 in return {-to Reader-monad-} MkResult {
				getSearchState		= Search.SearchState.mkSearchState (
					Data.Maybe.fromMaybe (
						Control.Exception.throw $ Data.Exception.mkIncompatibleData "BishBosh.Search.Search.search:\tBishBosh.Data.RoseTree.reduce failed."
					) $ Evaluation.PositionHashQuantifiedGameTree.reduce (isMatch turn) positionHashQuantifiedGameTree
				) dynamicMoveData,
				getQuantifiedGames	= map Evaluation.PositionHashQuantifiedGameTree.getQuantifiedGame . Data.Maybe.fromMaybe (
					Control.Exception.throw $ Data.Exception.mkSearchFailure "BishBosh.Search.Search.search:\tBishBosh.Data.RoseTree.traceRoute failed."
				) $ Evaluation.PositionHashQuantifiedGameTree.traceRoute isMatch positionHashQuantifiedGameTree turns,
				getNMovesEvaluated	= nMovesEvaluated
			}
			_							-> Control.Exception.throw $ Data.Exception.mkNullDatum "BishBosh.Search.Search.search:\tzero turns selected."
	where
		positionHashQuantifiedGameTree	= Search.SearchState.getPositionHashQuantifiedGameTree searchState
		game				= Evaluation.QuantifiedGame.getGame $ Evaluation.PositionHashQuantifiedGameTree.getRootQuantifiedGame positionHashQuantifiedGameTree

-- | Calculate the geometric-mean of the number of moves evaluated at each node.
calculateBranchingFactor :: Floating branchingFactor => Result x y positionHash criterionValue weightedMean -> branchingFactor
calculateBranchingFactor MkResult {
	getQuantifiedGames	= quantifiedGames,
	getNMovesEvaluated	= nMovesEvaluated
}
	| null quantifiedGames	= Control.Exception.throw $ Data.Exception.mkNullDatum "BishBosh.Search.Search.calculateBranchingFactor:\tnull quantifiedGames."
	| nMovesEvaluated == 0	= Control.Exception.throw $ Data.Exception.mkInsufficientData "BishBosh.Search.Search.calculateBranchingFactor:\tzero moves analysed."
	| otherwise		= fromIntegral nMovesEvaluated ** recip (
		fromIntegral $ length quantifiedGames	-- The search-depth.
	)

