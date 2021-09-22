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
		getNPositionsEvaluated
	),
-- * Constants
	showsSeparator,
-- * Functions
	search
--	calculateBranchingFactor,
-- ** Constructor
--	mkResult
 ) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Data.Exception					as Data.Exception
import qualified	BishBosh.Evaluation.PositionHashQuantifiedGameTree	as Evaluation.PositionHashQuantifiedGameTree
import qualified	BishBosh.Evaluation.QuantifiedGame			as Evaluation.QuantifiedGame
import qualified	BishBosh.Input.SearchOptions				as Input.SearchOptions
import qualified	BishBosh.Model.Game					as Model.Game
import qualified	BishBosh.Notation.MoveNotation				as Notation.MoveNotation
import qualified	BishBosh.Search.AlphaBeta				as Search.AlphaBeta
import qualified	BishBosh.Search.EphemeralData				as Search.EphemeralData
import qualified	BishBosh.Search.SearchState				as Search.SearchState
import qualified	BishBosh.State.TurnsByLogicalColour			as State.TurnsByLogicalColour
import qualified	BishBosh.Text.ShowList					as Text.ShowList
import qualified	BishBosh.Type.Count					as Type.Count
import qualified	BishBosh.Type.Crypto					as Type.Crypto
import qualified	BishBosh.Type.Length					as Type.Length
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Control.Monad.Reader
import qualified	Data.Maybe

-- | The type returned by 'search'.
data Result x y positionHash	= MkResult {
	getSearchState		:: Search.SearchState.SearchState x y positionHash,
	getQuantifiedGames	:: [Evaluation.QuantifiedGame.QuantifiedGame x y],	-- ^ The optimal path down the /positionHashQuantifiedGameTree/.
	getNPositionsEvaluated	:: Type.Count.NPositions				-- ^ The total number of nodes in the /positionHashQuantifiedGameTree/ which were analysed.
}

instance Control.DeepSeq.NFData (Result x y positionHash) where
	rnf MkResult { getQuantifiedGames = quantifiedGames }	= Control.DeepSeq.rnf quantifiedGames	-- CAVEAT: don't evaluate the search-state, since this contains the PositionHashQuantifiedGameTree !

-- | Used to format output.
showsSeparator :: ShowS
showsSeparator	= showString " -> "

instance (Enum x, Enum y) => Notation.MoveNotation.ShowNotationFloat (Result x y positionHash) where
	showsNotationFloat moveNotation showsDouble result@MkResult {
		getQuantifiedGames	= quantifiedGames,
		getNPositionsEvaluated	= nPositionsEvaluated
	} = Text.ShowList.showsFormattedList showsSeparator (
		Notation.MoveNotation.showsNotationFloat moveNotation showsDouble
	 ) quantifiedGames . showString "; selected after analysing " . shows nPositionsEvaluated . showString " nodes" . (
		if null quantifiedGames || nPositionsEvaluated == 0
			then id
			else showString " (branching-factor" . Text.ShowList.showsAssociation . showsDouble (calculateBranchingFactor result) . showChar ')'
	 )

-- | Smart constructor.
mkResult :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Show	x,
	Show	y
 )
	=> Search.SearchState.SearchState x y positionHash
	-> [Evaluation.QuantifiedGame.QuantifiedGame x y]
	-> Type.Count.NPositions
	-> Result x y positionHash
mkResult searchState quantifiedGames nPositionsEvaluated
	| null quantifiedGames	= Control.Exception.throw . Data.Exception.mkNullDatum . showString "BishBosh.Search.Search.mkResult:\tnull quantifiedGames; " $ shows game "."
	| nPositionsEvaluated < 0	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Search.Search.mkResult:\tnPositionsEvaluated=" . shows nPositionsEvaluated . showString " mustn't be negative; " $ shows game "."
	| otherwise		= MkResult {
		getSearchState		= searchState,
		getQuantifiedGames	= quantifiedGames,
		getNPositionsEvaluated	= nPositionsEvaluated
	}
	where
		game	= Evaluation.QuantifiedGame.getGame . Evaluation.PositionHashQuantifiedGameTree.getRootQuantifiedGame $ Search.SearchState.getPositionHashQuantifiedGameTree searchState

-- | Initiates the recursive function 'Search.AlphaBeta.negaMax', then unpacks the results.
search :: (
	Enum	x,
	Enum	y,
	Ord	positionHash,
	Ord	x,
	Ord	y,
	Show	x,
	Show	y
 )
	=> Type.Count.NPlies	-- ^ How deep down the tree to search.
	-> Search.SearchState.SearchState x y positionHash
	-> Input.SearchOptions.Reader (Result x y positionHash)
{-# SPECIALISE search :: Type.Count.NPlies -> Search.SearchState.SearchState Type.Length.X Type.Length.Y Type.Crypto.PositionHash -> Input.SearchOptions.Reader (Result Type.Length.X Type.Length.Y Type.Crypto.PositionHash) #-}
search 0 _	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Search.Search.search:\t" . shows Input.SearchOptions.searchDepthTag . showString " must be at least " $ shows Input.SearchOptions.minimumSearchDepth "."
search searchDepth searchState
	| Just terminationReason <- Model.Game.getMaybeTerminationReason game	= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Search.Search.search:\tthe game has already terminated; " $ shows terminationReason "."
	| otherwise								= do
		pair	<- Control.Monad.Reader.asks $ Input.SearchOptions.getMaybeRetireKillerMovesAfter &&& Input.SearchOptions.maybeRetireTranspositionsAfter

		let nPlies	= State.TurnsByLogicalColour.getNPlies $ Model.Game.getTurnsByLogicalColour game

		searchResult	<- Search.AlphaBeta.negaMax searchDepth $ uncurry (Search.EphemeralData.maybeEuthanise nPlies) pair searchState

		case Search.AlphaBeta.extractSelectedTurns nPlies searchResult of
			(dynamicMoveData, turns@(turn : _), nPositionsEvaluated)	-> let
				isMatch turn'	= (== turn') . Evaluation.QuantifiedGame.getLastTurn . Evaluation.PositionHashQuantifiedGameTree.getQuantifiedGame
			 in return {-to Reader-monad-} $ mkResult (
				Search.SearchState.mkSearchState (
					Data.Maybe.fromMaybe (
						Control.Exception.throw . Data.Exception.mkIncompatibleData . showString "BishBosh.Search.Search.search:\tBishBosh.Evaluation.PositionHashQuantifiedGameTree.reduce failed; " $ shows turn "."
					) $ Evaluation.PositionHashQuantifiedGameTree.reduce (isMatch turn) positionHashQuantifiedGameTree
				) dynamicMoveData
			 ) (
				map Evaluation.PositionHashQuantifiedGameTree.getQuantifiedGame . Data.Maybe.fromMaybe (
					Control.Exception.throw . Data.Exception.mkSearchFailure . showString "BishBosh.Search.Search.search:\tEvaluation.PositionHashQuantifiedGameTree.traceRoute failed; " $ shows turns "."
				) $ Evaluation.PositionHashQuantifiedGameTree.traceRoute isMatch positionHashQuantifiedGameTree turns
			 ) nPositionsEvaluated
			_							-> Control.Exception.throw $ Data.Exception.mkNullDatum "BishBosh.Search.Search.search:\tzero turns selected."
	where
		positionHashQuantifiedGameTree	= Search.SearchState.getPositionHashQuantifiedGameTree searchState
		game				= Evaluation.QuantifiedGame.getGame $ Evaluation.PositionHashQuantifiedGameTree.getRootQuantifiedGame positionHashQuantifiedGameTree

-- | Calculate the geometric-mean of the number of plies evaluated at each node.
calculateBranchingFactor :: Floating branchingFactor => Result x y positionHash -> branchingFactor
calculateBranchingFactor MkResult {
	getQuantifiedGames	= quantifiedGames,
	getNPositionsEvaluated	= nPositionsEvaluated
}
	| null quantifiedGames		= Control.Exception.throw $ Data.Exception.mkNullDatum "BishBosh.Search.Search.calculateBranchingFactor:\tnull quantifiedGames."
	| nPositionsEvaluated == 0	= Control.Exception.throw $ Data.Exception.mkOutOfBounds "BishBosh.Search.Search.calculateBranchingFactor:\tzero plies analysed."
	| otherwise			= fromIntegral nPositionsEvaluated ** recip (
		fromIntegral {-Int-} $ length quantifiedGames	-- The search-depth.
	)

