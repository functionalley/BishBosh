-- {-# LANGUAGE ScopedTypeVariables #-}
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

 [@DESCRIPTION@]

	* Performs an <https://www.chessprogramming.org/Alpha-Beta> search, implemented using <https://www.chessprogramming.org/Negamax>.

	* Moves are dynamically re-ordering using the killer-heuristic.

	* <https://www.chessprogramming.org/Repetitions> & <https://www.chessprogramming.org/Transposition>s are detected.
-}

module BishBosh.Search.AlphaBeta(
-- * Types
-- ** Type-synonyms
--	Transformation,
-- ** Data-types
--	Result(),
-- * Functions
	extractSelectedTurns,
--	updateKillerMoves,
--	findTranspositionTerminalQuantifiedGame,
--	updateTranspositions,
	negaMax,
--	negateFitnessOfResult,
--	addNMovesToResult
 ) where

import			BishBosh.Model.Game((=~))
import			Control.Arrow((&&&))
import qualified	BishBosh.Component.Move					as Component.Move
import qualified	BishBosh.Component.QualifiedMove			as Component.QualifiedMove
import qualified	BishBosh.Component.Turn					as Component.Turn
import qualified	BishBosh.Data.Exception					as Data.Exception
import qualified	BishBosh.Evaluation.PositionHashQuantifiedGameTree	as Evaluation.PositionHashQuantifiedGameTree
import qualified	BishBosh.Evaluation.QuantifiedGame			as Evaluation.QuantifiedGame
import qualified	BishBosh.Input.SearchOptions				as Input.SearchOptions
import qualified	BishBosh.Model.Game					as Model.Game
import qualified	BishBosh.Search.DynamicMoveData				as Search.DynamicMoveData
import qualified	BishBosh.Search.KillerMoves				as Search.KillerMoves
import qualified	BishBosh.Search.SearchState				as Search.SearchState
import qualified	BishBosh.Search.Transpositions				as Search.Transpositions
import qualified	BishBosh.Search.TranspositionValue			as Search.TranspositionValue
import qualified	BishBosh.State.InstancesByPosition			as State.InstancesByPosition
import qualified	BishBosh.State.TurnsByLogicalColour			as State.TurnsByLogicalColour
import qualified	Control.Exception
import qualified	Control.Monad.Reader
import qualified	Data.Maybe
import qualified	Data.Tree

-- | The type returned by 'negaMax'.
data Result x y positionHash criterionValue weightedMean	= MkResult {
	getDynamicMoveData	:: Search.DynamicMoveData.DynamicMoveData x y positionHash,	-- ^ Killer moves & transpositions.
	getQuantifiedGame	:: Evaluation.QuantifiedGame.QuantifiedGame x y criterionValue weightedMean,
	getNMovesEvaluated	:: Component.Move.NMoves					-- ^ The total number of moves analysed, before making the selection.
}

{- |
	* Remove any /turn/s made before starting the search.

	* N.B.: abandons the fitness of the game.
-}
extractSelectedTurns
	:: Component.Move.NPlies
	-> Result x y positionHash criterionValue weightedMean
	-> (Search.DynamicMoveData.DynamicMoveData x y positionHash, [Component.Turn.Turn x y], Component.Move.NMoves)
extractSelectedTurns nPlies MkResult {
	getDynamicMoveData	= dynamicMoveData,
	getQuantifiedGame	= quantifiedGame,
	getNMovesEvaluated	= nMovesEvaluated
} = (
	dynamicMoveData,
	Evaluation.QuantifiedGame.getLatestTurns nPlies quantifiedGame,
	nMovesEvaluated
 )

-- | Record the last move as a killer, unless it's a capture move.
updateKillerMoves
	:: (Ord x, Ord y)
	=> Model.Game.Game x y
	-> Search.DynamicMoveData.Transformation x y positionHash
updateKillerMoves game
	| Just lastTurn <- Model.Game.maybeLastTurn game	= if Component.Turn.isCapture lastTurn
		then id	-- This move was (assuming appropriate Search-options) statically sorted.
		else Search.DynamicMoveData.updateKillerMoves . Search.KillerMoves.insert (
			State.TurnsByLogicalColour.getNPlies $ Model.Game.getTurnsByLogicalColour game
		) $ Search.DynamicMoveData.mkKillerMoveKeyFromTurn lastTurn
	| otherwise						= Control.Exception.throw $ Data.Exception.mkNullDatum "BishBosh.Search.AlphaBeta.updateKillerMoves:\tzero turns have been made."

{- |
	* Track the specified move-sequence down the /positionHashQuantifiedGameTree/ & retrieve the fitness from the terminal quantified game.

	* CAVEAT: the return-value, is quantified from the perspective of the player who is about to move.
-}
findTranspositionTerminalQuantifiedGame :: (
	Eq	x,
	Eq	y,
	Num	weightedMean
 )
	=> Evaluation.PositionHashQuantifiedGameTree.PositionHashQuantifiedGameTree x y positionHash criterionValue weightedMean
	-> Search.TranspositionValue.Value (Component.Move.Move x y)
	-> Evaluation.QuantifiedGame.QuantifiedGame x y criterionValue weightedMean
findTranspositionTerminalQuantifiedGame positionHashQuantifiedGameTree transpositionValue	= Data.Maybe.maybe (
	Control.Exception.throw $ Data.Exception.mkSearchFailure "BishBosh.Search.AlphaBeta.findTranspositionTerminalQuantifiedGame:\tEvaluation.PositionHashQuantifiedGameTree.traceMatchingMoves failed."
 ) (
	(
		if even $ Search.TranspositionValue.inferSearchDepth transpositionValue
			then Evaluation.QuantifiedGame.negateFitness	-- The opponent made the last move in the list, & therefore defined the fitness.
			else id
	) . Evaluation.PositionHashQuantifiedGameTree.getQuantifiedGame . last
 ) . Evaluation.PositionHashQuantifiedGameTree.traceMatchingMoves positionHashQuantifiedGameTree $ Search.TranspositionValue.getMoves transpositionValue

-- | Record a move-sequence in the transposition-table.
updateTranspositions :: (
	Eq	x,
	Eq	y,
	Num	weightedMean,
	Ord	positionHash,
	Ord	weightedMean
 )
	=> Search.TranspositionValue.IsOptimal
	-> Component.Move.NPlies
	-> positionHash
	-> [Component.Turn.Turn x y]
	-> Evaluation.PositionHashQuantifiedGameTree.PositionHashQuantifiedGameTree x y positionHash criterionValue weightedMean
	-> Search.DynamicMoveData.Transformation x y positionHash
updateTranspositions isOptimal nPlies positionHash turns positionHashQuantifiedGameTree	= Search.DynamicMoveData.updateTranspositions $ Search.Transpositions.insert (
	Evaluation.QuantifiedGame.getFitness . findTranspositionTerminalQuantifiedGame positionHashQuantifiedGameTree
 ) positionHash {-the hash of the game before the first move in the sequence-} . Search.TranspositionValue.mkValue isOptimal nPlies $ map (
	Component.QualifiedMove.getMove . Component.Turn.getQualifiedMove
 ) turns

-- | Implements a depth-first search (implemented as nega-max), with alpha-beta pruning.
negaMax :: {-forall x y positionHash criterionValue weightedMean.-} (
	Enum	x,
	Enum	y,
	Eq	criterionValue,
	Num	weightedMean,
	Ord	weightedMean,
	Ord	positionHash,
	Ord	x,
	Ord	y
 )
	=> Input.SearchOptions.SearchDepth	-- ^ The depth to which the tree should be searched; i.e. the number of plies to look-ahead.
	-> Search.SearchState.SearchState x y positionHash criterionValue weightedMean
	-> Input.SearchOptions.Reader (Result x y positionHash criterionValue weightedMean)
negaMax initialSearchDepth initialSearchState	= do
	maybeMinimumTranspositionSearchDepth	<- Control.Monad.Reader.asks Input.SearchOptions.maybeMinimumTranspositionSearchDepth
	recordKillerMoves			<- Control.Monad.Reader.asks Input.SearchOptions.recordKillerMoves
	trapRepeatedPositions			<- Control.Monad.Reader.asks Input.SearchOptions.getTrapRepeatedPositions

	let
{-
		descend
			:: Evaluation.QuantifiedGame.OpenInterval x y criterionValue weightedMean
			-> Input.SearchOptions.SearchDepth
			-> Search.SearchState.SearchState x y positionHash criterionValue weightedMean
			-> Result x y positionHash criterionValue weightedMean
-}
		descend (maybeAlphaQuantifiedGame, maybeBetaQuantifiedGame) searchDepth searchState
			| searchDepth == 0 || Model.Game.isTerminated game	= MkResult {
				getDynamicMoveData	= dynamicMoveData,
				getQuantifiedGame	= Evaluation.QuantifiedGame.negateFitness quantifiedGame,	-- CAVEAT: zero new moves have been applied, so the last move was the opponent's.
				getNMovesEvaluated	= 1								-- Fitness-negation requires evaluation.
			} -- Terminate the recursion.
			| useTranspositions
			, Just transpositionValue	<- Search.Transpositions.find positionHash $ Search.DynamicMoveData.getTranspositions dynamicMoveData
			, let
				selectMax''	= selectMax' $ Data.Maybe.fromMaybe (
					Control.Exception.throw $ Data.Exception.mkSearchFailure "BishBosh.Search.AlphaBeta.negaMax.descend:\tEvaluation.PositionHashQuantifiedGameTree.promoteMatchingMoves failed."	-- N.B.: perhaps because of hash-collision.
				 ) . Evaluation.PositionHashQuantifiedGameTree.promoteMatchingMoves (Search.TranspositionValue.getMoves transpositionValue)
			= if Search.TranspositionValue.inferSearchDepth transpositionValue < searchDepth
				then selectMax''	-- This transposition resulted from a search-depth which is insufficient to compose a valid response to this search.
				else let
					transposedQuantifiedGame	= findTranspositionTerminalQuantifiedGame positionHashQuantifiedGameTree transpositionValue
				in if Search.TranspositionValue.getIsOptimal transpositionValue
					then MkResult {
						getDynamicMoveData	= dynamicMoveData,
						getQuantifiedGame	= Control.Exception.assert (transposedQuantifiedGame == getQuantifiedGame selectMax'') transposedQuantifiedGame,
						getNMovesEvaluated	= 0
					}
					else Data.Maybe.maybe selectMax'' (
						\betaQuantifiedGame -> if Evaluation.QuantifiedGame.compareFitness transposedQuantifiedGame betaQuantifiedGame /= LT
							then MkResult {
								getDynamicMoveData	= dynamicMoveData,
								getQuantifiedGame	= Control.Exception.assert (betaQuantifiedGame == getQuantifiedGame selectMax'') betaQuantifiedGame,
								getNMovesEvaluated	= 0
							}
							else selectMax''
					) maybeBetaQuantifiedGame
			| otherwise	= selectMax' id
			where
				(positionHashQuantifiedGameTree, dynamicMoveData)	= Search.SearchState.getPositionHashQuantifiedGameTree &&& Search.SearchState.getDynamicMoveData $ searchState

				useTranspositions		= Data.Maybe.maybe False (searchDepth >=) maybeMinimumTranspositionSearchDepth
				(positionHash, quantifiedGame)	= Evaluation.PositionHashQuantifiedGameTree.getRootPositionHash &&& Evaluation.PositionHashQuantifiedGameTree.getRootQuantifiedGame $ positionHashQuantifiedGameTree
				game				= Evaluation.QuantifiedGame.getGame quantifiedGame	-- Prior to application of any move from the forest.
				(nPlies, nDistinctPositions)	= State.TurnsByLogicalColour.getNPlies . Model.Game.getTurnsByLogicalColour &&& State.InstancesByPosition.getNDistinctPositions . Model.Game.getInstancesByPosition $ game	-- Count the distinct positions since the last irreversible move.

				selectMax' forestSorter	= selectMax dynamicMoveData maybeAlphaQuantifiedGame . forestSorter . (
					if recordKillerMoves
						then Evaluation.PositionHashQuantifiedGameTree.sortNonCaptureMoves (
							Search.KillerMoves.sortByHistoryHeuristic (
								Model.Game.getNextLogicalColour game
							) (
								Search.DynamicMoveData.mkKillerMoveKeyFromTurn . Evaluation.QuantifiedGame.getLastTurn . Evaluation.PositionHashQuantifiedGameTree.getRootQuantifiedGame'
							) $ Search.DynamicMoveData.getKillerMoves dynamicMoveData
						) -- Dynamically advance the evaluation of killer-moves, to just after the statically sorted capture-moves.
						else id
				 ) . Data.Tree.subForest $ Evaluation.PositionHashQuantifiedGameTree.deconstruct positionHashQuantifiedGameTree
{-
				selectMax
					:: Search.DynamicMoveData.DynamicMoveData x y positionHash
					-> Maybe (Evaluation.QuantifiedGame.QuantifiedGame x y criterionValue weightedMean)
					-> [Evaluation.PositionHashQuantifiedGameTree.BarePositionHashQuantifiedGameTree x y positionHash criterionValue weightedMean]
					-> Result x y positionHash criterionValue weightedMean
-}
				selectMax dynamicMoveData' maybeAlphaQuantifiedGame' (node : remainingNodes)
					| trapRepeatedPositions
					, nDistinctPositions >= State.InstancesByPosition.leastCyclicPlies	-- CAVEAT: accounting for the typically (except when its the initial position) unrepeatable first distinct position.
					, State.InstancesByPosition.getNDistinctPositions (
						Model.Game.getInstancesByPosition . Evaluation.QuantifiedGame.getGame $ Evaluation.PositionHashQuantifiedGameTree.getRootQuantifiedGame' node	-- If the size hasn't increased, then the recently added position must have already been a member; (size == 1) during successive unrepeatable moves also, but that exception is caught above.
					) == nDistinctPositions	= selectMax dynamicMoveData' maybeAlphaQuantifiedGame' remainingNodes		-- Skip this node & recurse through the remaining moves at this depth.
					| Just betaQuantifiedGame	<- maybeBetaQuantifiedGame	-- Beta-cutoff can't occur until beta has been defined.
					, let fitnessComparedWithBeta	= Evaluation.QuantifiedGame.compareFitness quantifiedGame'' betaQuantifiedGame
					, fitnessComparedWithBeta /= LT	= result'' {
						getDynamicMoveData	= let
							game''	= Evaluation.QuantifiedGame.getGame quantifiedGame''
						in (
							if recordKillerMoves && not (
								fitnessComparedWithBeta == EQ && game'' =~ Evaluation.QuantifiedGame.getGame betaQuantifiedGame	-- CAVEAT: betaQuantifiedGame was copied in selectMax's terminal case, from one of the open-interval's boundaries.
							) -- Confirm that betaQuantifiedGame is beneath the current node.
								then updateKillerMoves game''
								else id
						) dynamicMoveData'',
						getQuantifiedGame	= betaQuantifiedGame
					} -- Beta-cutoff; the solution-space is either zero or negative.
					| otherwise	= addNMovesToResult (
						getNMovesEvaluated result''
					) $ let
						isFitter	= Data.Maybe.maybe True {-alpha is undefined => anything qualifies-} (
							(== GT) . Evaluation.QuantifiedGame.compareFitness quantifiedGame''
						 ) maybeAlphaQuantifiedGame'
					in selectMax (
						(
							if useTranspositions && isFitter
								then updateTranspositions False {-isOptimal-} nPlies positionHash {-the hash of the game before the first move in the sequence-} (
									Evaluation.QuantifiedGame.getLatestTurns nPlies quantifiedGame''	-- Discard turns previously applied to the game to which the positionHash refers.
								) positionHashQuantifiedGameTree
								else id
						) dynamicMoveData''
					) (
						if isFitter
							then Just quantifiedGame''	-- Increase alpha (i.e. the lower acceptable solution-bound).
							else maybeAlphaQuantifiedGame'
					) remainingNodes	-- Recurse through the remaining moves at this depth.
					where
						result''@MkResult {
							getDynamicMoveData	= dynamicMoveData'',
							getQuantifiedGame	= quantifiedGame''
						} = negateFitnessOfResult . descend (
							curry Evaluation.QuantifiedGame.negateInterval maybeAlphaQuantifiedGame' maybeBetaQuantifiedGame
						 ) (
							pred searchDepth
						 ) $ Search.SearchState.mkSearchState (
							Evaluation.PositionHashQuantifiedGameTree.fromBarePositionHashQuantifiedGameTree node
						 ) dynamicMoveData'	-- Recurse.
				selectMax dynamicMoveData' maybeAlphaQuantifiedGame' _	= MkResult {
					getDynamicMoveData	= dynamicMoveData',
					getQuantifiedGame	= Data.Maybe.fromMaybe (
						Data.Maybe.fromMaybe (
							Control.Exception.throw $ Data.Exception.mkResultUndefined "BishBosh.Search.AlphaBeta.negaMax.selectMax:\tneither alpha nor beta is defined."
						) maybeBetaQuantifiedGame	-- Return the only viable position known.
					) maybeAlphaQuantifiedGame',	-- Return the fittest viable position found.
					getNMovesEvaluated	= 0
				} -- Zero moves remain => terminate the recursion.
	return {-to Reader-monad-} . (
		\result@MkResult {
			getDynamicMoveData	= dynamicMoveData,
			getQuantifiedGame	= quantifiedGame
		} -> let
			positionHashQuantifiedGameTree		= Search.SearchState.getPositionHashQuantifiedGameTree initialSearchState
			nPlies					= State.TurnsByLogicalColour.getNPlies . Model.Game.getTurnsByLogicalColour . Evaluation.QuantifiedGame.getGame $ Evaluation.PositionHashQuantifiedGameTree.getRootQuantifiedGame positionHashQuantifiedGameTree
		in result {
			getDynamicMoveData	= updateTranspositions True {-Optimal-} nPlies (
				Evaluation.PositionHashQuantifiedGameTree.getRootPositionHash positionHashQuantifiedGameTree
			) (
				Evaluation.QuantifiedGame.getLatestTurns nPlies quantifiedGame
			) positionHashQuantifiedGameTree dynamicMoveData
		}
	 ) $ descend Evaluation.QuantifiedGame.unboundedInterval initialSearchDepth initialSearchState

-- | The type of a function which transforms the result.
type Transformation x y positionHash criterionValue weightedMean	= Result x y positionHash criterionValue weightedMean -> Result x y positionHash criterionValue weightedMean

-- | Mutator.
negateFitnessOfResult :: Num weightedMean => Transformation x y positionHash criterionValue weightedMean
negateFitnessOfResult result@MkResult { getQuantifiedGame = quantifiedGame }	= result {
	getQuantifiedGame	= Evaluation.QuantifiedGame.negateFitness quantifiedGame
}

-- | Mutator.
addNMovesToResult :: Component.Move.NMoves -> Transformation x y positionHash criterionValue weightedMean
addNMovesToResult nMoves result@MkResult { getNMovesEvaluated = nMovesEvaluated }	= Control.Exception.assert (nMoves > 0) result {
	getNMovesEvaluated	= nMoves + nMovesEvaluated
}

