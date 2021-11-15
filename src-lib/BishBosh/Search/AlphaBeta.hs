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
--	addNPositionsToResult
 ) where

import			BishBosh.Evaluation.QuantifiedGame((<=>), (===))
import			Control.Applicative((<|>))
import			Control.Arrow((&&&))
import qualified	BishBosh.Component.QualifiedMove			as Component.QualifiedMove
import qualified	BishBosh.Component.Turn					as Component.Turn
import qualified	BishBosh.Data.Exception					as Data.Exception
import qualified	BishBosh.Evaluation.PositionHashQuantifiedGameTree	as Evaluation.PositionHashQuantifiedGameTree
import qualified	BishBosh.Evaluation.QuantifiedGame			as Evaluation.QuantifiedGame
import qualified	BishBosh.Input.SearchOptions				as Input.SearchOptions
import qualified	BishBosh.Model.Game					as Model.Game
import qualified	BishBosh.Notation.MoveNotation				as Notation.MoveNotation
import qualified	BishBosh.Property.Arboreal				as Property.Arboreal
import qualified	BishBosh.Search.DynamicMoveData				as Search.DynamicMoveData
import qualified	BishBosh.Search.KillerMoves				as Search.KillerMoves
import qualified	BishBosh.Search.SearchState				as Search.SearchState
import qualified	BishBosh.Search.Transpositions				as Search.Transpositions
import qualified	BishBosh.Search.TranspositionValue			as Search.TranspositionValue
import qualified	BishBosh.State.InstancesByPosition			as State.InstancesByPosition
import qualified	BishBosh.State.TurnsByLogicalColour			as State.TurnsByLogicalColour
import qualified	BishBosh.Type.Count					as Type.Count
import qualified	BishBosh.Type.Crypto					as Type.Crypto
import qualified	Control.Exception
import qualified	Control.Monad.Reader
import qualified	Data.Default
import qualified	Data.Maybe
import qualified	Data.Tree

-- | The type returned by 'negaMax'.
data Result positionHash	= MkResult {
	getDynamicMoveData	:: Search.DynamicMoveData.DynamicMoveData positionHash,	-- ^ Killer moves & Transpositions.
	getQuantifiedGame	:: Evaluation.QuantifiedGame.QuantifiedGame,
	getNPositionsEvaluated	:: Type.Count.NPositions				-- ^ The total number of nodes analysed, before making the selection.
}

{- |
	* Drop the specified number of plies; typically those made before starting the search.

	* CAVEAT: abandons the fitness component of the quantified game.
-}
extractSelectedTurns
	:: Type.Count.NPlies
	-> Result positionHash
	-> (Search.DynamicMoveData.DynamicMoveData positionHash, [Component.Turn.Turn], Type.Count.NPositions)
extractSelectedTurns nPlies MkResult {
	getDynamicMoveData	= dynamicMoveData,
	getQuantifiedGame	= quantifiedGame,
	getNPositionsEvaluated	= nPositionsEvaluated
} = (
	dynamicMoveData,
	Evaluation.QuantifiedGame.getLatestTurns nPlies quantifiedGame,
	nPositionsEvaluated
 )

-- | Record the last move as a killer, unless it's a capture move.
updateKillerMoves :: Model.Game.Game -> Search.DynamicMoveData.Transformation positionHash
updateKillerMoves game
	| Just lastTurn <- Model.Game.maybeLastTurn game	= if Component.Turn.isCapture lastTurn
		then id	-- This move was (assuming appropriate Search-options) statically sorted.
		else Search.DynamicMoveData.updateKillerMoves . Search.KillerMoves.insert (
			State.TurnsByLogicalColour.getNPlies $ Model.Game.getTurnsByLogicalColour game
		) $ Search.DynamicMoveData.mkKillerMoveKeyFromTurn lastTurn
	| otherwise						= Control.Exception.throw . Data.Exception.mkNullDatum . showString "BishBosh.Search.AlphaBeta.updateKillerMoves:\tzero turns have been made; " $ shows game "."

{- |
	* Track the specified move-sequence down the /positionHashQuantifiedGameTree/ & retrieve the fitness from the terminal quantified game.

	* CAVEAT: the return-value, is quantified from the perspective of the player who is about to move.
-}
findTranspositionTerminalQuantifiedGame
	:: Evaluation.PositionHashQuantifiedGameTree.PositionHashQuantifiedGameTree positionHash
	-> Search.TranspositionValue.TranspositionValue Component.QualifiedMove.QualifiedMove
	-> Evaluation.QuantifiedGame.QuantifiedGame
findTranspositionTerminalQuantifiedGame positionHashQuantifiedGameTree transpositionValue	= Data.Maybe.maybe (
	Control.Exception.throw . Data.Exception.mkSearchFailure . showString "BishBosh.Search.AlphaBeta.findTranspositionTerminalQuantifiedGame:\tEvaluation.PositionHashQuantifiedGameTree.traceMatchingMoveSequence failed; " . shows transpositionValue . showString ":\n" $ (
		Notation.MoveNotation.showsNotationFloatToNDecimals Data.Default.def {-move-notation-} 3 {-decimal digits-} $ Property.Arboreal.prune (fromIntegral inferredSearchDepth) positionHashQuantifiedGameTree
	 ) ""
 ) (
	(
		if even inferredSearchDepth
			then Evaluation.QuantifiedGame.negateFitness	-- The opponent made the last move in the list, & therefore defined the fitness.
			else id
	) . Evaluation.PositionHashQuantifiedGameTree.getQuantifiedGame . last
 ) . Evaluation.PositionHashQuantifiedGameTree.traceMatchingMoveSequence positionHashQuantifiedGameTree $ Search.TranspositionValue.getQualifiedMoves transpositionValue	where
	inferredSearchDepth	= Search.TranspositionValue.inferSearchDepth transpositionValue

-- | Record a qualifiedMove-sequence in the transposition-table.
updateTranspositions
	:: Ord positionHash
	=> Search.TranspositionValue.IsOptimal
	-> Type.Count.NPlies
	-> positionHash
	-> [Component.Turn.Turn]
	-> Evaluation.PositionHashQuantifiedGameTree.PositionHashQuantifiedGameTree positionHash
	-> Search.DynamicMoveData.Transformation positionHash
updateTranspositions isOptimal nPlies positionHash turns positionHashQuantifiedGameTree	= Search.DynamicMoveData.updateTranspositions . Search.Transpositions.insert (
	Evaluation.QuantifiedGame.getFitness . findTranspositionTerminalQuantifiedGame positionHashQuantifiedGameTree
 ) positionHash {-the hash of the game before the first move in the sequence-} . Search.TranspositionValue.mkTranspositionValue isOptimal nPlies $ map Component.Turn.getQualifiedMove turns

{- |
	* Implements a depth-first search (implemented as nega-max), with alpha-beta pruning.

	* /alpha/ is the minimum fitness of which the maximising player is assured.

	* /beta/ is the maximum fitness which the minimising player will tolerate.
-}
negaMax
	:: Ord positionHash
	=> Type.Count.NPlies	-- ^ The depth to which the tree should be searched; i.e. the number of plies to look-ahead.
	-> Search.SearchState.SearchState positionHash
	-> Input.SearchOptions.Reader (Result positionHash)
{-# SPECIALISE negaMax :: Type.Count.NPlies -> Search.SearchState.SearchState Type.Crypto.PositionHash -> Input.SearchOptions.Reader (Result Type.Crypto.PositionHash) #-}
negaMax initialSearchDepth initialSearchState	= do
	maybeMinimumTranspositionSearchDepth	<- Control.Monad.Reader.asks Input.SearchOptions.maybeMinimumTranspositionSearchDepth
	recordKillerMoves			<- Control.Monad.Reader.asks Input.SearchOptions.recordKillerMoves
	trapRepeatedPositions			<- Control.Monad.Reader.asks Input.SearchOptions.getTrapRepeatedPositions

	let
		getNPlies	= State.TurnsByLogicalColour.getNPlies . Model.Game.getTurnsByLogicalColour	-- Abbreviate.

		descend :: Ord positionHash => Evaluation.QuantifiedGame.OpenInterval -> Type.Count.NPlies -> Search.SearchState.SearchState positionHash -> Result positionHash
		descend (maybeAlphaQuantifiedGame, maybeBetaQuantifiedGame) searchDepth searchState
			| searchDepth == 0 || Model.Game.isTerminated game	= MkResult {
				getDynamicMoveData	= dynamicMoveData,
				getQuantifiedGame	= Evaluation.QuantifiedGame.negateFitness quantifiedGame,	-- CAVEAT: zero new moves have been applied, so the last move was the opponent's.
				getNPositionsEvaluated	= 1								-- Fitness-negation requires evaluation.
			} -- Terminate the recursion.
			| useTranspositions
			, Just transpositionValue	<- Search.Transpositions.find positionHash $ Search.DynamicMoveData.getTranspositions dynamicMoveData	-- Look for a previously encountered position with a matching positionHash.
			, let
--				selectMaxUsingTranspositions :: Result positionHash
				selectMaxUsingTranspositions	= selectMaxWithSorter $ Data.Maybe.fromMaybe (
					Control.Exception.throw . Data.Exception.mkSearchFailure . showString "BishBosh.Search.AlphaBeta.negaMax.descend:\tEvaluation.PositionHashQuantifiedGameTree.promoteMatchingMoveSequence failed; " $ shows transpositionValue "."	-- N.B.: perhaps because of hash-collision.
				 ) . Evaluation.PositionHashQuantifiedGameTree.promoteMatchingMoveSequence (
					Search.TranspositionValue.getQualifiedMoves transpositionValue
				 ) -- For efficiency, promote moves in the positionHashQuantifiedGameTree, using the knowledge in the transposition.
			= if Search.TranspositionValue.inferSearchDepth transpositionValue < searchDepth
				then selectMaxUsingTranspositions	-- This transposition resulted from a search-depth which is insufficient to compose a valid response to this search.
				else let
					transposedQuantifiedGame	= findTranspositionTerminalQuantifiedGame positionHashQuantifiedGameTree transpositionValue
				in if Search.TranspositionValue.getIsOptimal transpositionValue
					then MkResult {
						getDynamicMoveData	= dynamicMoveData,
						getQuantifiedGame	= Control.Exception.assert (transposedQuantifiedGame == getQuantifiedGame selectMaxUsingTranspositions) transposedQuantifiedGame,
						getNPositionsEvaluated	= 0
					}
					else Data.Maybe.maybe selectMaxUsingTranspositions (
						\betaQuantifiedGame -> if uncurry (<) $ (($ transposedQuantifiedGame) &&& ($ betaQuantifiedGame)) Evaluation.QuantifiedGame.getFitness
							then selectMaxUsingTranspositions
							else MkResult {
								getDynamicMoveData	= dynamicMoveData,
								getQuantifiedGame	= Control.Exception.assert (betaQuantifiedGame == getQuantifiedGame selectMaxUsingTranspositions) betaQuantifiedGame,
								getNPositionsEvaluated	= 0
							} -- Beta-cutoff.
					) maybeBetaQuantifiedGame
			| otherwise	= selectMaxWithSorter id
			where
				(positionHashQuantifiedGameTree, dynamicMoveData)	= Search.SearchState.getPositionHashQuantifiedGameTree &&& Search.SearchState.getDynamicMoveData $ searchState

				useTranspositions		= Data.Maybe.maybe False (searchDepth >=) maybeMinimumTranspositionSearchDepth
				(positionHash, quantifiedGame)	= Evaluation.PositionHashQuantifiedGameTree.getRootPositionHash &&& Evaluation.PositionHashQuantifiedGameTree.getRootQuantifiedGame $ positionHashQuantifiedGameTree
				game				= Evaluation.QuantifiedGame.getGame quantifiedGame	-- Prior to application of any move from the forest.
				(nPlies, nDistinctPositions)	= getNPlies &&& State.InstancesByPosition.getNDistinctPositions . Model.Game.getInstancesByPosition $ game	-- Count the distinct positions since the last irreversible move.

--				selectMaxWithSorter :: (Evaluation.PositionHashQuantifiedGameTree.Forest positionHash -> Evaluation.PositionHashQuantifiedGameTree.Forest positionHash) -> Result positionHash
				selectMaxWithSorter forestSorter	= selectMax dynamicMoveData maybeAlphaQuantifiedGame . forestSorter . (
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

--				selectMax :: Search.DynamicMoveData.DynamicMoveData positionHash -> Maybe Evaluation.QuantifiedGame.QuantifiedGame -> Evaluation.PositionHashQuantifiedGameTree.Forest positionHash -> Result positionHash
				selectMax dynamicMoveData' maybeAlphaQuantifiedGame' (node : remainingNodes)
					| trapRepeatedPositions
					, nDistinctPositions >= fromIntegral State.InstancesByPosition.leastCyclicPlies	-- CAVEAT: accounting for the typically (except the initial position) unrepeatable first distinct position.
					, State.InstancesByPosition.getNDistinctPositions (
						Model.Game.getInstancesByPosition . Evaluation.QuantifiedGame.getGame $ Evaluation.PositionHashQuantifiedGameTree.getRootQuantifiedGame' node	-- If the size hasn't increased, then the recently added position must have already been a member; (size == 1) during successive unrepeatable moves also, but that exception is caught above.
					) == nDistinctPositions		= selectMax dynamicMoveData' (
						maybeAlphaQuantifiedGame' <|> Just quantifiedGame''	-- CAVEAT: guard against exhausting all nodes without defining alpha.
					) remainingNodes						-- Skip this repetitive node & recurse through the remaining nodes at this depth.
					| Just betaQuantifiedGame	<- maybeBetaQuantifiedGame	-- Beta-cutoff can't occur until beta has been defined.
					, let fitnessComparedWithBeta	= quantifiedGame'' <=> betaQuantifiedGame
					, fitnessComparedWithBeta /= LT	= result'' {
						getDynamicMoveData	= (
							if recordKillerMoves && not (
								fitnessComparedWithBeta == EQ && quantifiedGame'' === betaQuantifiedGame	-- betaQuantifiedGame was copied in the terminal case of 'selectMax', from one of the open-interval's boundaries.
							) -- Confirm that betaQuantifiedGame is beneath the current node.
								then updateKillerMoves $ Evaluation.QuantifiedGame.getGame quantifiedGame''
								else id
						) dynamicMoveData'',
						getQuantifiedGame	= betaQuantifiedGame
					} -- Beta-cutoff; the solution-space is either zero or negative.
					| otherwise	= let
						isFitter	= Data.Maybe.maybe True {-alpha is undefined => anything qualifies-} (
							\alphaQuantifiedGame -> uncurry (>) $ (($ quantifiedGame'') &&& ($ alphaQuantifiedGame)) Evaluation.QuantifiedGame.getFitness
						 ) maybeAlphaQuantifiedGame'
					in addNPositionsToResult (
						getNPositionsEvaluated result''
					) $ selectMax (
						(
							if useTranspositions && isFitter
								then updateTranspositions False {-isOptimal-} nPlies positionHash {-the hash of the game before the first move in the sequence-} (
									Evaluation.QuantifiedGame.getLatestTurns nPlies quantifiedGame''	-- Discard turns previously applied to the game to which the positionHash refers.
								) positionHashQuantifiedGameTree
								else id
						) dynamicMoveData''
					) (
						if isFitter
							then Just quantifiedGame''	-- Increase the alpha solution (i.e. the lower acceptable solution-bound).
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
				selectMax dynamicMoveData' maybeAlphaQuantifiedGame' []	= MkResult {
					getDynamicMoveData	= dynamicMoveData',
					getQuantifiedGame	= Data.Maybe.fromMaybe (
						Data.Maybe.fromMaybe (
							Control.Exception.throw . Data.Exception.mkResultUndefined . showString "BishBosh.Search.AlphaBeta.negaMax.descend.selectMax:\tthere are zero nodes to process, but neither alpha nor beta is defined; " $ shows game "."
						) maybeBetaQuantifiedGame	-- Return the only viable position known.
					) maybeAlphaQuantifiedGame',	-- Return the fittest viable position found.
					getNPositionsEvaluated	= 0
				} -- Zero moves remain => terminate the recursion.
	return {-to Reader-monad-} . (
		\result@MkResult {
			getDynamicMoveData	= dynamicMoveData,
			getQuantifiedGame	= quantifiedGame
		} -> let
			positionHashQuantifiedGameTree	= Search.SearchState.getPositionHashQuantifiedGameTree initialSearchState
			nPlies				= getNPlies . Evaluation.QuantifiedGame.getGame $ Evaluation.PositionHashQuantifiedGameTree.getRootQuantifiedGame positionHashQuantifiedGameTree
		in result {
			getDynamicMoveData	= updateTranspositions True {-Optimal-} nPlies (
				Evaluation.PositionHashQuantifiedGameTree.getRootPositionHash positionHashQuantifiedGameTree
			) (
				Evaluation.QuantifiedGame.getLatestTurns nPlies quantifiedGame
			) positionHashQuantifiedGameTree dynamicMoveData
		}
	 ) $ descend Evaluation.QuantifiedGame.unboundedInterval initialSearchDepth initialSearchState

-- | The type of a function which transforms the result.
type Transformation positionHash	= Result positionHash -> Result positionHash

-- | Mutator.
negateFitnessOfResult :: Transformation positionHash
negateFitnessOfResult result@MkResult { getQuantifiedGame = quantifiedGame }	= result {
	getQuantifiedGame	= Evaluation.QuantifiedGame.negateFitness quantifiedGame
}

-- | Mutator.
addNPositionsToResult :: Type.Count.NPositions -> Transformation positionHash
addNPositionsToResult nPositions result@MkResult { getNPositionsEvaluated = nPositionsEvaluated }	= Control.Exception.assert (nPositions > 0) result {
	getNPositionsEvaluated	= nPositions + nPositionsEvaluated
}

