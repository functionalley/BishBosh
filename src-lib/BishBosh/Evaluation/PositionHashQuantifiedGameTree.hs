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

	* Constructs a tree in which each node contains;
	a /Zobrist-hash/;
	a /quantifiedGame/ with one of the moves available to its parent node applied;
	& an evaluation of the fitness of the resulting position.

	* Each forest in the tree is sorted, before evaluation of its fitness is performed.

	* CAVEAT: promotions are insufficiently frequent to be treated specially when sorting.
-}

module BishBosh.Evaluation.PositionHashQuantifiedGameTree(
-- * Types
-- ** Type-synonyms
--	BarePositionHashQuantifiedGameTree,
	Forest,
-- ** Data-types
	NodeLabel(
--		MkNodeLabel,
		getPositionHash,
		getQuantifiedGame
	),
	PositionHashQuantifiedGameTree(
		MkPositionHashQuantifiedGameTree,
		deconstruct
	),
-- * Functions
	reduce,
	traceRoute,
	resign,
	traceMatchingMoveSequence,
	promoteMatchingMoveSequence,
	sortNonCaptureMoves,
-- ** Accessors
	getRootQuantifiedGame',
	getRootPositionHash,
	getRootQuantifiedGame,
-- ** Constructor
	fromBarePositionHashQuantifiedGameTree,
	mkPositionHashQuantifiedGameTree
-- ** Predicates
--	equalsLastQualifiedMove
 ) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Component.QualifiedMove		as Component.QualifiedMove
import qualified	BishBosh.Component.Turn				as Component.Turn
import qualified	BishBosh.Component.Zobrist			as Component.Zobrist
import qualified	BishBosh.Data.RoseTree				as Data.RoseTree
import qualified	BishBosh.Evaluation.Fitness			as Evaluation.Fitness
import qualified	BishBosh.Evaluation.QuantifiedGame		as Evaluation.QuantifiedGame
import qualified	BishBosh.Input.EvaluationOptions		as Input.EvaluationOptions
import qualified	BishBosh.Input.RankValues			as Input.RankValues
import qualified	BishBosh.Input.SearchOptions			as Input.SearchOptions
import qualified	BishBosh.Metric.WeightedMeanAndCriterionValues	as Metric.WeightedMeanAndCriterionValues
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.Model.GameTree				as Model.GameTree
import qualified	BishBosh.Notation.MoveNotation			as Notation.MoveNotation
import qualified	BishBosh.Property.Arboreal			as Property.Arboreal
import qualified	BishBosh.Property.Null				as Property.Null
import qualified	BishBosh.StateProperty.Hashable			as StateProperty.Hashable
import qualified	BishBosh.Type.Crypto				as Type.Crypto
import qualified	Control.Arrow
import qualified	Control.Monad.Reader
import qualified	Data.Bits
import qualified	Data.Maybe
import qualified	Data.Tree

-- | Define a node in the tree to contain the hash of a /game/ & an evaluation of the fitness of that /game/.
data NodeLabel positionHash	= MkNodeLabel {
	getPositionHash		:: positionHash,	-- ^ The hash of the /game/ contained in 'getQuantifiedGame'.
	getQuantifiedGame	:: Evaluation.QuantifiedGame.QuantifiedGame
} deriving (Eq, Show)

instance Notation.MoveNotation.ShowNotationFloat (NodeLabel positionHash) where
	showsNotationFloat moveNotation showsDouble MkNodeLabel { getQuantifiedGame = quantifiedGame }	= Notation.MoveNotation.showsNotation moveNotation (
		Evaluation.QuantifiedGame.getLastTurn quantifiedGame
	 ) . showString "\t=> " . showsDouble (
		realToFrac . Metric.WeightedMeanAndCriterionValues.getWeightedMean $ Evaluation.QuantifiedGame.getWeightedMeanAndCriterionValues quantifiedGame
	 )

instance Property.Null.Null (NodeLabel positionHash) where
	isNull MkNodeLabel { getQuantifiedGame = quantifiedGame }	= Property.Null.isNull quantifiedGame

-- | Whether the last qualifiedMove of the /game/ in a node, matches a specified /QualifiedMove/.
equalsLastQualifiedMove :: Component.QualifiedMove.QualifiedMove -> Data.RoseTree.IsMatch (NodeLabel positionHash)
equalsLastQualifiedMove qualifiedMove MkNodeLabel { getQuantifiedGame = quantifiedGame }	= (== qualifiedMove) . Component.Turn.getQualifiedMove $ Evaluation.QuantifiedGame.getLastTurn quantifiedGame

-- | The tree resulting from each possible move-choice applied to a /game/, including a position-hash & an evaluation of the resulting fitness.
type BarePositionHashQuantifiedGameTree positionHash	= Data.Tree.Tree (NodeLabel positionHash)

-- | Accessor.
getRootQuantifiedGame' :: BarePositionHashQuantifiedGameTree positionHash -> Evaluation.QuantifiedGame.QuantifiedGame
getRootQuantifiedGame' Data.Tree.Node {
	Data.Tree.rootLabel	= MkNodeLabel { getQuantifiedGame = quantifiedGame }
} = quantifiedGame

-- | Wrap the bare tree.
newtype PositionHashQuantifiedGameTree positionHash	= MkPositionHashQuantifiedGameTree {
	deconstruct	:: BarePositionHashQuantifiedGameTree positionHash
} deriving Eq

instance Property.Arboreal.Prunable (PositionHashQuantifiedGameTree positionHash) where
	prune depth MkPositionHashQuantifiedGameTree { deconstruct = barePositionHashQuantifiedGameTree }	= MkPositionHashQuantifiedGameTree $ Property.Arboreal.prune depth barePositionHashQuantifiedGameTree

instance Notation.MoveNotation.ShowNotationFloat (PositionHashQuantifiedGameTree positionHash) where
	showsNotationFloat moveNotation showsDouble MkPositionHashQuantifiedGameTree { deconstruct = barePositionHashQuantifiedGameTree } = showString $ (
		if Property.Null.isNull . Data.Tree.rootLabel $ barePositionHashQuantifiedGameTree
			then Data.RoseTree.drawForest toString . Data.Tree.subForest
			else Data.RoseTree.drawTree toString
	 ) barePositionHashQuantifiedGameTree where
		toString nodeLabel	= Notation.MoveNotation.showsNotationFloat moveNotation showsDouble nodeLabel ""

-- | Constructor.
fromBarePositionHashQuantifiedGameTree :: BarePositionHashQuantifiedGameTree positionHash -> PositionHashQuantifiedGameTree positionHash
fromBarePositionHashQuantifiedGameTree	= MkPositionHashQuantifiedGameTree

-- | Constructor.
mkPositionHashQuantifiedGameTree
	:: Data.Bits.Bits positionHash
	=> Input.EvaluationOptions.EvaluationOptions
	-> Input.SearchOptions.SearchOptions
	-> Component.Zobrist.Zobrist positionHash
	-> Model.GameTree.MoveFrequency
	-> Model.Game.Game	-- ^ The current state of the /game/.
	-> PositionHashQuantifiedGameTree positionHash
{-# SPECIALISE mkPositionHashQuantifiedGameTree
	:: Input.EvaluationOptions.EvaluationOptions
	-> Input.SearchOptions.SearchOptions
	-> Component.Zobrist.Zobrist Type.Crypto.PositionHash
	-> Model.GameTree.MoveFrequency
	-> Model.Game.Game
	-> PositionHashQuantifiedGameTree Type.Crypto.PositionHash
 #-}
mkPositionHashQuantifiedGameTree evaluationOptions searchOptions zobrist moveFrequency seedGame	= MkPositionHashQuantifiedGameTree (
	if Input.EvaluationOptions.getIncrementalEvaluation evaluationOptions
		then let
			apexPositionHash	= StateProperty.Hashable.hash seedGame zobrist
		in Data.Tree.Node {
			Data.Tree.rootLabel	= MkNodeLabel apexPositionHash $ Control.Monad.Reader.runReader (
				Evaluation.QuantifiedGame.fromGame Nothing seedGame
			) evaluationOptions,	-- Neither the previous positionHash nor the previous pieceSquareValueDifference, are available to support incremental construction.
			Data.Tree.subForest	= map (
				Data.Maybe.maybe (
					let
						slave positionHash game Data.Tree.Node {
							Data.Tree.rootLabel	= game',
							Data.Tree.subForest	= gameForest'
						} = Data.Tree.Node {
							Data.Tree.rootLabel	= MkNodeLabel positionHash' $ Control.Monad.Reader.runReader (
								Evaluation.QuantifiedGame.fromGame Nothing game'
							) evaluationOptions,
							Data.Tree.subForest	= map (slave positionHash' game') gameForest'	-- Recurse.
						} where
							positionHash'	= Model.Game.updateIncrementalPositionHash game positionHash game' zobrist
					in slave
				) (
					\pieceSquareByCoordinatesByRank -> let
						slave pieceSquareValueDifference positionHash game Data.Tree.Node {
							Data.Tree.rootLabel	= game',
							Data.Tree.subForest	= gameForest'
						} = Data.Tree.Node {
							Data.Tree.rootLabel	= MkNodeLabel positionHash' $ Control.Monad.Reader.runReader (
								Evaluation.QuantifiedGame.fromGame (Just pieceSquareValueDifference') game'
							) evaluationOptions,
							Data.Tree.subForest	= map (slave pieceSquareValueDifference' positionHash' game') gameForest'	-- Recurse.
						} where
							pieceSquareValueDifference'	= Evaluation.Fitness.measurePieceSquareValueDifferenceIncrementally pieceSquareValueDifference pieceSquareByCoordinatesByRank game'
							positionHash'			= Model.Game.updateIncrementalPositionHash game positionHash game' zobrist
					in slave $ Evaluation.Fitness.measurePieceSquareValueDifference pieceSquareByCoordinatesByRank seedGame
				) (
					Input.EvaluationOptions.getMaybePieceSquareByCoordinatesByRank evaluationOptions
				) apexPositionHash seedGame
			) $ Data.Tree.subForest bareGameTree
		}
		else uncurry MkNodeLabel . (
			(`StateProperty.Hashable.hash` zobrist) &&& (`Control.Monad.Reader.runReader` evaluationOptions) . Evaluation.QuantifiedGame.fromGame Nothing
		) <$> bareGameTree
 ) where
	bareGameTree	= Model.GameTree.deconstruct . Model.GameTree.sortGameTree (
		Input.SearchOptions.getMaybeCaptureMoveSortAlgorithm searchOptions
	 ) (
		`Input.RankValues.findRankValue` Input.EvaluationOptions.getRankValues evaluationOptions
	 ) moveFrequency $ Model.GameTree.fromGame seedGame

-- | Accessor.
getRootPositionHash :: PositionHashQuantifiedGameTree positionHash -> positionHash
getRootPositionHash MkPositionHashQuantifiedGameTree {
	deconstruct = Data.Tree.Node {
		Data.Tree.rootLabel	= MkNodeLabel { getPositionHash = positionHash }
	}
} = positionHash

-- | Accessor.
getRootQuantifiedGame :: PositionHashQuantifiedGameTree positionHash -> Evaluation.QuantifiedGame.QuantifiedGame
getRootQuantifiedGame MkPositionHashQuantifiedGameTree {
	deconstruct = Data.Tree.Node {
		Data.Tree.rootLabel	= MkNodeLabel { getQuantifiedGame = quantifiedGame }
	}
} = quantifiedGame

-- | Forward request.
reduce
	:: Data.RoseTree.IsMatch (NodeLabel positionHash)
	-> PositionHashQuantifiedGameTree positionHash
	-> Maybe (PositionHashQuantifiedGameTree positionHash)
reduce isMatch MkPositionHashQuantifiedGameTree { deconstruct = barePositionHashQuantifiedGameTree }	= MkPositionHashQuantifiedGameTree <$> Data.RoseTree.reduce isMatch barePositionHashQuantifiedGameTree

-- | Forward request.
traceRoute
	:: (Component.Turn.Turn -> Data.RoseTree.IsMatch (NodeLabel positionHash))
	-> PositionHashQuantifiedGameTree positionHash
	-> [Component.Turn.Turn]
	-> Maybe [NodeLabel positionHash]
traceRoute isMatch MkPositionHashQuantifiedGameTree { deconstruct = barePositionHashQuantifiedGameTree }	= Data.RoseTree.traceRoute isMatch barePositionHashQuantifiedGameTree

-- | Follow the specified move-sequence down the /positionHashQuantifiedGameTree/.
traceMatchingMoveSequence
	:: PositionHashQuantifiedGameTree positionHash
	-> Component.QualifiedMove.QualifiedMoveSequence
	-> Maybe [NodeLabel positionHash]	-- ^ Returns 'Nothing', on failure to match a move.
traceMatchingMoveSequence MkPositionHashQuantifiedGameTree { deconstruct = barePositionHashQuantifiedGameTree }	= Data.RoseTree.traceRoute equalsLastQualifiedMove barePositionHashQuantifiedGameTree

-- | Amend the apex-game to reflect the resignation of the next player.
resign :: PositionHashQuantifiedGameTree positionHash -> PositionHashQuantifiedGameTree positionHash
resign MkPositionHashQuantifiedGameTree {
	deconstruct	= barePositionHashQuantifiedGameTree@Data.Tree.Node {
		Data.Tree.rootLabel	= nodeLabel@MkNodeLabel { getQuantifiedGame = quantifiedGame }
	}
} = MkPositionHashQuantifiedGameTree $ barePositionHashQuantifiedGameTree {
	Data.Tree.rootLabel	= nodeLabel {
		getQuantifiedGame	= quantifiedGame { Evaluation.QuantifiedGame.getGame = Model.Game.resign $ Evaluation.QuantifiedGame.getGame quantifiedGame }
	}
}

-- | Self-documentation.
type Forest positionHash	= [BarePositionHashQuantifiedGameTree positionHash]

{- |
	* Promotes the first matching /move/ to the head of the forest, then descends & recursively promotes the next matching move in the sub-forest.

	* N.B.: this can be used to dynamically re-order the forest when a transposition is detected.
-}
promoteMatchingMoveSequence
	:: Component.QualifiedMove.QualifiedMoveSequence	-- ^ The list of qualifiedMoves, which should be promoted at successively deeper levels in the tree.
	-> Forest positionHash
	-> Maybe (Forest positionHash)				-- ^ Returns 'Nothing' on failure to match a move.
promoteMatchingMoveSequence	= Data.RoseTree.promote equalsLastQualifiedMove

{- |
	* Sorts the forest, starting just after any initial capture-moves.

	* N.B.: this can be used to dynamically re-order the forest using the killer heuristic.
-}
sortNonCaptureMoves
	:: (Forest positionHash -> Forest positionHash)
	-> Forest positionHash
	-> Forest positionHash
sortNonCaptureMoves sortForest	= uncurry (++) . Control.Arrow.second sortForest . span (
	Component.Turn.isCapture . Evaluation.QuantifiedGame.getLastTurn . getRootQuantifiedGame'	-- Shield any capture-moves, which were previously advanced by static sorting, from the sort.
 )

