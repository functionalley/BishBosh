{-# LANGUAGE CPP, FlexibleContexts #-}
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
	traceMatchingMoves,
	promoteMatchingMoves,
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
import qualified	BishBosh.Attribute.RankValues			as Attribute.RankValues
import qualified	BishBosh.Component.QualifiedMove		as Component.QualifiedMove
import qualified	BishBosh.Component.Turn				as Component.Turn
import qualified	BishBosh.Component.Zobrist			as Component.Zobrist
import qualified	BishBosh.Data.RoseTree				as Data.RoseTree
import qualified	BishBosh.Evaluation.Fitness			as Evaluation.Fitness
import qualified	BishBosh.Evaluation.QuantifiedGame		as Evaluation.QuantifiedGame
import qualified	BishBosh.Input.EvaluationOptions		as Input.EvaluationOptions
import qualified	BishBosh.Input.SearchOptions			as Input.SearchOptions
import qualified	BishBosh.Metric.WeightedMeanAndCriterionValues	as Metric.WeightedMeanAndCriterionValues
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.Model.GameTree				as Model.GameTree
import qualified	BishBosh.Notation.MoveNotation			as Notation.MoveNotation
import qualified	BishBosh.Property.Arboreal			as Property.Arboreal
import qualified	BishBosh.Property.Null				as Property.Null
import qualified	BishBosh.Type.Crypto				as Type.Crypto
import qualified	BishBosh.Type.Length				as Type.Length
import qualified	BishBosh.Type.Mass				as Type.Mass
import qualified	Control.Arrow
import qualified	Control.Monad.Reader
import qualified	Data.Array.IArray
import qualified	Data.Bits
import qualified	Data.Maybe
import qualified	Data.Tree

#ifdef USE_UNBOXED_ARRAYS
import qualified	Data.Array.Unboxed
#endif

-- | Define a node in the tree to contain the hash of a /game/ & an evaluation of the fitness of that /game/.
data NodeLabel x y positionHash	= MkNodeLabel {
	getPositionHash		:: positionHash,	-- ^ The hash of the /game/ contained in 'getQuantifiedGame'.
	getQuantifiedGame	:: Evaluation.QuantifiedGame.QuantifiedGame x y
} deriving (Eq, Show)

instance (Enum x, Enum y) => Notation.MoveNotation.ShowNotationFloat (NodeLabel x y positionHash) where
	showsNotationFloat moveNotation showsDouble MkNodeLabel { getQuantifiedGame = quantifiedGame }	= Notation.MoveNotation.showsNotation moveNotation (
		Evaluation.QuantifiedGame.getLastTurn quantifiedGame
	 ) . showString "\t=> " . showsDouble (
		realToFrac . Metric.WeightedMeanAndCriterionValues.getWeightedMean $ Evaluation.QuantifiedGame.getWeightedMeanAndCriterionValues quantifiedGame
	 )

instance Property.Null.Null (NodeLabel x y positionHash) where
	isNull MkNodeLabel { getQuantifiedGame = quantifiedGame }	= Property.Null.isNull quantifiedGame

-- | Whether the last qualifiedMove of the /game/ in a node, matches a specified /QualifiedMove/.
equalsLastQualifiedMove :: (Eq x, Eq y) => Component.QualifiedMove.QualifiedMove x y -> Data.RoseTree.IsMatch (NodeLabel x y positionHash)
equalsLastQualifiedMove qualifiedMove MkNodeLabel { getQuantifiedGame = quantifiedGame }	= (== qualifiedMove) . Component.Turn.getQualifiedMove $ Evaluation.QuantifiedGame.getLastTurn quantifiedGame

-- | The tree resulting from each possible move-choice applied to a /game/, including a position-hash & an evaluation of the resulting fitness.
type BarePositionHashQuantifiedGameTree x y positionHash	= Data.Tree.Tree (NodeLabel x y positionHash)

-- | Accessor.
getRootQuantifiedGame' :: BarePositionHashQuantifiedGameTree x y positionHash -> Evaluation.QuantifiedGame.QuantifiedGame x y
getRootQuantifiedGame' Data.Tree.Node {
	Data.Tree.rootLabel	= MkNodeLabel { getQuantifiedGame = quantifiedGame }
} = quantifiedGame

-- | Wrap the bare tree.
newtype PositionHashQuantifiedGameTree x y positionHash	= MkPositionHashQuantifiedGameTree {
	deconstruct	:: BarePositionHashQuantifiedGameTree x y positionHash
} deriving Eq

instance Property.Arboreal.Prunable (PositionHashQuantifiedGameTree x y positionHash) where
	prune depth MkPositionHashQuantifiedGameTree { deconstruct = barePositionHashQuantifiedGameTree }	= MkPositionHashQuantifiedGameTree $ Property.Arboreal.prune depth barePositionHashQuantifiedGameTree

instance (Enum	x, Enum	y) => Notation.MoveNotation.ShowNotationFloat (PositionHashQuantifiedGameTree x y positionHash) where
	showsNotationFloat moveNotation showsDouble MkPositionHashQuantifiedGameTree { deconstruct = barePositionHashQuantifiedGameTree } = showString $ (
		if Property.Null.isNull . Data.Tree.rootLabel $ barePositionHashQuantifiedGameTree
			then Data.RoseTree.drawForest toString . Data.Tree.subForest
			else Data.RoseTree.drawTree toString
	 ) barePositionHashQuantifiedGameTree where
		toString nodeLabel	= Notation.MoveNotation.showsNotationFloat moveNotation showsDouble nodeLabel ""

-- | Constructor.
fromBarePositionHashQuantifiedGameTree :: BarePositionHashQuantifiedGameTree x y positionHash -> PositionHashQuantifiedGameTree x y positionHash
fromBarePositionHashQuantifiedGameTree	= MkPositionHashQuantifiedGameTree

-- | Constructor.
mkPositionHashQuantifiedGameTree :: (
	Data.Array.IArray.Ix					x,
#ifdef USE_UNBOXED_ARRAYS
	Data.Array.Unboxed.IArray Data.Array.Unboxed.UArray	pieceSquareValue,	-- Requires 'FlexibleContexts'. The unboxed representation of the array-element must be defined (& therefore must be of fixed size).
#endif
	Data.Bits.Bits						positionHash,
	Fractional						pieceSquareValue,
	Fractional						rankValue,
	Integral						x,
	Integral						y,
	Real							pieceSquareValue,
	Real							rankValue,
	Show							x,
	Show							y
 )
	=> Input.EvaluationOptions.EvaluationOptions pieceSquareValue rankValue x y
	-> Input.SearchOptions.SearchOptions
	-> Component.Zobrist.Zobrist x y positionHash
	-> Model.GameTree.MoveFrequency x y
	-> Model.Game.Game x y	-- ^ The current state of the /game/.
	-> PositionHashQuantifiedGameTree x y positionHash
{-# SPECIALISE mkPositionHashQuantifiedGameTree
	:: Input.EvaluationOptions.EvaluationOptions Type.Mass.PieceSquareValue Type.Mass.RankValue Type.Length.X Type.Length.Y
	-> Input.SearchOptions.SearchOptions
	-> Component.Zobrist.Zobrist Type.Length.X Type.Length.Y Type.Crypto.PositionHash
	-> Model.GameTree.MoveFrequency Type.Length.X Type.Length.Y
	-> Model.Game.Game Type.Length.X Type.Length.Y
	-> PositionHashQuantifiedGameTree Type.Length.X Type.Length.Y Type.Crypto.PositionHash
 #-}
mkPositionHashQuantifiedGameTree evaluationOptions searchOptions zobrist moveFrequency seedGame	= MkPositionHashQuantifiedGameTree (
	if Input.EvaluationOptions.getIncrementalEvaluation evaluationOptions
		then let
			apexPositionHash	= Component.Zobrist.hash2D seedGame zobrist
		in Data.Tree.Node {
			Data.Tree.rootLabel	= MkNodeLabel apexPositionHash $ Control.Monad.Reader.runReader (
				Evaluation.QuantifiedGame.fromGame Nothing seedGame
			) evaluationOptions,	-- Neither the previous positionHash nor the previous pieceSquareValue, are available to support incremental construction.
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
						slave pieceSquareValue positionHash game Data.Tree.Node {
							Data.Tree.rootLabel	= game',
							Data.Tree.subForest	= gameForest'
						} = Data.Tree.Node {
							Data.Tree.rootLabel	= MkNodeLabel positionHash' $ Control.Monad.Reader.runReader (
								Evaluation.QuantifiedGame.fromGame (Just pieceSquareValue') game'
							) evaluationOptions,
							Data.Tree.subForest	= map (slave pieceSquareValue' positionHash' game') gameForest'	-- Recurse.
						} where
							pieceSquareValue'	= Evaluation.Fitness.measurePieceSquareValueIncrementally pieceSquareValue pieceSquareByCoordinatesByRank game'
							positionHash'		= Model.Game.updateIncrementalPositionHash game positionHash game' zobrist
					in slave $ Evaluation.Fitness.measurePieceSquareValue pieceSquareByCoordinatesByRank seedGame
				) (
					Input.EvaluationOptions.getMaybePieceSquareByCoordinatesByRank evaluationOptions
				) apexPositionHash seedGame
			) $ Data.Tree.subForest bareGameTree
		}
		else fmap (
			uncurry MkNodeLabel . (
				(`Component.Zobrist.hash2D` zobrist) &&& (`Control.Monad.Reader.runReader` evaluationOptions) . Evaluation.QuantifiedGame.fromGame Nothing
			)
		) bareGameTree
 ) where
	bareGameTree	= Model.GameTree.deconstruct . Model.GameTree.sortGameTree (
		Input.SearchOptions.getMaybeCaptureMoveSortAlgorithm searchOptions
	 ) (
		`Attribute.RankValues.findRankValue` Input.EvaluationOptions.getRankValues evaluationOptions
	 ) moveFrequency $ Model.GameTree.fromGame seedGame

-- | Accessor.
getRootPositionHash :: PositionHashQuantifiedGameTree x y positionHash -> positionHash
getRootPositionHash MkPositionHashQuantifiedGameTree {
	deconstruct = Data.Tree.Node {
		Data.Tree.rootLabel	= MkNodeLabel { getPositionHash = positionHash }
	}
} = positionHash

-- | Accessor.
getRootQuantifiedGame :: PositionHashQuantifiedGameTree x y positionHash -> Evaluation.QuantifiedGame.QuantifiedGame x y
getRootQuantifiedGame MkPositionHashQuantifiedGameTree {
	deconstruct = Data.Tree.Node {
		Data.Tree.rootLabel	= MkNodeLabel { getQuantifiedGame = quantifiedGame }
	}
} = quantifiedGame

-- | Forward request.
reduce
	:: Data.RoseTree.IsMatch (NodeLabel x y positionHash)
	-> PositionHashQuantifiedGameTree x y positionHash
	-> Maybe (PositionHashQuantifiedGameTree x y positionHash)
reduce isMatch MkPositionHashQuantifiedGameTree { deconstruct = barePositionHashQuantifiedGameTree }	= MkPositionHashQuantifiedGameTree `fmap` Data.RoseTree.reduce isMatch barePositionHashQuantifiedGameTree

-- | Forward request.
traceRoute
	:: (Component.Turn.Turn x y -> Data.RoseTree.IsMatch (NodeLabel x y positionHash))
	-> PositionHashQuantifiedGameTree x y positionHash
	-> [Component.Turn.Turn x y]
	-> Maybe [NodeLabel x y positionHash]
traceRoute isMatch MkPositionHashQuantifiedGameTree { deconstruct = barePositionHashQuantifiedGameTree }	= Data.RoseTree.traceRoute isMatch barePositionHashQuantifiedGameTree

-- | Follow the specified move-sequence down the /positionHashQuantifiedGameTree/.
traceMatchingMoves
	:: (Eq x, Eq y)
	=> PositionHashQuantifiedGameTree x y positionHash
	-> [Component.QualifiedMove.QualifiedMove x y]
	-> Maybe [NodeLabel x y positionHash]	-- ^ Returns 'Nothing', on failure to match a move.
traceMatchingMoves MkPositionHashQuantifiedGameTree { deconstruct = barePositionHashQuantifiedGameTree }	= Data.RoseTree.traceRoute equalsLastQualifiedMove barePositionHashQuantifiedGameTree

-- | Amend the apex-game to reflect the resignation of the next player.
resign :: PositionHashQuantifiedGameTree x y positionHash -> PositionHashQuantifiedGameTree x y positionHash
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
type Forest x y positionHash	= [BarePositionHashQuantifiedGameTree x y positionHash]

{- |
	* Promotes the first matching /move/ to the head of the forest, then descends & recursively promotes the next matching move in the sub-forest.

	* N.B.: this can be used to dynamically re-order the forest when a transposition is detected.
-}
promoteMatchingMoves
	:: (Eq x, Eq y)
	=> [Component.QualifiedMove.QualifiedMove x y]	-- ^ The list of qualifiedMoves, which should be promoted at successively deeper levels in the tree.
	-> Forest x y positionHash
	-> Maybe (Forest x y positionHash)		-- ^ Returns 'Nothing' on failure to match a move.
promoteMatchingMoves	= Data.RoseTree.promote equalsLastQualifiedMove

{- |
	* Sorts the forest, starting just after any initial capture-moves.

	* N.B.: this can be used to dynamically re-order the forest using the killer heuristic.
-}
sortNonCaptureMoves
	:: (Forest x y positionHash -> Forest x y positionHash)
	-> Forest x y positionHash
	-> Forest x y positionHash
sortNonCaptureMoves sortForest	= uncurry (++) . Control.Arrow.second sortForest . span (
	Component.Turn.isCapture . Evaluation.QuantifiedGame.getLastTurn . getRootQuantifiedGame'	-- Shield any capture-moves, which were previously advanced by static sorting, from the sort.
 )

