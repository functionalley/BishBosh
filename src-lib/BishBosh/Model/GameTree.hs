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

	* Defines chess as a constant tree of all possible moves.

	* Because of the conceptually infinite size of this data-structure, care must be taken not to attempt to call 'show', '(==)', ...
-}

module BishBosh.Model.GameTree(
-- * Types
-- ** Type-synonyms
--	BareGameTree,
	MoveFrequency,
--	Transformation,
-- ** Data-types
	GameTree(
--		MkGameTree,
		deconstruct
	),
-- * Function
--	compareByMVVLVA,
--	getLastMove,
--	staticExchangeEvaluation,
--	getRankAndMove,
	countGames,
	countMoves,
	traceRoute,
	sortGameTree,
	toMoveFrequency,
-- ** Constructor
	fromBareGameTree,
	fromGame
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.CaptureMoveSortAlgorithm	as Attribute.CaptureMoveSortAlgorithm
import qualified	BishBosh.Attribute.LogicalColour		as Attribute.LogicalColour
import qualified	BishBosh.Attribute.MoveType			as Attribute.MoveType
import qualified	BishBosh.Attribute.Rank				as Attribute.Rank
import qualified	BishBosh.Component.Move				as Component.Move
import qualified	BishBosh.Component.QualifiedMove		as Component.QualifiedMove
import qualified	BishBosh.Component.Turn				as Component.Turn
import qualified	BishBosh.Data.Exception				as Data.Exception
import qualified	BishBosh.Data.RoseTree				as Data.RoseTree
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.Model.MoveFrequency			as Model.MoveFrequency
import qualified	BishBosh.Notation.MoveNotation			as Notation.MoveNotation
import qualified	BishBosh.Property.Empty				as Property.Empty
import qualified	BishBosh.Property.Null				as Property.Null
import qualified	BishBosh.Property.Tree				as Property.Tree
import qualified	BishBosh.State.TurnsByLogicalColour		as State.TurnsByLogicalColour
import qualified	BishBosh.Types					as T
import qualified	Control.Exception
import qualified	Data.Default
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.Maybe
import qualified	Data.Tree

-- | Each node defines the state of the game.
type BareGameTree x y	= Data.Tree.Tree (Model.Game.Game x y)

-- | Forwards request to 'Component.Turn.compareByMVVLVA'.
compareByMVVLVA
	:: Ord rankValue
	=> Attribute.Rank.EvaluateRank rankValue
	-> BareGameTree x y
	-> BareGameTree x y
	-> Ordering
compareByMVVLVA evaluateRank Data.Tree.Node { Data.Tree.rootLabel = gameL } Data.Tree.Node { Data.Tree.rootLabel = gameR }	= uncurry (
	Component.Turn.compareByMVVLVA evaluateRank
 ) . (
	($ gameL) &&& ($ gameR)
 ) $ Data.Maybe.fromJust . Model.Game.maybeLastTurn

-- | Get the last move responsible for the current position.
getLastMove :: BareGameTree x y -> Component.Move.Move x y
{-# INLINE getLastMove #-}
getLastMove Data.Tree.Node { Data.Tree.rootLabel = game }	= Component.QualifiedMove.getMove . Component.Turn.getQualifiedMove . Data.Maybe.fromJust $ Model.Game.maybeLastTurn game

{- |
	* <https://www.chessprogramming.org/Static_Exchange_Evaluation>.

	* Returns the net /rankValue/-difference in the /piece/s lost by either side, after a continuous battle at the specified coordinates.

	* CAVEAT: assumes that the battle continues until either player concludes it's disadvantageous to continue, or fire-power has been exhausted.
-}
staticExchangeEvaluation :: (
	Eq	x,
	Eq	y,
	Num	rankValue,
	Ord	rankValue
 )
	=> Attribute.Rank.EvaluateRank rankValue
	-> BareGameTree x y
	-> rankValue
staticExchangeEvaluation evaluateRank node@Data.Tree.Node { Data.Tree.rootLabel = game }	= Data.Maybe.maybe 0 {-nothing taken-} (slave node) $ getMaybeImplicitlyTakenRank game where	-- Find the rank of any victim.
	getMaybeImplicitlyTakenRank game'	= Attribute.MoveType.getMaybeImplicitlyTakenRank . Component.QualifiedMove.getMoveType . Component.Turn.getQualifiedMove =<< Model.Game.maybeLastTurn game'

	slave node'@Data.Tree.Node { Data.Tree.subForest = forest' }	= max 0 {-this player shouldn't progress the battle-} . subtract (
		case filter (
			(
				== Component.Move.getDestination (getLastMove node')
			) . Component.Move.getDestination . getLastMove	-- Find counter-attacks at the same coordinates.
		 ) forest' of
			[]		-> 0	-- Fire-power has been exhausted => terminate recursion.
			forest''	-> let
				node''@Data.Tree.Node { Data.Tree.rootLabel = game'' }	= Data.List.minimumBy (
					\Data.Tree.Node { Data.Tree.rootLabel = gameL } Data.Tree.Node { Data.Tree.rootLabel = gameR } -> uncurry (
						Component.Turn.compareByLVA evaluateRank
					) . (
						($ gameL) &&& ($ gameR)
					) $ Data.Maybe.fromMaybe (
						Control.Exception.throw $ Data.Exception.mkResultUndefined "BishBosh.Model.GameTree:\tModel.Game.maybeLastTurn failed."
					) . Model.Game.maybeLastTurn
				 ) forest'' -- Select the least valuable aggressor.
			 in slave node'' . Data.Maybe.fromJust $ getMaybeImplicitlyTakenRank game''	-- Recurse.
	 ) . evaluateRank {-of victim-}

-- | Accessor.
getRankAndMove :: Model.MoveFrequency.GetRankAndMove (BareGameTree x y) (Component.Move.Move x y)
{-# INLINE getRankAndMove #-}
getRankAndMove Data.Tree.Node { Data.Tree.rootLabel = game }	= (Component.Turn.getRank &&& Component.QualifiedMove.getMove . Component.Turn.getQualifiedMove) . Data.Maybe.fromJust $ Model.Game.maybeLastTurn game

-- | Wrap a 'BareGameTree'.
newtype GameTree x y	= MkGameTree {
	deconstruct	:: BareGameTree x y
} deriving Show {-CAVEAT: required by QuickCheck, but shouldn't actually be called-}

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Show	x,
	Show	y
 ) => Data.Default.Default (GameTree x y) where
	{-# SPECIALISE instance Data.Default.Default (GameTree T.X T.Y) #-}
	def	= fromGame Data.Default.def

instance Property.Tree.Prunable (GameTree x y) where
	prune depth MkGameTree { deconstruct = bareGameTree }	= MkGameTree $ Property.Tree.prune depth bareGameTree

instance (Enum x, Enum y) => Notation.MoveNotation.ShowNotation (GameTree x y) where
	showsNotation moveNotation MkGameTree {
		deconstruct	= bareGameTree@Data.Tree.Node {
			Data.Tree.rootLabel	= game,
			Data.Tree.subForest	= forest
		}
	} = showString $ if Property.Null.isNull game
		then Data.RoseTree.drawForest toString forest
		else Data.RoseTree.drawTree toString bareGameTree
		where
			toString	= Notation.MoveNotation.showNotation moveNotation . Data.Maybe.fromJust . Model.Game.maybeLastTurn

-- | Constructor.
fromBareGameTree :: BareGameTree x y -> GameTree x y
fromBareGameTree	= MkGameTree

-- | Constructs a game-tree with the specified game at its root.
fromGame :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Show	x,
	Show	y
 ) => Model.Game.Game x y -> GameTree x y
{-# SPECIALISE fromGame :: Model.Game.Game T.X T.Y -> GameTree T.X T.Y #-}
fromGame	= MkGameTree . Data.Tree.unfoldTree (
	\game -> (
		game,
		if Model.Game.isTerminated game
			then []
			else map (
				`Model.Game.applyQualifiedMove` game
			) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
	) -- Pair.
 )

{- |
	* Counts the number of /game-state/s in the constant game of chess, at the specified depth, including any which terminated earlier.

	* N.B.: some of the /game-state/s may have identical positions, reached by different sequences of /move/s.
-}
countGames :: Property.Tree.Depth -> Model.Game.NGames
countGames depth	= Data.RoseTree.countTerminalNodes . deconstruct $ Property.Tree.prune depth (Data.Default.def :: GameTree T.X T.Y)

-- | Counts the number of possible plies in chess, down to the specified depth.
countMoves :: Property.Tree.Depth -> Model.Game.NGames
countMoves depth	= pred {-the apex is constructed without moving-} . Data.Foldable.length . deconstruct $ Property.Tree.prune depth (Data.Default.def :: GameTree T.X T.Y)

-- | Trace the route down the tree which matches the specified list of turns.
traceRoute
	:: (Eq x, Eq y)
	=> GameTree x y
	-> [Component.Turn.Turn x y]	-- ^ The data against which, nodes from the tree should be matched.
	-> Maybe [Model.Game.Game x y]	-- ^ Returns 'Nothing' on match-failure.
traceRoute MkGameTree { deconstruct = bareGameTree }	= Data.RoseTree.traceRoute (\turn -> (== Just turn) . Model.Game.maybeLastTurn) bareGameTree

-- | Focus the underlying type.
type MoveFrequency x y	= Model.MoveFrequency.MoveFrequency (Component.Move.Move x y)

-- | Self-documentation.
type Transformation x y	= GameTree x y -> GameTree x y

{- |
	* Independently sorts the forest of moves at each node of the tree, without regard to runtime-data.

	* Depending on preferences, the list of moves available to each game is sequentially sorted by:
		those which reduce the radius from the centre of the board.
		either those which capture a valuable piece using a cheap piece, or those which win extended battles at a specific location.

	* The above sort-algorithms are stable & can therefore be applied independently.
-}
sortGameTree :: (
	Integral	x,
	Integral	y,
	Num		rankValue,
	Ord		rankValue
 )
	=> Bool	-- ^ preferMovesTowardsCentre.
	-> Maybe Attribute.CaptureMoveSortAlgorithm.CaptureMoveSortAlgorithm
	-> Attribute.Rank.EvaluateRank rankValue
	-> MoveFrequency x y
	-> Transformation x y
{-# SPECIALISE sortGameTree :: Bool -> Maybe Attribute.CaptureMoveSortAlgorithm.CaptureMoveSortAlgorithm -> Attribute.Rank.EvaluateRank T.RankValue -> MoveFrequency T.X T.Y -> Transformation T.X T.Y #-}
sortGameTree preferMovesTowardsCentre maybeCaptureMoveSortAlgorithm evaluateRank standardOpeningMoveFrequency MkGameTree { deconstruct = bareGameTree }	= MkGameTree $ Data.RoseTree.mapForest (
	\game -> Data.Maybe.maybe id (
		\captureMoveSortAlgorithm -> case captureMoveSortAlgorithm of
			Attribute.CaptureMoveSortAlgorithm.MVVLVA	-> Data.List.sortBy $ compareByMVVLVA evaluateRank
			Attribute.CaptureMoveSortAlgorithm.SEE		-> Data.List.sortOn $ negate {-largest first-} . staticExchangeEvaluation evaluateRank
	 ) maybeCaptureMoveSortAlgorithm . (
		if Property.Null.isNull standardOpeningMoveFrequency
			then id
			else Model.MoveFrequency.sortByDescendingMoveFrequency (Model.Game.getNextLogicalColour game) getRankAndMove standardOpeningMoveFrequency
	 ) . (
		if preferMovesTowardsCentre
			then Data.List.sortOn $ \node -> Component.Move.getDeltaRadiusSquared $ getLastMove node	:: Double
			else id
	 )
 ) bareGameTree

{- |
	* Count the instances of each /move/ in the specified tree, including any pre-applied to the apex game.

	* CAVEAT: ambiguity remains regarding the /move-type/ (especially any piece taken).

	* CAVEAT: a node is counted as just one instance of the move, rather than the number of games which passed through that node.
	Had the move-frequency been derived from a list of games, a different distribution would result,
	but then early moves would appear popular rather than just the consequence of limited choice.
-}
toMoveFrequency :: (Ord x, Ord y) => GameTree x y -> MoveFrequency x y
toMoveFrequency MkGameTree {
	deconstruct	= bareGameTree@Data.Tree.Node { Data.Tree.rootLabel = rootGame }
} = slave (
	Data.List.foldl' (
		\moveFrequency logicalColour -> Model.MoveFrequency.insertMoves logicalColour (
			Component.Turn.getRank &&& Component.QualifiedMove.getMove . Component.Turn.getQualifiedMove
		) moveFrequency . State.TurnsByLogicalColour.dereference logicalColour $ Model.Game.getTurnsByLogicalColour rootGame
	) Property.Empty.empty {-MoveFrequency-} Attribute.LogicalColour.range
 ) bareGameTree where
	slave moveFrequency Data.Tree.Node {
		Data.Tree.rootLabel	= game,
		Data.Tree.subForest	= forest
	} = Data.List.foldl' slave {-recurse-} (
		Model.MoveFrequency.insertMoves (Model.Game.getNextLogicalColour game) getRankAndMove moveFrequency forest
	 ) forest

