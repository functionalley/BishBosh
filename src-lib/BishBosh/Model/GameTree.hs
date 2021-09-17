{-# LANGUAGE LambdaCase #-}
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
	countPositions,
	traceRoute,
	sortGameTree,
	toMoveFrequency,
-- ** Constructor
	fromBareGameTree,
	fromGame
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.CaptureMoveSortAlgorithm	as Attribute.CaptureMoveSortAlgorithm
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
import qualified	BishBosh.Property.Arboreal			as Property.Arboreal
import qualified	BishBosh.Property.Empty				as Property.Empty
import qualified	BishBosh.Property.Null				as Property.Null
import qualified	BishBosh.Property.Opposable			as Property.Opposable
import qualified	BishBosh.Type.Count				as Type.Count
import qualified	BishBosh.Type.Length				as Type.Length
import qualified	BishBosh.Type.Mass				as Type.Mass
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
	{-# SPECIALISE instance Data.Default.Default (GameTree Type.Length.X Type.Length.Y) #-}
	def	= fromGame Data.Default.def

instance Property.Arboreal.Prunable (GameTree x y) where
	prune depth MkGameTree { deconstruct = bareGameTree }	= MkGameTree $ Property.Arboreal.prune depth bareGameTree

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
{-# SPECIALISE fromGame :: Model.Game.Game Type.Length.X Type.Length.Y -> GameTree Type.Length.X Type.Length.Y #-}
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
countGames :: Property.Arboreal.Depth -> Type.Count.NGames
countGames depth	= Data.RoseTree.countTerminalNodes . deconstruct $ Property.Arboreal.prune depth (Data.Default.def :: GameTree Type.Length.X Type.Length.Y)

-- | Counts the number of possible positions in chess, down to the specified depth. N.B.: some of these may be transpositions.
countPositions :: Property.Arboreal.Depth -> Type.Count.NPositions
countPositions depth	= fromIntegral . pred {-the apex is constructed without moving-} . Data.Foldable.length . deconstruct $ Property.Arboreal.prune depth (Data.Default.def :: GameTree Type.Length.X Type.Length.Y)

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

	* Depending on preferences, the list of moves available from each position is sorted by; either those which capture a valuable piece using a cheap piece, or those which win extended battles at a specific location.

	* The above sort-algorithms are stable & can therefore be applied independently.
-}
sortGameTree :: (
	Integral	x,
	Integral	y,
	Num		rankValue,
	Ord		rankValue
 )
	=> Maybe Attribute.CaptureMoveSortAlgorithm.CaptureMoveSortAlgorithm
	-> Attribute.Rank.EvaluateRank rankValue
	-> MoveFrequency x y
	-> Transformation x y
{-# SPECIALISE sortGameTree :: Maybe Attribute.CaptureMoveSortAlgorithm.CaptureMoveSortAlgorithm -> Attribute.Rank.EvaluateRank Type.Mass.RankValue -> MoveFrequency Type.Length.X Type.Length.Y -> Transformation Type.Length.X Type.Length.Y #-}
sortGameTree maybeCaptureMoveSortAlgorithm evaluateRank standardOpeningMoveFrequency MkGameTree { deconstruct = bareGameTree }	= MkGameTree $ Data.RoseTree.mapForest (
	\game -> Data.Maybe.maybe id (
		\case
			Attribute.CaptureMoveSortAlgorithm.MVVLVA	-> Data.List.sortBy $ compareByMVVLVA evaluateRank
			Attribute.CaptureMoveSortAlgorithm.SEE		-> Data.List.sortOn $ negate {-largest first-} . staticExchangeEvaluation evaluateRank
	 ) maybeCaptureMoveSortAlgorithm . (
		if Property.Null.isNull standardOpeningMoveFrequency
			then id
			else Model.MoveFrequency.sortByDescendingMoveFrequency (Model.Game.getNextLogicalColour game) getRankAndMove standardOpeningMoveFrequency
	 )
 ) bareGameTree

{- |
	* Count the instances of each /move/ in the specified tree.

	* CAVEAT: assumes that root game hasn't any pre-applied moves; which might occur in a test-case.

	* CAVEAT: ambiguity remains regarding the /move-type/ (especially any piece taken).

	* CAVEAT: a node is counted as just one instance of the move, rather than the number of games which passed through that node.
	Had the move-frequency been derived from a list of games, a different distribution would result,
	but then early moves would appear popular rather than just the consequence of limited choice.
-}
toMoveFrequency :: (Ord x, Ord y) => GameTree x y -> MoveFrequency x y
toMoveFrequency MkGameTree { deconstruct = bareGameTree } = slave maxBound {-logicalColour-} Property.Empty.empty {-MoveFrequency-} bareGameTree where
	slave _ moveFrequency Data.Tree.Node { Data.Tree.subForest = [] }			= moveFrequency
	slave logicalColour moveFrequency Data.Tree.Node { Data.Tree.subForest = forest }	= Data.List.foldl' (
		slave {-recurse-} $ Property.Opposable.getOpposite logicalColour
	 ) (
		Model.MoveFrequency.insertMoves logicalColour getRankAndMove moveFrequency forest
	 ) forest

