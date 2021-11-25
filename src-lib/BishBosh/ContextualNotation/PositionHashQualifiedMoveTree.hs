{-# LANGUAGE CPP #-}
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

 [@DESCRIPTION@]	Facilitates matching of the current /position/ in a tree built from standard openings.
-}

module BishBosh.ContextualNotation.PositionHashQualifiedMoveTree(
-- * Types
-- ** Type-synonyms
--	Tree,
--	OnymousQualifiedMove,
--	FindMatch,
	TryToMatchMoves,
	TryToMatchViaJoiningMove,
	TryToMatchColourFlippedPosition,
	PreferVictories,
	MatchSwitches,
-- ** Data-types
	NodeLabel(),
	PositionHashQualifiedMoveTree(),
-- * Functions
--	onymiseQualifiedMove,
--	findNextOnymousQualifiedMovesForGame,
	findNextOnymousQualifiedMovesForPosition,
--	findNextJoiningOnymousQualifiedMovesFromPosition,
	findNextOnymousQualifiedMoves,
--	shortListMostVictorious,
	maybeRandomlySelectOnymousQualifiedMove,
-- ** Constructors
	fromQualifiedMoveForest,
-- ** Predicates
--	cantConverge,
	isTerminal
 ) where

import			Control.Arrow((&&&), (***))
import qualified	BishBosh.Attribute.MoveType			as Attribute.MoveType
import qualified	BishBosh.Colour.LogicalColour			as Colour.LogicalColour
import qualified	BishBosh.Component.Piece			as Component.Piece
import qualified	BishBosh.Component.QualifiedMove		as Component.QualifiedMove
import qualified	BishBosh.Component.Turn				as Component.Turn
import qualified	BishBosh.Component.Zobrist			as Component.Zobrist
import qualified	BishBosh.ContextualNotation.QualifiedMoveForest	as ContextualNotation.QualifiedMoveForest
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.Property.Reflectable			as Property.Reflectable
import qualified	BishBosh.Rule.Result				as Rule.Result
import qualified	BishBosh.State.Board				as State.Board
import qualified	BishBosh.StateProperty.Censor			as StateProperty.Censor
import qualified	BishBosh.StateProperty.Hashable			as StateProperty.Hashable
import qualified	BishBosh.Type.Count				as Type.Count
import qualified	BishBosh.Type.Crypto				as Type.Crypto
import qualified	Control.Arrow
import qualified	Control.Exception
import qualified	Data.Bits
import qualified	Data.Default
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.List.Extra
import qualified	Data.Maybe
import qualified	Data.Tree
import qualified	System.Random
import qualified	ToolShed.System.Random

#ifdef USE_PARALLEL
import qualified	Control.Parallel.Strategies
#endif

-- | Each label of the tree contains a /Zobrist-hash/ of the current position, augmented (except in the case of the apex-game) by the last /move/ that was played, & any conclusive result.
data NodeLabel positionHash	= MkNodeLabel {
	getPositionHash				:: positionHash,
	getMaybeQualifiedMoveWithOnymousResult	:: Maybe (Component.QualifiedMove.QualifiedMove, Maybe ContextualNotation.QualifiedMoveForest.OnymousResult)
}

-- | The tree of /qualified move/s.
type Tree positionHash	= Data.Tree.Tree (NodeLabel positionHash)

-- | Constructor.
data PositionHashQualifiedMoveTree positionHash	= MkPositionHashQualifiedMoveTree {
	getZobrist		:: Component.Zobrist.Zobrist positionHash,	-- ^ Used to hash each position in the tree.
	getTree			:: Tree positionHash,
	getMinimumPieces	:: ! Type.Count.NPieces,			-- ^ The minimum number of /piece/s remaining after the last /move/ in any game defined in the tree.
	getHasAnyVictories	:: Bool						-- ^ Whether a victory has been recorded for any game in the tree; which won't be the case if it was constructed from a PGN-database containing standard-openings.
}

-- | Augment the specified /qualified-move forest/ with a /Zobrist-hash/ of the /position/ & include the default initial game at the apex.
fromQualifiedMoveForest
	:: Data.Bits.Bits positionHash
	=> Bool	-- ^ IncrementalEvaluation.
	-> Component.Zobrist.Zobrist positionHash
	-> ContextualNotation.QualifiedMoveForest.QualifiedMoveForest
	-> PositionHashQualifiedMoveTree positionHash
{-# SPECIALISE fromQualifiedMoveForest :: Bool -> Component.Zobrist.Zobrist Type.Crypto.PositionHash -> ContextualNotation.QualifiedMoveForest.QualifiedMoveForest -> PositionHashQualifiedMoveTree Type.Crypto.PositionHash #-}
fromQualifiedMoveForest incrementalEvaluation zobrist qualifiedMoveForest	= MkPositionHashQualifiedMoveTree {
	getZobrist		= zobrist,
	getTree			= tree,
	getMinimumPieces	= ContextualNotation.QualifiedMoveForest.findMinimumPieces qualifiedMoveForest,
	getHasAnyVictories	= Data.Foldable.any (
		Data.Maybe.maybe False {-no QualifiedMove-} (
			Data.Maybe.maybe False {-no OnymousResult-} (
				not . Rule.Result.isDraw . snd {-Result-}
			) . snd {-Maybe OnymousResult-}
		) . getMaybeQualifiedMoveWithOnymousResult
	) tree
} where
	initialGame		= Data.Default.def
	initialPositionHash	= StateProperty.Hashable.hash initialGame zobrist
	tree			= Data.Tree.Node {
		Data.Tree.rootLabel	= MkNodeLabel initialPositionHash Nothing,
		Data.Tree.subForest	= map (
			if incrementalEvaluation
				then let
					slave game positionHash Data.Tree.Node {
						Data.Tree.rootLabel	= label@(qualifiedMove, _),
						Data.Tree.subForest	= qualifiedMoveForest'
					} = Data.Tree.Node {
						Data.Tree.rootLabel	= MkNodeLabel positionHash' $ Just label,
						Data.Tree.subForest	= map (slave game' positionHash') qualifiedMoveForest'	-- Recurse.
					} where
						game'		= Model.Game.applyQualifiedMove qualifiedMove game
						positionHash'	= Model.Game.updateIncrementalPositionHash game positionHash game' zobrist
				in slave initialGame initialPositionHash
				else let
					slave game Data.Tree.Node {
						Data.Tree.rootLabel	= label@(qualifiedMove, _),
						Data.Tree.subForest	= qualifiedMoveForest'
					} = Data.Tree.Node {
						Data.Tree.rootLabel	= MkNodeLabel (StateProperty.Hashable.hash game' zobrist) $ Just label,	-- Hash the game after applying the move.
						Data.Tree.subForest	= map (slave game') qualifiedMoveForest'	-- Recurse.
					} where
						game'	= Model.Game.applyQualifiedMove qualifiedMove game
				in slave initialGame
		) $ ContextualNotation.QualifiedMoveForest.deconstruct qualifiedMoveForest
	}

-- | Predicate.
isTerminal :: PositionHashQualifiedMoveTree positionHash -> Bool
isTerminal MkPositionHashQualifiedMoveTree { getTree = Data.Tree.Node { Data.Tree.subForest = [] } }	= True
isTerminal _												= False

{- |
	* Determines whether, based on the current number of pieces, the specified game can't migrate to any /position/ defined in the tree.

	* CAVEAT: a negative result doesn't imply that convergence is possible, since other factors may prevent it.
-}
cantConverge :: Model.Game.Game -> PositionHashQualifiedMoveTree positionHash -> Bool
cantConverge game MkPositionHashQualifiedMoveTree { getMinimumPieces = minimumPieces }	= State.Board.getNPieces (Model.Game.getBoard game) < minimumPieces

-- | A /qualified move/ annotated by the name & ultimate /result/, of each /game/ from which it could have originated.
type OnymousQualifiedMove	= (Component.QualifiedMove.QualifiedMove, [ContextualNotation.QualifiedMoveForest.OnymousResult])

-- | Find the /onymous result/s for all /game/s originating from the specified tree.
onymiseQualifiedMove :: Tree positionHash -> OnymousQualifiedMove
onymiseQualifiedMove	= (
	fst {-qualifiedMove-} . head &&& Data.Maybe.mapMaybe snd {-Maybe OnymousResult-}
 ) . (
	\l -> Control.Exception.assert (not $ null l) l
 ) . map (
	\MkNodeLabel { getMaybeQualifiedMoveWithOnymousResult = Just qualifiedMoveWithOnymousResult } -> qualifiedMoveWithOnymousResult
 ) . Data.Tree.flatten

-- | The type of a function used to locate a match in the tree.
type FindMatch positionHash	= Model.Game.Game -> PositionHashQualifiedMoveTree positionHash -> [OnymousQualifiedMove]

-- | For any exactly matching /game/ in the tree, return the subsequent /qualifiedMove/s.
findNextOnymousQualifiedMovesForGame :: FindMatch positionHash
findNextOnymousQualifiedMovesForGame requiredGame	= slave (
	Model.Game.listTurnsChronologically requiredGame
 ) . Data.Tree.subForest {-remove the apex which lacks a founding move-} . getTree where
	slave (turn : remainingTurns)	= Data.Maybe.maybe [] {-match-failure-} (
		slave remainingTurns . Data.Tree.subForest	-- Recurse.
	 ) . Data.List.find (
		\Data.Tree.Node {
			Data.Tree.rootLabel	= MkNodeLabel { getMaybeQualifiedMoveWithOnymousResult = Just (qualifiedMove, _) }
		} -> qualifiedMove == Component.Turn.getQualifiedMove turn
	 )
	slave _ {-none left to match-}	= map onymiseQualifiedMove

{- |
	* For all matching /position/s, return the subsequent /qualifiedMove/.

	* By matching the /position/ rather than the precise sequence of /move/s, transpositions <https://www.chessprogramming.org/Transposition> can also be identified.

	* N.B.: a comparison between the number of pieces in the game we're required to match & the decreasing number of pieces down the tree, permits early termination of the search.

	* CAVEAT: a null list can result from either match-failure, or a match with the final /move/ of a /game/.
-}
findNextOnymousQualifiedMovesForPosition :: Data.Bits.Bits positionHash => FindMatch positionHash
{-# SPECIALISE findNextOnymousQualifiedMovesForPosition :: FindMatch Type.Crypto.PositionHash #-}
findNextOnymousQualifiedMovesForPosition requiredGame positionHashQualifiedMoveTree
	| cantConverge requiredGame positionHashQualifiedMoveTree	= []	-- The game we're required to match has fewer pieces than any defined in the tree.
	| otherwise							= slave (
		(
			uncurry (***) . (id &&& id) $ (Component.Piece.nPiecesPerSide -)	-- Find the number of pieces at the apex of the tree, in excess of the requiredGame, to be taken before a match can be found.
		) . StateProperty.Censor.countPiecesByLogicalColour . State.Board.getCoordinatesByRankByLogicalColour . Model.Game.getBoard $ requiredGame
	) $ getTree positionHashQualifiedMoveTree
	where
		slave (nPiecesDiffOpponent, nPiecesDiffMover) Data.Tree.Node {
			Data.Tree.rootLabel	= MkNodeLabel { getPositionHash = positionHash },
			Data.Tree.subForest	= forest
		} = (
			case nPiecesDiffMover `compare` 0 of	-- N.B. equivalent to 'signum' to slightly better performance.
				GT	-> id		-- This node can't match, but there may be a match further down the tree.
				EQ
					| positionHash == StateProperty.Hashable.hash requiredGame (
						getZobrist positionHashQualifiedMoveTree
					)		-> (map onymiseQualifiedMove forest ++) -- The position matches, so one can select any move from the forest.
					| otherwise	-> id					-- This node doesn't match, but there may be a match further down the tree.
				_	-> const []	-- Terminate the recursion, since from here down the tree, the mover has insufficient pieces to match the required game.
		 ) $ concatMap (
			\node@Data.Tree.Node {
				Data.Tree.rootLabel	= MkNodeLabel { getMaybeQualifiedMoveWithOnymousResult = Just (qualifiedMove, _) }
			} -> slave (
				nPiecesDiffMover,
				(
					if Attribute.MoveType.isCapture $! Component.QualifiedMove.getMoveType qualifiedMove
						then pred
						else id
				) nPiecesDiffOpponent
			) {-swap pair-} node	-- Recurse.
		 ) forest

-- | Finds any single /move/s which can join the current /position/ with a member of the forest.
findNextJoiningOnymousQualifiedMovesFromPosition :: Data.Bits.Bits positionHash => FindMatch positionHash
{-# SPECIALISE findNextJoiningOnymousQualifiedMovesFromPosition :: FindMatch Type.Crypto.PositionHash #-}
findNextJoiningOnymousQualifiedMovesFromPosition game positionHashQualifiedMoveTree
	| Model.Game.isTerminated game	= []
	| otherwise			= [
		Control.Arrow.second (concatMap snd {-[OnymousResult]-}) movePair |	-- Discard the opponent's matching move, but cite the names of archived games it reached.
			movePair@(_, _ : _)	<-
#ifdef USE_PARALLEL
				Control.Parallel.Strategies.withStrategy (
					Control.Parallel.Strategies.parList $ Control.Parallel.Strategies.evalTuple2 Control.Parallel.Strategies.r0 {-pre-match move-} Control.Parallel.Strategies.rdeepseq {-matching moves-}
				) .
#endif
				map (
					id &&& (`findNextOnymousQualifiedMovesForPosition` positionHashQualifiedMoveTree) . (`Model.Game.applyQualifiedMove` game)	-- Apply this player's move.
				) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
	] -- List-comprehension.

-- | Whether to attempt to exactly match moves with a standard opening; transpositions won't be matched.
type TryToMatchMoves	= Bool

-- | Whether to attempt to join the current position to a standard opening that's only one ply away.
type TryToMatchViaJoiningMove	= Bool

-- | Whether to attempt to match a colour-flipped version of the current position with a standard opening
type TryToMatchColourFlippedPosition	= Bool

-- | The switches used to control attempts to find a match amongst standard openings.
type MatchSwitches	= (TryToMatchMoves, TryToMatchViaJoiningMove, TryToMatchColourFlippedPosition)

-- | Whether from all matching positions extracted from the tree, to prefer moves which result in a greater probability of victory, for the player who has the next move.
type PreferVictories	= Bool

{- |
	* Calls 'findNextOnymousQualifiedMovesForGame' to find an exact match for the current /game/ in the tree.

	* Calls 'findNextOnymousQualifiedMovesForPosition' to find a match for the current /position/ in the tree.

	* On failure, it searches the tree to find a match for the colour-flipped /position/.

	* On failure, it searches for any /move/ which can be used to join the /position/ with the tree.

	* On failure, it searches for any /move/ which can be used to join the colour-flipped /position/ with the tree.

	* CAVEAT: the order of these searches has been hard-coded.
-}
findNextOnymousQualifiedMoves
	:: Data.Bits.Bits positionHash
	=> MatchSwitches
	-> FindMatch positionHash
{-# SPECIALISE findNextOnymousQualifiedMoves :: MatchSwitches -> FindMatch Type.Crypto.PositionHash #-}
findNextOnymousQualifiedMoves (tryToMatchMoves, tryToMatchViaJoiningMove, tryToMatchColourFlippedPosition) game positionHashQualifiedMoveTree
	| cantConverge game positionHashQualifiedMoveTree	= []	-- The specified game is smaller than any defined in the tree.
	| otherwise						= Data.Maybe.fromMaybe [] . Data.List.find (
		not . null	-- Accept the results from the first match-function which returns any.
	) $ (
		if tryToMatchMoves
			then (findNextOnymousQualifiedMovesForGame game positionHashQualifiedMoveTree :)
			else id
	) [
		colourFlipper findMatch game positionHashQualifiedMoveTree |
			findMatch	<- findNextOnymousQualifiedMovesForPosition : [findNextJoiningOnymousQualifiedMovesFromPosition | tryToMatchViaJoiningMove] {-list-comprehension-},
			colourFlipper	<- id : [
				\findMatch' game' -> map (
					Property.Reflectable.reflectOnX {-reflect matching moves back into the original domain-} *** map (
						Control.Arrow.first $ showString "Colour-flipped:\t"
					)
				) . findMatch' (
					Property.Reflectable.reflectOnX game'
				) | tryToMatchColourFlippedPosition
			] -- Transform an arbitrary match-function to operate on either the original or the colour-flipped game.
	] -- List-comprehension.

-- | Shortlist matching moves extracted from the tree, prefering those after which the player who makes it, has the greatest recorded incidence of victory.
shortListMostVictorious
	:: Colour.LogicalColour.LogicalColour	-- ^ The player who is next to move.
	-> [OnymousQualifiedMove]
	-> [OnymousQualifiedMove]
shortListMostVictorious nextLogicalColour	= last {-highest scoring group-} . Data.List.Extra.groupSortOn (
	Data.List.foldl' (
		\acc -> ($ acc) . Data.Maybe.maybe id {-draw-} (
			\victorsLogicalColour -> if victorsLogicalColour == nextLogicalColour then succ else pred	-- Score the result, according to which side we'd like to win.
		) . Rule.Result.findMaybeVictor . snd {-result-}
	) (0 :: Int) . snd {-[OnymousResult]-}
 )

-- | Randomly select a /qualifiedMove/ from matching /position/s in the tree, & supply the names of those archived games from which it originated.
maybeRandomlySelectOnymousQualifiedMove :: (
	Data.Bits.Bits		positionHash,
	System.Random.RandomGen	randomGen
 )
	=> randomGen
	-> PreferVictories
	-> MatchSwitches
	-> Model.Game.Game
	-> PositionHashQualifiedMoveTree positionHash
	-> Maybe (Component.QualifiedMove.QualifiedMove, [ContextualNotation.QualifiedMoveForest.Name])
{-# SPECIALISE maybeRandomlySelectOnymousQualifiedMove :: System.Random.StdGen -> PreferVictories -> MatchSwitches -> Model.Game.Game -> PositionHashQualifiedMoveTree Type.Crypto.PositionHash -> Maybe (Component.QualifiedMove.QualifiedMove, [ContextualNotation.QualifiedMoveForest.Name]) #-}
maybeRandomlySelectOnymousQualifiedMove randomGen preferVictories matchSwitches game positionHashQualifiedMoveTree	= case findNextOnymousQualifiedMoves matchSwitches game positionHashQualifiedMoveTree of
	[]			-> Nothing
	onymousQualifiedMoves	-> fmap (
		Control.Arrow.second $ Data.List.nub . map fst {-Name-}
	 ) . ToolShed.System.Random.select randomGen $ (
		if preferVictories && getHasAnyVictories positionHashQualifiedMoveTree
			then shortListMostVictorious $ Model.Game.getNextLogicalColour game
			else id
	 ) onymousQualifiedMoves

