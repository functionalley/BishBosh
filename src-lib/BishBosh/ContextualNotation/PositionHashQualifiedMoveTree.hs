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

 [@DESCRIPTION@]	Facilitates matching of the current /position/ with a tree built from standard openings.
-}

module BishBosh.ContextualNotation.PositionHashQualifiedMoveTree(
-- * Types
-- ** Type-synonyms
--	Tree,
	OnymousQualifiedMove,
--	FindMatch,
-- ** Data-types
	NodeLabel(),
	PositionHashQualifiedMoveTree(),
-- * Functions
--	onymiseQualifiedMove,
--	findNextOnymousQualifiedMovesForGame,
	findNextOnymousQualifiedMovesForPosition,
--	findNextJoiningOnymousQualifiedMovesFromPosition,
	findNextOnymousQualifiedMoves,
	maybeRandomlySelectOnymousQualifiedMove,
-- ** Constructors
	fromQualifiedMoveForest,
-- ** Predicates
--	cantConverge,
	isTerminal
 ) where

import			Control.Arrow((&&&), (***))
import qualified	BishBosh.Attribute.MoveType			as Attribute.MoveType
import qualified	BishBosh.Component.Piece			as Component.Piece
import qualified	BishBosh.Component.QualifiedMove		as Component.QualifiedMove
import qualified	BishBosh.Component.Turn				as Component.Turn
import qualified	BishBosh.Component.Zobrist			as Component.Zobrist
import qualified	BishBosh.ContextualNotation.QualifiedMoveForest	as ContextualNotation.QualifiedMoveForest
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.Model.Result				as Model.Result
import qualified	BishBosh.Property.Reflectable			as Property.Reflectable
import qualified	BishBosh.State.Board				as State.Board
import qualified	BishBosh.Types					as T
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Parallel.Strategies
import qualified	Data.Array.IArray
import qualified	Data.Bits
import qualified	Data.Default
import qualified	Data.List
import qualified	Data.List.Extra
import qualified	Data.Maybe
import qualified	Data.Tree
import qualified	Factory.Math.Statistics
import qualified	System.Random
import qualified	ToolShed.System.Random

-- | Each label of the tree contains a Zobrist-hash of the current position, augmented (except in the case of the apex-game) by the last move that was played & any conclusive result.
data NodeLabel x y positionHash	= MkNodeLabel {
	getPositionHash				:: positionHash,
	getMaybeQualifiedMoveWithOnymousResult	:: Maybe (Component.QualifiedMove.QualifiedMove x y, Maybe ContextualNotation.QualifiedMoveForest.OnymousResult)
}

-- | The tree of /qualified move/s.
type Tree x y positionHash	= Data.Tree.Tree (NodeLabel x y positionHash)

-- | Constructor.
data PositionHashQualifiedMoveTree x y positionHash	= MkPositionHashQualifiedMoveTree {
	getZobrist		:: Component.Zobrist.Zobrist x y positionHash,	-- ^ Used to hash each position in the tree.
	getTree			:: Tree x y positionHash,
	getMinimumPieces	:: Component.Piece.NPieces			-- ^ The minimum number of pieces remaining after the last move in any game defined in the tree.
}

-- | Augment the specified /qualified-move forest/ with a zobrist-hash of the /position/ & include the default initial game at the apex.
fromQualifiedMoveForest :: (
	Data.Array.IArray.Ix	x,
	Data.Bits.Bits		positionHash,
	Enum			x,
	Enum			y,
	Ord			y,
	Show			x,
	Show			y
 )
	=> Bool	-- ^ IncrementalEvaluation.
	-> Component.Zobrist.Zobrist x y positionHash
	-> ContextualNotation.QualifiedMoveForest.QualifiedMoveForest x y
	-> PositionHashQualifiedMoveTree x y positionHash
{-# SPECIALISE fromQualifiedMoveForest :: Bool -> Component.Zobrist.Zobrist T.X T.Y T.PositionHash -> ContextualNotation.QualifiedMoveForest.QualifiedMoveForest T.X T.Y -> PositionHashQualifiedMoveTree T.X T.Y T.PositionHash #-}
fromQualifiedMoveForest incrementalEvaluation zobrist qualifiedMoveForest	= MkPositionHashQualifiedMoveTree {
	getZobrist		= zobrist,
	getTree			= let
		initialGame		= Data.Default.def
		initialPositionHash	= Component.Zobrist.hash2D initialGame zobrist
	in Data.Tree.Node {
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
						positionHash'	= Model.Game.incrementalHash game positionHash game' zobrist
				in slave initialGame initialPositionHash
				else let
					slave game Data.Tree.Node {
						Data.Tree.rootLabel	= label@(qualifiedMove, _),
						Data.Tree.subForest	= qualifiedMoveForest'
					} = Data.Tree.Node {
						Data.Tree.rootLabel	= MkNodeLabel (Component.Zobrist.hash2D game' zobrist) $ Just label,	-- Hash the game after applying the move.
						Data.Tree.subForest	= map (slave game') qualifiedMoveForest'	-- Recurse.
					} where
						game'	= Model.Game.applyQualifiedMove qualifiedMove game
				in slave initialGame
		) $ ContextualNotation.QualifiedMoveForest.deconstruct qualifiedMoveForest
	},
	getMinimumPieces	= ContextualNotation.QualifiedMoveForest.findMinimumPieces qualifiedMoveForest
}

-- | Predicate.
isTerminal :: PositionHashQualifiedMoveTree x y positionHash -> Bool
isTerminal MkPositionHashQualifiedMoveTree { getTree = Data.Tree.Node { Data.Tree.subForest = [] } }	= True
isTerminal _												= False

{- |
	* Determines whether, based on the current number of pieces, the specified game can't migrate to any /position/ defined in the tree.

	* CAVEAT: a negative result doesn't imply that convergence is possible, since other factors may prevent it.
-}
cantConverge :: Model.Game.Game x y -> PositionHashQualifiedMoveTree x y positionHash -> Bool
cantConverge game MkPositionHashQualifiedMoveTree { getMinimumPieces = minimumPieces }	= State.Board.getNPieces (Model.Game.getBoard game) < minimumPieces

-- | A /qualified move/ annotated by the name & ultimate /result/, of each /game/ from which it could have originated.
type OnymousQualifiedMove x y	= (Component.QualifiedMove.QualifiedMove x y, [ContextualNotation.QualifiedMoveForest.OnymousResult])

-- | Find the /onymous result/s for all /game/s originating from the specified tree.
onymiseQualifiedMove :: Tree x y positionHash -> OnymousQualifiedMove x y
onymiseQualifiedMove	= (
	fst {-qualifiedMove-} . head &&& Data.Maybe.mapMaybe snd {-Maybe OnymousResult-}
 ) . map (
	\MkNodeLabel { getMaybeQualifiedMoveWithOnymousResult = Just qualifiedMoveWithOnymousResult } -> qualifiedMoveWithOnymousResult
 ) . Data.Tree.flatten

-- | The type of a function used to locate a match in the tree.
type FindMatch x y positionHash	= Model.Game.Game x y -> PositionHashQualifiedMoveTree x y positionHash -> [OnymousQualifiedMove x y]

-- | For any exactly matching /game/ in the tree, return the subsequent /qualifiedMove/s.
findNextOnymousQualifiedMovesForGame :: (Eq x, Eq y) => FindMatch x y positionHash
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

	* CAVEAT: a null list can result from either match-failure, or a match with the final /move/ of a /game/.
-}
findNextOnymousQualifiedMovesForPosition :: (
	Data.Array.IArray.Ix	x,
	Data.Bits.Bits		positionHash,
	Enum			x,
	Enum			y,
	Ord			y
 ) => FindMatch x y positionHash
{-# SPECIALISE findNextOnymousQualifiedMovesForPosition :: FindMatch T.X T.Y T.PositionHash #-}
findNextOnymousQualifiedMovesForPosition requiredGame positionHashQualifiedMoveTree@MkPositionHashQualifiedMoveTree {
	getZobrist	= zobrist,
	getTree		= tree
}
	| cantConverge requiredGame positionHashQualifiedMoveTree	= []	-- The specified game has fewer pieces than any defined in the tree.
	| otherwise							= slave (2 * Component.Piece.nPiecesPerSide) tree
	where
		slave nPieces Data.Tree.Node {
			Data.Tree.rootLabel	= MkNodeLabel { getPositionHash = positionHash },
			Data.Tree.subForest	= forest
		}
			| nPieces < State.Board.getNPieces (
				Model.Game.getBoard requiredGame
			)		= []	-- There are fewer pieces remaining in the tree than required.
			| otherwise	= (
				if positionHash == Component.Zobrist.hash2D requiredGame zobrist
					then (
						map onymiseQualifiedMove forest ++	-- The Zobrist-hash/position matches, so one can select any move from the forest.
					) -- Section.
					else id
			) $ concatMap (
				\node@Data.Tree.Node {
					Data.Tree.rootLabel	= MkNodeLabel { getMaybeQualifiedMoveWithOnymousResult = Just (qualifiedMove, _) }
				} -> slave (
					Attribute.MoveType.nPiecesMutator (Component.QualifiedMove.getMoveType qualifiedMove) nPieces
				) node -- Recurse to see if deeper matches exist.
			) forest

-- | Finds any single /move/s which can join the current /position/ with a member of the forest.
findNextJoiningOnymousQualifiedMovesFromPosition :: (
	Data.Array.IArray.Ix	x,
	Data.Bits.Bits		positionHash,
	Control.DeepSeq.NFData	x,
	Control.DeepSeq.NFData	y,
	Enum			x,
	Enum			y,
	Ord			y,
	Show			x,
	Show			y
 ) => FindMatch x y positionHash
{-# SPECIALISE findNextJoiningOnymousQualifiedMovesFromPosition :: FindMatch T.X T.Y T.PositionHash #-}
findNextJoiningOnymousQualifiedMovesFromPosition game positionHashQualifiedMoveTree	= [
	(
		preMatchQualifiedMove,
		concatMap snd {-[OnymousResult]-} matchingOnymousQualifiedMoves	-- Discard the opponent's matching move, but cite the names of archived games it reached.
	) |
		not $ Model.Game.isTerminated game,
		(preMatchQualifiedMove, matchingOnymousQualifiedMoves)	<- Control.Parallel.Strategies.withStrategy (
			Control.Parallel.Strategies.parList $ Control.Parallel.Strategies.parTuple2 Control.Parallel.Strategies.r0 Control.Parallel.Strategies.rdeepseq
		) . map (
			id &&& (`findNextOnymousQualifiedMovesForPosition` positionHashQualifiedMoveTree) . (`Model.Game.applyQualifiedMove` game)	-- Apply this player's move.
		) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game,
		not $ null matchingOnymousQualifiedMoves
 ] -- List-comprehension.

{- |
	* Calls 'findNextOnymousQualifiedMovesForGame' to find an exact match for the current /game/ in the tree.

	* Calls 'findNextOnymousQualifiedMovesForPosition' to find a match for the current /position/ in the tree.

	* On failure, it searches the tree to find a match for the colour-flipped /position/.

	* On failure, it searches for any /move/ which can be used to join the /position/ with the tree.

	* On failure, it searches for any /move/ which can be used to join the colour-flipped /position/ with the tree.

	* CAVEAT: the order of these searches has been hard-coded.
-}
findNextOnymousQualifiedMoves :: (
	Data.Array.IArray.Ix	x,
	Data.Bits.Bits		positionHash,
	Control.DeepSeq.NFData	x,
	Control.DeepSeq.NFData	y,
	Enum			x,
	Enum			y,
	Ord			y,
	Show			x,
	Show			y
 )
	=> (Bool, Bool, Bool)	-- ^ MatchSwitches.
	-> FindMatch x y positionHash
{-# SPECIALISE findNextOnymousQualifiedMoves :: (Bool, Bool, Bool) -> FindMatch T.X T.Y T.PositionHash #-}
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

-- | Randomly select a /qualifiedMove/ from matching /position/s in the tree, & supply the names of those archived games from which it originated.
maybeRandomlySelectOnymousQualifiedMove :: (
	Control.DeepSeq.NFData	x,
	Control.DeepSeq.NFData	y,
	Data.Array.IArray.Ix	x,
	Data.Bits.Bits		positionHash,
	Enum			x,
	Enum			y,
	Ord			y,
	Show			x,
	Show			y,
	System.Random.RandomGen	randomGen
 )
	=> randomGen
	-> (Bool, Bool, Bool)	-- ^ MatchSwitches.
	-> Model.Game.Game x y
	-> PositionHashQualifiedMoveTree x y positionHash
	-> Maybe (Component.QualifiedMove.QualifiedMove x y, [ContextualNotation.QualifiedMoveForest.Name])
{-# SPECIALISE maybeRandomlySelectOnymousQualifiedMove
	:: System.Random.RandomGen randomGen
	=> randomGen
	-> (Bool, Bool, Bool)
	-> Model.Game.Game T.X T.Y
	-> PositionHashQualifiedMoveTree T.X T.Y T.PositionHash
	-> Maybe (Component.QualifiedMove.QualifiedMove T.X T.Y, [ContextualNotation.QualifiedMoveForest.Name])
 #-}
maybeRandomlySelectOnymousQualifiedMove randomGen matchSwitches game positionHashQualifiedMoveTree	= case findNextOnymousQualifiedMoves matchSwitches game positionHashQualifiedMoveTree of
	[]			-> Nothing
	onymousQualifiedMoves	-> fmap (
		Control.Arrow.second $ Data.List.nub . map fst {-Name-}
	 ) . ToolShed.System.Random.select randomGen . last {-highest scoring group-} $ Data.List.Extra.groupSortOn (
		(
			Factory.Math.Statistics.getMean	:: [Int] -> Rational
		) . map (
			 Data.Maybe.maybe 0 {-a draw-} (
				\victorsLogicalColour -> (
					if victorsLogicalColour == Model.Game.getNextLogicalColour game
						then id
						else negate
				) 1 {-victory-}
			) . Model.Result.findMaybeVictor . snd {-result-}	-- Score the result, according to which side we'd like to win.
		) . snd {-[OnymousResult]-}
	 ) onymousQualifiedMoves

