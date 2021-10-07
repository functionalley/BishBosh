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

	* The instances of various moves, categorised by /logical colour/ & /rank/, are recorded from a large resource of games.

	* The frequency-distribution can then be used to sort the moves in the current game, to prioritise evaluation of likely candidates.
-}

module BishBosh.Model.MoveFrequency(
-- * Types
-- ** Type-synonyms
--	InstancesByMoveByRankByLogicalColour,
	GetRankAndMove,
-- ** Data-types
	MoveFrequency(),
-- * Functions
	countEntries,
--	countDistinctEntries,
	insertMoves,
	sortByDescendingMoveFrequency
) where

import			Control.Arrow((&&&))
import			Data.Array.IArray((!), (//))
import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Property.Empty			as Property.Empty
import qualified	BishBosh.Property.Null			as Property.Null
import qualified	BishBosh.Type.Count			as Type.Count
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.List.Extra
import qualified	Data.Map.Strict				as Map
import qualified	Data.Ord

{- |
	* Records the number of instances, indexed by /move/, by /rank/, by /logical colour/.

	* CAVEAT: the /move-type/ isn't recorded.
-}
type InstancesByMoveByRankByLogicalColour move	= Attribute.LogicalColour.ArrayByLogicalColour (
	Attribute.Rank.ArrayByRank (
		Map.Map move Type.Count.NPlies
	)
 )

-- | The number of recorded instances of each move.
newtype MoveFrequency move	= MkMoveFrequency {
	deconstruct	:: InstancesByMoveByRankByLogicalColour move
} deriving Eq

instance Property.Empty.Empty (MoveFrequency move) where
	empty	= MkMoveFrequency . Attribute.LogicalColour.listArrayByLogicalColour . repeat . Attribute.Rank.listArrayByRank $ repeat Property.Empty.empty

instance Property.Null.Null (MoveFrequency move) where
	isNull MkMoveFrequency { deconstruct = instancesByMoveByRankByLogicalColour }	= Data.Foldable.all (Data.Foldable.all Data.Foldable.null) instancesByMoveByRankByLogicalColour

-- | Count the total number of entries.
countEntries :: MoveFrequency move -> Type.Count.NPlies
countEntries MkMoveFrequency { deconstruct = instancesByMoveByRankByLogicalColour }	= Data.Foldable.foldl' (
	Data.Foldable.foldl' $ \acc -> (acc +) . Data.Foldable.sum
 ) 0 instancesByMoveByRankByLogicalColour

-- | Count the total number of distinct entries.
countDistinctEntries :: MoveFrequency move -> Type.Count.NPlies
countDistinctEntries MkMoveFrequency { deconstruct = instancesByMoveByRankByLogicalColour }	= fromIntegral $ Data.Foldable.foldl' (
	Data.Foldable.foldl' $ \acc -> (acc +) . Data.Foldable.length
 ) 0 instancesByMoveByRankByLogicalColour

-- | The type of a function which can extract the /rank/ & /move/ from a datum.
type GetRankAndMove a move	= a -> (Attribute.Rank.Rank, move)

{- |
	* Inserts a list of data from which /rank/ & /move/ can be extracted, each of which were made by pieces of the same /logical colour/, i.e. by the same player.

	* If the entry already exists, then the count for that /rank/ & /move/, is increased.
-}
insertMoves
	:: Ord move
	=> Attribute.LogicalColour.LogicalColour	-- ^ References the player who is required to make any one of the specified moves.
	-> GetRankAndMove a move			-- ^ How to extract the required /rank/ & /move/ from a datum.
	-> MoveFrequency move
	-> [a]						-- ^ The data from each of which, /rank/ & /move/ can be extracted.
	-> MoveFrequency move
insertMoves logicalColour getRankAndMove MkMoveFrequency { deconstruct = instancesByMoveByRankByLogicalColour }	= MkMoveFrequency . (
	instancesByMoveByRankByLogicalColour //
 ) . return {-to List-monad-} . (,) logicalColour . (
	instancesByMoveByRank //
 ) . \case
	[datum]	-> let
		(rank, move)	= getRankAndMove datum
	 in [id &&& incrementMoveCount move . (instancesByMoveByRank !) $ rank]	-- Singleton.
	l	-> [
		(
			rank,
			foldr (
				incrementMoveCount . snd {-move-}
			) (
				instancesByMoveByRank ! rank
			) assocs
--		) | assocs@((rank, _) : _) <- Data.List.Extra.groupSortOn fst {-rank-} $ map getRankAndMove l	-- CAVEAT: wastes space.
		) | assocs@((rank, _) : _) <- Data.List.Extra.groupSortBy (Data.Ord.comparing fst {-rank-}) $ map getRankAndMove l
	 ] -- List-comprehension.
	where
		instancesByMoveByRank	= instancesByMoveByRankByLogicalColour ! logicalColour
		incrementMoveCount	= flip (Map.insertWith (+)) 1

{- |
	* Sorts an arbitrary list on the recorded frequency of the /rank/ & /move/ accessible from each list-item.

	* The /rank/ & /move/ extracted from each list-item, is assumed to have been made by the player of the specified /logical colour/.
-}
sortByDescendingMoveFrequency
	:: Ord move
	=> Attribute.LogicalColour.LogicalColour	-- ^ References the player who is required to make any one of the specified moves.
	-> GetRankAndMove a move			-- ^ How to extract the required /rank/ & /move/ from a datum.
	-> MoveFrequency move
	-> [a]						-- ^ The data from each of which, /rank/ & /move/ can be extracted.
	-> [a]
{-# INLINE sortByDescendingMoveFrequency #-}
sortByDescendingMoveFrequency logicalColour getRankAndMove MkMoveFrequency { deconstruct = instancesByMoveByRankByLogicalColour }	= Data.List.sortOn $ negate {-most frequent first-} . (
	\(rank, move) -> Map.findWithDefault 0 move $ instancesByMoveByRankByLogicalColour ! logicalColour ! rank
 ) . getRankAndMove

