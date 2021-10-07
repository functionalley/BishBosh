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

 [@DESCRIPTION@] <https://www.chessprogramming.org/Killer_Heuristic>.
-}

module BishBosh.Search.KillerMoves (
-- * Types
-- ** Type-synonyms
--	NInstancesByNPliesByKeyByLogicalColour,
	Transformation,
-- ** Data-types
	KillerMoves(),
-- * Functions
	sortByHistoryHeuristic,
-- ** Mutators
	insert
 ) where

import			Control.Arrow((&&&))
import			Data.Array.IArray((!), (//))
import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Property.Empty			as Property.Empty
import qualified	BishBosh.Search.EphemeralData		as Search.EphemeralData
import qualified	BishBosh.Type.Count			as Type.Count
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.IntMap.Strict			as IntMap
import qualified	Data.List
import qualified	Data.Map				as Map
import qualified	Data.Maybe

{- |
	Used to contain the number of instances of each killer-move (a quiet move which triggered beta-cutoff),
	indexed by the number of plies into the game, at which it occurred,
	a key containing the killer-move,
	& the logical colour of the player making the move.
-}
type NInstancesByNPliesByKeyByLogicalColour killerMoveKey	= Attribute.LogicalColour.ArrayByLogicalColour (
	Map.Map killerMoveKey (
		IntMap.IntMap Type.Count.NPlies {-NInstances-}	-- CAVEAT: 'Int' is used to represent the number of plies into the game (in order to utilise 'IntMap') though it ought to be NPlies also.
	)
 )

-- | Data which can be used to advance the evaluation of identical sibling moves, in the hope of achieving beta-cutoff sooner.
newtype KillerMoves killerMoveKey	= MkKillerMoves {
	deconstruct	:: NInstancesByNPliesByKeyByLogicalColour killerMoveKey
}

instance Property.Empty.Empty (KillerMoves killerMoveKey) where
	empty	= MkKillerMoves . Attribute.LogicalColour.listArrayByLogicalColour $ repeat Property.Empty.empty

instance Search.EphemeralData.EphemeralData (KillerMoves killerMoveKey) where
	getSize MkKillerMoves { deconstruct = nInstancesByNPliesByKeyByLogicalColour }	= fromIntegral $ Data.Foldable.foldl' (
		Data.Foldable.foldl' $ Data.Foldable.foldl' (+)
	 ) 0 nInstancesByNPliesByKeyByLogicalColour

	euthanise nPlies killerMoves@MkKillerMoves { deconstruct = nInstancesByNPliesByKeyByLogicalColour }
		| nPlies <= 0	= killerMoves	-- This might occur at the start of the game, because the caller subtracts a fixed value from the current number of plies.
		| otherwise	= MkKillerMoves $ Data.Array.IArray.amap (
			Map.mapMaybe $ \m -> let
				m'	= IntMap.filterWithKey (\nPlies' _ -> nPlies' > fromIntegral nPlies) m
			in if Data.Foldable.null m'
				then Nothing
				else Just m'
		) nInstancesByNPliesByKeyByLogicalColour

-- | The type of a function which transforms a collection of killer-moves.
type Transformation killerMoveKey	= KillerMoves killerMoveKey -> KillerMoves killerMoveKey

-- | Insert a killer-move.
insert
	:: Ord killerMoveKey
	=> Type.Count.NPlies	-- ^ The total number of plies applied to the game.
	-> killerMoveKey
	-> Transformation killerMoveKey
insert nPlies killerMoveKey MkKillerMoves { deconstruct = nInstancesByNPliesByKeyByLogicalColour }	= MkKillerMoves $ nInstancesByNPliesByKeyByLogicalColour // [
	id &&& Map.insertWith (
		IntMap.unionWith (+)
	) killerMoveKey (
		IntMap.singleton (fromIntegral nPlies) 1
	) . (
		nInstancesByNPliesByKeyByLogicalColour !
	) $ if even nPlies
		then Attribute.LogicalColour.Black
		else Attribute.LogicalColour.White	-- White makes the first move.
 ] -- Singleton.

-- | Sorts an arbitrary list using the History-heuristic; <https://www.chessprogramming.org/History_Heuristic>.
sortByHistoryHeuristic
	:: Ord killerMoveKey
	=> Attribute.LogicalColour.LogicalColour
	-> (a -> killerMoveKey)	-- ^ Key-constructor.
	-> KillerMoves killerMoveKey
	-> [a]
	-> [a]
{-# INLINABLE sortByHistoryHeuristic #-}
sortByHistoryHeuristic logicalColour killerMoveKeyConstructor MkKillerMoves { deconstruct = nInstancesByNPliesByKeyByLogicalColour }	= Data.List.sortOn $ Data.Maybe.maybe 0 (
	negate {-largest first-} . Data.Foldable.foldl' (+) 0
 ) . (
	`Map.lookup` (nInstancesByNPliesByKeyByLogicalColour ! logicalColour)
 ) . killerMoveKeyConstructor

