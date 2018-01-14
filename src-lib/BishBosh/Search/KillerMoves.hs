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

 [@DESCRIPTION@] <https://chessprogramming.wikispaces.com/Killer+Heuristic>.
-}

module BishBosh.Search.KillerMoves (
-- * Types
-- ** Type-synonyms
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
import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Property.Empty			as Property.Empty
import qualified	BishBosh.Search.EphemeralData		as Search.EphemeralData
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.IntMap
import qualified	Data.List
import qualified	Data.Map
import qualified	Data.Map.Strict
import qualified	Data.Maybe

{- |
	* Used to contain the number of instances of each killer-move (a quiet move which triggered beta-cutoff),
	indexed by the logical-colour of the player making the move & the number of plies into the game, at which it occurred.

	* These data can be used to advance the evaluation of identical sibling moves, in the hope of achieving beta-cutoff sooner.
-}
newtype KillerMoves killerMove	= MkKillerMoves {
	deconstruct	:: Attribute.LogicalColour.ByLogicalColour (
		Data.Map.Map killerMove (
			Data.IntMap.IntMap {-by NPlies-} Component.Move.NMoves
		)
	)
}

instance Property.Empty.Empty (KillerMoves killerMove) where
	empty	= MkKillerMoves . Attribute.LogicalColour.listArrayByLogicalColour $ repeat Data.Map.empty

instance Search.EphemeralData.EphemeralData (KillerMoves killerMove) where
	getSize MkKillerMoves { deconstruct = nInstancesByKeyByNPliesByLogicalColour }	= Data.Foldable.foldl' (
		Data.Map.foldl' $ Data.IntMap.foldl' (+)
	 ) 0 nInstancesByKeyByNPliesByLogicalColour

	euthanise nPlies killerMoves@MkKillerMoves { deconstruct = nInstancesByKeyByNPliesByLogicalColour }
		| nPlies <= 0	= killerMoves	-- This might occur at the start of the game, because the caller subtracts a fixed value from the current number of plies.
		| otherwise	= MkKillerMoves $ Data.Array.IArray.amap (
			Data.Map.mapMaybe $ \m -> let
				m'	= Data.IntMap.filterWithKey (\nPlies' _ -> nPlies' > nPlies) m
			in if Data.IntMap.null m'
				then Nothing
				else Just m'
		) nInstancesByKeyByNPliesByLogicalColour

-- | The type of a function which transforms a collection of killer-moves.
type Transformation killerMove	= KillerMoves killerMove -> KillerMoves killerMove

-- | Insert a killer-move.
insert
	:: Ord killerMove
	=> Component.Move.NPlies	-- ^ The total number of plies applied to the game.
	-> killerMove
	-> Transformation killerMove
insert nPlies killerMove MkKillerMoves { deconstruct = nInstancesByKeyByNPliesByLogicalColour }	= MkKillerMoves $ nInstancesByKeyByNPliesByLogicalColour // [
	id &&& Data.Map.Strict.insertWith (
		Data.IntMap.unionWith (+)
	) killerMove (
		Data.IntMap.singleton nPlies 1
	) . (nInstancesByKeyByNPliesByLogicalColour !) $ if even nPlies
		then Attribute.LogicalColour.Black
		else Attribute.LogicalColour.White	-- White makes the first move.
 ] -- Singleton.

-- | Sorts an arbitrary list using the History-heuristic; <https://chessprogramming.wikispaces.com/History+Heuristic>.
sortByHistoryHeuristic
	:: Ord killerMove
	=> Attribute.LogicalColour.LogicalColour
	-> (a -> killerMove)	-- ^ Constructor.
	-> KillerMoves killerMove
	-> [a]
	-> [a]
sortByHistoryHeuristic logicalColour killerMoveConstructor MkKillerMoves { deconstruct = nInstancesByNPliesByKeyByLogicalColour }	= Data.List.sortOn $ Data.Maybe.maybe 0 (
	negate {-largest first-} . Data.IntMap.foldl' (+) 0
 ) . (
	`Data.Map.lookup` (nInstancesByNPliesByKeyByLogicalColour ! logicalColour)
 ) . killerMoveConstructor
