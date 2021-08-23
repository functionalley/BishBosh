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

 [@DESCRIPTION@]	Records the number of times each /position/ has been encountered since the last unrepeatable move.
-}

module BishBosh.State.InstancesByPosition(
-- * Types
-- ** Type-synonyms
--	NBoardsByPosition,
--	Transformation.
-- * Constants
	leastCyclicPlies,
-- ** Data-types
	InstancesByPosition(),
-- * Functions
	countConsecutiveRepeatablePlies,
	countPositionRepetitions,
	getNDistinctPositions,
	findMaximumInstances,
-- ** Constructors
	mkInstancesByPosition,
	mkSingleton,
-- ** Mutators
	insertPosition,
	deletePosition,
-- ** Predicates
	anyInstancesByPosition
) where

import qualified	BishBosh.Component.Move		as Component.Move
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Property.Reflectable	as Property.Reflectable
import qualified	BishBosh.State.Board		as State.Board
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Foldable
import qualified	Data.Map.Strict

-- | The smallest number of repeatable plies required to form a cycle.
leastCyclicPlies :: Component.Move.NPlies
leastCyclicPlies	= 4

{- |
	* The number of times each /position/ has been encountered.

	* The /position/ can either be represented by a physical 'State.Position.Position', or by proxy using a hash.
-}
type NBoardsByPosition position	= Data.Map.Strict.Map position State.Board.NBoards

{- |
	* A count of the number of instances of /position/s which have occurred.

	* N.B.: a number greater than @1@ represents repetition.
-}
newtype InstancesByPosition position	= MkInstancesByPosition {
	getNBoardsByPosition	:: NBoardsByPosition position
} deriving Eq

instance Control.DeepSeq.NFData position => Control.DeepSeq.NFData (InstancesByPosition position) where
	rnf MkInstancesByPosition { getNBoardsByPosition = m }	= Control.DeepSeq.rnf m

instance (
	Ord					position,
	Property.Reflectable.ReflectableOnX	position
 ) => Property.Reflectable.ReflectableOnX (InstancesByPosition position) where
	reflectOnX MkInstancesByPosition { getNBoardsByPosition = m }	= MkInstancesByPosition $ Data.Map.Strict.mapKeys Property.Reflectable.reflectOnX m

-- | Smart constructor.
mkInstancesByPosition :: NBoardsByPosition position -> InstancesByPosition position
mkInstancesByPosition nBoardsByPosition
	| Data.Foldable.any (< 1) nBoardsByPosition	= Control.Exception.throw $ Data.Exception.mkOutOfBounds "BishBosh.State.InstancesByPosition.mkInstancesByPosition:\teach specified position must have been visited at least once."
	| otherwise					= MkInstancesByPosition nBoardsByPosition

-- | Constructor.
mkSingleton :: position -> InstancesByPosition position
mkSingleton position	= MkInstancesByPosition $ Data.Map.Strict.singleton position 1

{- |
	* Count the total number of consecutive repeatable plies amongst recent moves.

	* This is equivalent to the number of entries in the map, since adding a non-repeatable move triggers a purge.
-}
countConsecutiveRepeatablePlies :: InstancesByPosition position -> Component.Move.NPlies
countConsecutiveRepeatablePlies MkInstancesByPosition { getNBoardsByPosition = m }	= Data.Map.Strict.foldl' (+) (
	negate 1	-- The map is never empty, since before the first move a singleton is constructed with the initial position.
 ) m

-- | Count the total number of repetitions of /position/s.
countPositionRepetitions :: InstancesByPosition position -> State.Board.NBoards
countPositionRepetitions MkInstancesByPosition { getNBoardsByPosition = m }	= Data.Map.Strict.foldl' (
	(+) . pred	-- The initial instance isn't a repetition.
 ) 0 m

-- | The number of distinct /position/s.
getNDistinctPositions :: InstancesByPosition position -> State.Board.NBoards
getNDistinctPositions MkInstancesByPosition { getNBoardsByPosition = m }	= Data.Map.Strict.size m

-- | Predicate: apply the specified predicate to the map.
anyInstancesByPosition
	:: (State.Board.NBoards -> Bool)
	-> InstancesByPosition position
	-> Bool
anyInstancesByPosition predicate MkInstancesByPosition { getNBoardsByPosition = m }	= Data.Foldable.any predicate m

{- |
	* Find the maximum number of times any one position has already been visited.

	* CAVEAT: only those positions that can still be reached are considered.
-}
findMaximumInstances :: InstancesByPosition position -> State.Board.NBoards
findMaximumInstances MkInstancesByPosition { getNBoardsByPosition = m }
	| Data.Map.Strict.null m	= 0	-- CAVEAT: this shouldn't happen.
	| otherwise			= Data.Foldable.maximum m

-- | The type of a function which transforms the collection.
type Transformation position	= InstancesByPosition position -> InstancesByPosition position

-- | Insert a /position/ into the collection.
insertPosition
	:: Ord position
	=> Bool	-- ^ Whether the /turn/ which led to the specified /position/, was repeatable.
	-> position
	-> Transformation position
insertPosition isRepeatable position MkInstancesByPosition { getNBoardsByPosition = m }
	| isRepeatable	= MkInstancesByPosition $ Data.Map.Strict.insertWith (const succ) position 1 m	-- Include this position.
	| otherwise	= mkSingleton position								-- The previous position can't be revisited without rolling-back.

-- | Remove a /position/ from the collection, as required to implement rollback.
deletePosition :: Ord position => position -> Transformation position
deletePosition position MkInstancesByPosition { getNBoardsByPosition = m }	= MkInstancesByPosition . Data.Map.Strict.update (
	\n -> if n == 1
		then Nothing		-- Delete the entry.
		else Just $ pred n	-- Decrement the number of instances.
 ) position $ Control.Exception.assert (Data.Map.Strict.member position m) m
