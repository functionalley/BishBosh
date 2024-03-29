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
--	NPositionsByPosition,
--	Transformation,
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
--	insertPosition',
	insertPosition,
	deletePosition,
-- ** Predicates
	anyInstancesByPosition
) where

import qualified	BishBosh.Property.Empty		as Property.Empty
import qualified	BishBosh.Property.Reflectable	as Property.Reflectable
import qualified	BishBosh.Type.Count		as Type.Count
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Foldable
import qualified	Data.Map.Strict			as Map

-- | The smallest number of repeatable plies (applied by alternating players) required to form a cycle.
leastCyclicPlies :: Type.Count.NPlies
leastCyclicPlies	= 4

{- |
	* A count of the number of instances of /position/s which have occurred.

	* N.B.: a number greater than @1@ represents repetition.

	* The /position/ can either be represented by a physical 'State.Position.Position', or by proxy using a hash.
-}
type NPositionsByPosition position	= Map.Map position Type.Count.NPositions

-- | Insert a position into the unwrapped collection.
insertPosition' :: Ord position => position -> NPositionsByPosition position -> NPositionsByPosition position
insertPosition'	= flip (Map.insertWith $ const succ) 1

-- | Wrap the type, so that class-instances can be hung from it.
newtype InstancesByPosition position	= MkInstancesByPosition {
	getNPositionsByPosition	:: NPositionsByPosition position
} deriving Eq

instance Control.DeepSeq.NFData position => Control.DeepSeq.NFData (InstancesByPosition position) where
	rnf MkInstancesByPosition { getNPositionsByPosition = m }	= Control.DeepSeq.rnf m

instance (
	Ord					position,
	Property.Reflectable.ReflectableOnX	position
 ) => Property.Reflectable.ReflectableOnX (InstancesByPosition position) where
	reflectOnX MkInstancesByPosition { getNPositionsByPosition = m }	= MkInstancesByPosition $ Map.mapKeys Property.Reflectable.reflectOnX m

-- | Construct from repeatable data.
mkInstancesByPosition
	:: (Foldable foldable, Ord position)
	=> (a -> position)	-- ^ Position-constructor.
	-> foldable a		-- ^ Data from which to construct positions.
	-> InstancesByPosition position
mkInstancesByPosition f	= MkInstancesByPosition . Data.Foldable.foldr (insertPosition' . f) Property.Empty.empty

-- | Constructor.
mkSingleton :: position -> InstancesByPosition position
mkSingleton	= MkInstancesByPosition . (`Map.singleton` 1)


{- |
	* Count the total number of consecutive repeatable plies amongst recent moves.

	* This is equivalent to the number of entries in the map, since adding a non-repeatable move triggers a purge.
-}
countConsecutiveRepeatablePlies :: InstancesByPosition position -> Type.Count.NPlies
countConsecutiveRepeatablePlies MkInstancesByPosition { getNPositionsByPosition = m }	= fromIntegral $ Data.Foldable.foldl' (+) (
	negate 1	-- The map is never empty, since before the first move a singleton is constructed with the initial position.
 ) m

-- | Count the total number of repetitions of /position/s.
countPositionRepetitions :: InstancesByPosition position -> Type.Count.NPositions
countPositionRepetitions MkInstancesByPosition { getNPositionsByPosition = m }	= Data.Foldable.foldl' (
	(+) . pred	-- The initial instance isn't a repetition.
 ) 0 m

-- | The number of distinct /position/s.
getNDistinctPositions :: InstancesByPosition position -> Type.Count.NPositions
getNDistinctPositions MkInstancesByPosition { getNPositionsByPosition = m }	= fromIntegral $ Data.Foldable.length m {-the number of keys-}

-- | Predicate: apply the specified predicate to the map.
anyInstancesByPosition
	:: (Type.Count.NPositions -> Bool)
	-> InstancesByPosition position
	-> Bool
anyInstancesByPosition predicate MkInstancesByPosition { getNPositionsByPosition = m }	= Data.Foldable.any predicate m

{- |
	* Find the maximum number of times any one position has already been visited.

	* CAVEAT: only those positions that can still be reached are considered.
-}
findMaximumInstances :: InstancesByPosition position -> Type.Count.NPositions
findMaximumInstances MkInstancesByPosition { getNPositionsByPosition = m }
	| Data.Foldable.null m	= 0	-- CAVEAT: this shouldn't happen.
	| otherwise		= Data.Foldable.maximum m

-- | The type of a function which transforms the collection.
type Transformation position	= InstancesByPosition position -> InstancesByPosition position

-- | Insert a /position/ into the collection.
insertPosition
	:: Ord position
	=> Bool	-- ^ Whether the /turn/ which led to the specified /position/, was repeatable.
	-> position
	-> Transformation position
insertPosition isRepeatable position MkInstancesByPosition { getNPositionsByPosition = m }
	| isRepeatable	= MkInstancesByPosition $ insertPosition' position m	-- Include this position.
	| otherwise	= mkSingleton position					-- The previous position can't be revisited without rolling-back.

-- | Remove a /position/ from the collection, as required to implement rollback.
deletePosition :: Ord position => position -> Transformation position
deletePosition position MkInstancesByPosition { getNPositionsByPosition = m }	= MkInstancesByPosition . Map.update (
	\n -> if n == 1
		then Nothing		-- Delete the entry.
		else Just $ pred n	-- Decrement the number of instances.
 ) position $ Control.Exception.assert (Map.member position m) m

