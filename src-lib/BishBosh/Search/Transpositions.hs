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

	* <https://www.chessprogramming.org/Transposition_Table>.

	* <https://en.wikipedia.org/wiki/Transposition_table>.

	* Valid move-sequences can be recorded against the hash of the position from which they start.
-}

module BishBosh.Search.Transpositions (
-- * Types
-- ** Type-synonyms
	Transformation,
-- ** Data-types
	Transpositions(),
-- * Functions
	find,
-- ** Mutators
	insert
 ) where

import qualified	BishBosh.Property.Empty			as Property.Empty
import qualified	BishBosh.Search.EphemeralData		as Search.EphemeralData
import qualified	BishBosh.Search.TranspositionValue	as Search.TranspositionValue
import qualified	Data.Map
import qualified	Data.Maybe

-- | Stores the result of an alpha-beta search from a /position/.
newtype Transpositions move positionHash	= MkTranspositions {
	deconstruct	:: Data.Map.Map positionHash (Search.TranspositionValue.Value move)
}

instance Property.Empty.Empty (Transpositions move positionHash) where
	empty	= MkTranspositions Data.Map.empty

instance Search.EphemeralData.EphemeralData (Transpositions move positionHash) where
	getSize	MkTranspositions { deconstruct = byPositionHash }		= Data.Map.size byPositionHash
	euthanise nPlies MkTranspositions { deconstruct = byPositionHash }	= MkTranspositions $ Data.Map.filter ((> nPlies) . Search.TranspositionValue.getNPlies) byPositionHash

-- | Returns any value previously recorded when searching from the specified /position/.
find
	:: Ord positionHash
	=> positionHash
	-> Transpositions move positionHash
	-> Maybe (Search.TranspositionValue.Value move)
find positionHash MkTranspositions { deconstruct = byPositionHash }	= Data.Map.lookup positionHash byPositionHash

-- | The type of a function which transforms 'Transpositions'.
type Transformation move positionHash	= Transpositions move positionHash -> Transpositions move positionHash

{- |
	* Optionally record a value found while searching for the optimal move from a position, against the position's hash.

	* If a matching key already exists, it's replaced if the new value is considered to be better.
-}
insert
	:: (Ord positionHash, Ord weightedMean)
	=> Search.TranspositionValue.FindFitness move weightedMean
	-> positionHash				-- ^ Represents the game from which the sequence of moves starts.
	-> Search.TranspositionValue.Value move	-- ^ The value to record.
	-> Transformation move positionHash
insert findFitness positionHash proposedValue MkTranspositions { deconstruct = byPositionHash }	= MkTranspositions $ Data.Map.alter (
	Data.Maybe.maybe (Just proposedValue) {-there's no incumbent-} $ \incumbentValue -> if Search.TranspositionValue.isBetter findFitness proposedValue incumbentValue
		then Just proposedValue	-- Upgrade.
		else Nothing	-- Leave incumbent.
 ) positionHash byPositionHash

