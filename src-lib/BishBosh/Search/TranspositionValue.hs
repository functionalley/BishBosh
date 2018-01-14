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

 [@DESCRIPTION@]	Defines an entry in the transposition-table.
-}

module BishBosh.Search.TranspositionValue (
-- * Types
-- ** Type-synonyms
	IsOptimal,
	FindFitness,
-- ** Data-types
	Value(
--		MkValue,
		getIsOptimal,
		getNPlies,
		getMoves
	),
-- * Functions
	inferSearchDepth,
-- ** Constructor
	mkValue,
-- ** Predicates
	isBetter
 ) where

import qualified	BishBosh.Component.Move	as Component.Move
import qualified	BishBosh.Data.Exception	as Data.Exception
import qualified	Control.Exception
import qualified	Data.Ord

-- | Whether the recorded move-sequence is known to be optimal.
type IsOptimal	= Bool

-- | The type of the values in the transposition-table.
data Value move	= MkValue {
	getIsOptimal	:: IsOptimal,
	getNPlies	:: Component.Move.NPlies,	-- ^ The number of plies applied to the /game/ before application of any of the specified moves.
	getMoves	:: [move]			-- ^ The sequence of moves applied to the /game/, which caused the alpha-beta event.
}

-- | Smart constructor.
mkValue
	:: IsOptimal
	-> Component.Move.NPlies
	-> [move]
	-> Value move
mkValue _ _ []	= Control.Exception.throw $ Data.Exception.mkNullDatum "BishBosh.Search.TranspositionValue.mkValue:\tnull list of moves."
mkValue isOptimal nPlies moves
	| nPlies < 0	= Control.Exception.throw $ Data.Exception.mkOutOfBounds "BishBosh.Search.TranspositionValue.mkValue:\tnPlies can't be negative."
	| otherwise	= MkValue {
		getIsOptimal	= isOptimal,
		getNPlies	= nPlies,
		getMoves	= moves
	}

-- | Infer the search-depth from the length of the move-sequence.
inferSearchDepth :: Value move -> Component.Move.NPlies
inferSearchDepth	= length . getMoves

{- |
	* The type of a function which can find the fitness of the game resulting from the recorded sequence of moves.

	* CAVEAT: the fitness this function returns should be from the perspective of the player to make the first move.
-}
type FindFitness move weightedMean	= Value move -> weightedMean

{- |
	* Whether a proposed value is better than the incumbent.

	* CAVEAT: this is a narrower concept than addressed by 'Ord', which implies 'Eq'.
-}
isBetter
	:: Ord weightedMean
	=> FindFitness move weightedMean
	-> Value move	-- ^ The proposed value.
	-> Value move	-- ^ The incumbent value.
	-> Bool
isBetter findFitness proposedValue incumbentValue	= case Data.Ord.comparing inferSearchDepth proposedValue incumbentValue of
	GT	-> True	-- The new search is deeper.
	EQ	-> getIsOptimal proposedValue || Data.Ord.comparing findFitness proposedValue incumbentValue == GT
	_	-> False

