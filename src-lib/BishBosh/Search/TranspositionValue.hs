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
	TranspositionValue (
--		MkTranspositionValue,
		getIsOptimal,
		getNPlies,
		getQualifiedMoves
	),
-- * Functions
	inferSearchDepth,
-- ** Constructor
	mkTranspositionValue,
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
data TranspositionValue qualifiedMove	= MkTranspositionValue {
	getIsOptimal		:: IsOptimal,			-- ^ Whether the recorded move-sequence is known to be optimal.
	getNPlies		:: Component.Move.NPlies,	-- ^ The number of plies applied to the /game/ before application of any of the specified moves.
	getQualifiedMoves	:: [qualifiedMove]		-- ^ The sequence of qualifiedMoves applied to the /game/, which caused the alpha-beta event.
} deriving Show

-- | Smart constructor.
mkTranspositionValue
	:: IsOptimal
	-> Component.Move.NPlies
	-> [qualifiedMove]
	-> TranspositionValue qualifiedMove
mkTranspositionValue _ _ []	= Control.Exception.throw $ Data.Exception.mkNullDatum "BishBosh.Search.TranspositionValue.mkTranspositionValue:\tnull list of qualifiedMoves."
mkTranspositionValue isOptimal nPlies qualifiedMoves
	| nPlies < 0	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Search.TranspositionValue.mkTranspositionValue:\tnPlies=" $ shows nPlies " can't be negative."
	| otherwise	= MkTranspositionValue {
		getIsOptimal		= isOptimal,
		getNPlies		= nPlies,
		getQualifiedMoves	= qualifiedMoves
	}

-- | Infer the search-depth from the length of the qualifiedMove-sequence.
inferSearchDepth :: TranspositionValue qualifiedMove -> Component.Move.NPlies
inferSearchDepth	= length . getQualifiedMoves

{- |
	* The type of a function which can find the fitness of the game resulting from the recorded sequence of qualifiedMoves.

	* CAVEAT: the fitness this function returns should be from the perspective of the player to make the first move.
-}
type FindFitness qualifiedMove weightedMean	= TranspositionValue qualifiedMove -> weightedMean

{- |
	* Whether a proposed value is better than the incumbent.

	* CAVEAT: this is a narrower concept than addressed by 'Ord', which implies 'Eq'.
-}
isBetter
	:: Ord weightedMean
	=> FindFitness qualifiedMove weightedMean
	-> TranspositionValue qualifiedMove	-- ^ The proposed value.
	-> TranspositionValue qualifiedMove	-- ^ The incumbent value.
	-> Bool
isBetter findFitness proposedValue incumbentValue	= case Data.Ord.comparing inferSearchDepth proposedValue incumbentValue of
	GT	-> True	-- The new search is deeper.
	EQ	-> getIsOptimal proposedValue || Data.Ord.comparing findFitness proposedValue incumbentValue == GT
	_	-> False

