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

 [@DESCRIPTION@]	Data on /move/s, gathered while searching.

-}

module BishBosh.Search.DynamicMoveData(
-- * Types
-- ** Type-synonyms
	Transformation,
-- ** Data-types
	KillerMoveKey(),
	DynamicMoveData(
--		MkDynamicMoveData,
		getKillerMoves,
		getTranspositions
	),
-- * Functions
-- ** Constructors
	mkKillerMoveKeyFromTurn,
-- ** Mutators
	updateKillerMoves,
	updateTranspositions
 ) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Component.QualifiedMove	as Component.QualifiedMove
import qualified	BishBosh.Component.Turn			as Component.Turn
import qualified	BishBosh.Property.Empty			as Property.Empty
import qualified	BishBosh.Search.EphemeralData		as Search.EphemeralData
import qualified	BishBosh.Search.KillerMoves		as Search.KillerMoves
import qualified	BishBosh.Search.Transpositions		as Search.Transpositions
import qualified	BishBosh.Type.Count			as Type.Count
import qualified	Data.Maybe

{- |
	* Killer-moves are indexed by both the /move/ & the /rank/ of the piece which made it.

	* CAVEAT: there's still ambiguity in this /key/, since it may match either a different piece of the same /rank/ or have a different /move-type/ (though typically only quiet moves are recorded), in sibling games.
-}
data KillerMoveKey x y	= MkKillerMoveKey (Component.Move.Move x y) Attribute.Rank.Rank deriving (Eq, Ord, Show)

-- | Constructor.
mkKillerMoveKeyFromTurn :: Component.Turn.Turn x y -> KillerMoveKey x y
mkKillerMoveKeyFromTurn	= uncurry MkKillerMoveKey . (Component.QualifiedMove.getMove . Component.Turn.getQualifiedMove &&& Component.Turn.getRank)

-- | The data on /move/s, gathered while searching.
data DynamicMoveData x y positionHash	= MkDynamicMoveData {
	getKillerMoves		:: Search.KillerMoves.KillerMoves (KillerMoveKey x y),
	getTranspositions	:: Search.Transpositions.Transpositions (Component.QualifiedMove.QualifiedMove x y) positionHash	-- ^ N.B. a qualifiedMove is used to additionally record any promotion-rank.
}

instance Property.Empty.Empty (DynamicMoveData x y positionHash) where
	empty = MkDynamicMoveData {
		getKillerMoves		= Property.Empty.empty,
		getTranspositions	= Property.Empty.empty
	}

-- | The type of a function which transforms the dynamic move-data.
type Transformation x y positionHash	= DynamicMoveData x y positionHash -> DynamicMoveData x y positionHash

-- | Mutator.
updateKillerMoves :: Search.KillerMoves.Transformation (KillerMoveKey x y) -> Transformation x y positionHash
updateKillerMoves f dynamicMoveData@MkDynamicMoveData { getKillerMoves = killerMoves }	= dynamicMoveData { getKillerMoves = f killerMoves }

-- | Mutator.
updateTranspositions :: Search.Transpositions.Transformation (Component.QualifiedMove.QualifiedMove x y) positionHash -> Transformation x y positionHash
updateTranspositions f dynamicMoveData@MkDynamicMoveData { getTranspositions = transpositions }	= dynamicMoveData { getTranspositions = f transpositions }

instance Search.EphemeralData.MaybeEphemeralData (DynamicMoveData x y positionHash) where
	maybeEuthanise nPlies maybeRetireKillerMovesAfter maybeRetireTranspositionsAfter MkDynamicMoveData {
		getKillerMoves		= killerMoves,
		getTranspositions	= transpositions
	} = MkDynamicMoveData {
		getKillerMoves	= Data.Maybe.maybe id (
			Search.EphemeralData.euthanise . reduceNPlies	-- When searching for a move at (nPlies + 1), matches with killer-moves from 'iterate (subtract 2) $ pred nPlies' are relevant up to a point. N.B. the opponent's killer-moves are useless.
		) maybeRetireKillerMovesAfter killerMoves,
		getTranspositions	= Data.Maybe.maybe id (
			Search.EphemeralData.euthanise . reduceNPlies
		) maybeRetireTranspositionsAfter transpositions
	} where
		reduceNPlies :: Type.Count.NMoves -> Type.Count.NPlies
		reduceNPlies	= (`subtract` nPlies) . (* Component.Move.nPliesPerMove) . fromIntegral

