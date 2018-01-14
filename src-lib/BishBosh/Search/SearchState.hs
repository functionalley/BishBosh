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

 [@DESCRIPTION@]	The state which is threaded through successive calls to 'Search.Search.search'.
-}

module BishBosh.Search.SearchState(
-- * Types
-- ** Data-types
	SearchState(
--		MkSearchState,
		getPositionHashQuantifiedGameTree,
		getDynamicMoveData
	),
-- * Functions
	euthanise,
-- ** Constructors
	mkSearchState,
	initialise
 ) where

import qualified	BishBosh.Component.Move					as Component.Move
import qualified	BishBosh.Evaluation.PositionHashQuantifiedGameTree	as Evaluation.PositionHashQuantifiedGameTree
import qualified	BishBosh.Input.SearchOptions				as Input.SearchOptions
import qualified	BishBosh.Property.Empty					as Property.Empty
import qualified	BishBosh.Search.DynamicMoveData				as Search.DynamicMoveData

-- | The data which is both received & returned by 'select', so that it is transported through the entire game.
data SearchState x y positionHash criterionValue weightedMean	= MkSearchState {
	getPositionHashQuantifiedGameTree	:: Evaluation.PositionHashQuantifiedGameTree.PositionHashQuantifiedGameTree x y positionHash criterionValue weightedMean,
	getDynamicMoveData			:: Search.DynamicMoveData.DynamicMoveData x y positionHash
}

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Show	x,
	Show	y,
	Show	criterionValue,
	Show	positionHash,
	Show	weightedMean
 ) => Show (SearchState x y positionHash criterionValue weightedMean) where
	showsPrec _ _	= showString "SearchState {...}"

-- | Constructor.
mkSearchState
	:: Evaluation.PositionHashQuantifiedGameTree.PositionHashQuantifiedGameTree x y positionHash criterionValue weightedMean
	-> Search.DynamicMoveData.DynamicMoveData x y positionHash
	-> SearchState x y positionHash criterionValue weightedMean
mkSearchState	= MkSearchState

-- | Constructor.
initialise :: Evaluation.PositionHashQuantifiedGameTree.PositionHashQuantifiedGameTree x y positionHash criterionValue weightedMean -> SearchState x y positionHash criterionValue weightedMean
initialise positionHashQuantifiedGameTree = MkSearchState {
	getPositionHashQuantifiedGameTree	= positionHashQuantifiedGameTree,
	getDynamicMoveData			= Property.Empty.empty
}

-- | Forwards request.
euthanise
	:: Component.Move.NPlies
	-> Input.SearchOptions.MaybeRetireAfterNMoves
	-> Input.SearchOptions.MaybeRetireAfterNMoves
	-> SearchState x y positionHash criterionValue weightedMean
	-> SearchState x y positionHash criterionValue weightedMean
euthanise nPlies maybeRetireKillerMovesAfter maybeRetireTranspositionsAfter searchState@MkSearchState { getDynamicMoveData = dynamicMoveData }	= searchState {
	getDynamicMoveData	= Search.DynamicMoveData.euthanise nPlies maybeRetireKillerMovesAfter maybeRetireTranspositionsAfter dynamicMoveData
}

