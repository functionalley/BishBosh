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
-- ** Constructors
	mkSearchState,
	initialise
 ) where

import qualified	BishBosh.Data.Exception					as Data.Exception
import qualified	BishBosh.Evaluation.PositionHashQuantifiedGameTree	as Evaluation.PositionHashQuantifiedGameTree
import qualified	BishBosh.Evaluation.QuantifiedGame			as Evaluation.QuantifiedGame
import qualified	BishBosh.Model.Game					as Model.Game
import qualified	BishBosh.Property.Empty					as Property.Empty
import qualified	BishBosh.Search.DynamicMoveData				as Search.DynamicMoveData
import qualified	BishBosh.Search.EphemeralData				as Search.EphemeralData
import qualified	Control.Exception

-- | The data which is both received & returned by 'Search.Search.search', so that it is transported through the entire game.
data SearchState x y positionHash	= MkSearchState {
	getPositionHashQuantifiedGameTree	:: Evaluation.PositionHashQuantifiedGameTree.PositionHashQuantifiedGameTree x y positionHash,
	getDynamicMoveData			:: Search.DynamicMoveData.DynamicMoveData x y positionHash
}

instance Show (SearchState x y positionHash) where
	show _	= "SearchState {...}"

-- | Constructor.
mkSearchState
	:: Evaluation.PositionHashQuantifiedGameTree.PositionHashQuantifiedGameTree x y positionHash
	-> Search.DynamicMoveData.DynamicMoveData x y positionHash
	-> SearchState x y positionHash
mkSearchState	= MkSearchState

-- | Smart constructor.
initialise :: Evaluation.PositionHashQuantifiedGameTree.PositionHashQuantifiedGameTree x y positionHash -> SearchState x y positionHash
initialise positionHashQuantifiedGameTree
	| Model.Game.isTerminated game	= Control.Exception.throw $ Data.Exception.mkResultUndefined "BishBosh.Search.SearchState.initialise:\tcan't search for a move from a terminated game."
	| otherwise			= MkSearchState {
		getPositionHashQuantifiedGameTree	= positionHashQuantifiedGameTree,
		getDynamicMoveData			= Property.Empty.empty
	}
	where
		game	= Evaluation.QuantifiedGame.getGame $ Evaluation.PositionHashQuantifiedGameTree.getRootQuantifiedGame positionHashQuantifiedGameTree

instance Search.EphemeralData.MaybeEphemeralData (SearchState x y positionHash) where
	maybeEuthanise nPlies maybeRetireKillerMovesAfter maybeRetireTranspositionsAfter searchState@MkSearchState { getDynamicMoveData = dynamicMoveData }	= searchState {
		getDynamicMoveData	= Search.EphemeralData.maybeEuthanise nPlies maybeRetireKillerMovesAfter maybeRetireTranspositionsAfter dynamicMoveData	-- Forward the request.
	}

