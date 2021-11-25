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
	along with BishBosh.  If not, see <https://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]	The details of a player's turn.
-}

module BishBosh.Component.Turn(
-- * Types
-- ** Data-types
	Turn(
--		MkTurn,
		getQualifiedMove,
		getRank,
		getIsRepeatableMove
	),
-- * Functions
	compareByLVA,
	compareByMVVLVA,
-- ** Constructor
	mkTurn,
-- ** Predicates
	isCapture,
	isPawnDoubleAdvance
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.MoveType		as Attribute.MoveType
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Colour.LogicalColour		as Colour.LogicalColour
import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Component.QualifiedMove	as Component.QualifiedMove
import qualified	BishBosh.Property.Reflectable		as Property.Reflectable
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Data.Ord

{- |
	* Defines one turn of a player.

	* Additional data is recorded to facilitate both rollback & recording of the /move/ in various conventional notations.
-}
data Turn	= MkTurn {
	getQualifiedMove	:: Component.QualifiedMove.QualifiedMove,
	getRank			:: Attribute.Rank.Rank,	-- ^ The /rank/ of /piece/ that was moved, prior to any promotion.
	getIsRepeatableMove	:: Bool			-- ^ Whether this move can ever recur; without rolling-back.
}

instance Eq Turn where
	MkTurn {
		getQualifiedMove	= qualifiedMove,
		getRank			= rank
	} == MkTurn {
		getQualifiedMove	= qualifiedMove',
		getRank			= rank'
	} = (qualifiedMove, rank) == (qualifiedMove', rank')	-- 'getIsRepeatableMove' can be derived.

instance Control.DeepSeq.NFData Turn where
	rnf MkTurn {
		getQualifiedMove	= qualifiedMove,
		getRank			= rank,
		getIsRepeatableMove	= isRepeatableMove
	} = Control.DeepSeq.rnf (qualifiedMove, rank, isRepeatableMove)

instance Show Turn where
	showsPrec precedence MkTurn {
		getQualifiedMove	= qualifiedMove,
		getRank			= rank
--		getIsRepeatableMove	= isRepeatableMove
	} = showsPrec precedence (
		qualifiedMove,
		rank
--		isRepeatableMove	-- Derived.
	 ) -- Represent as a tuple.

instance Read Turn where
	readsPrec precedence	= map (Control.Arrow.first $ uncurry mkTurn) . readsPrec precedence

instance Property.Reflectable.ReflectableOnX Turn where
	reflectOnX turn@MkTurn { getQualifiedMove = qualifiedMove } = turn { getQualifiedMove = Property.Reflectable.reflectOnX qualifiedMove }

-- | Smart constructor.
mkTurn
	:: Component.QualifiedMove.QualifiedMove
	-> Attribute.Rank.Rank
	-> Turn
mkTurn qualifiedMove rank = MkTurn {
	getQualifiedMove	= qualifiedMove,
	getRank			= rank,
	getIsRepeatableMove	= rank /= Attribute.Rank.Pawn {-can't retreat-} && not (
		Attribute.MoveType.isAcyclic $ Component.QualifiedMove.getMoveType qualifiedMove
	) -- Infer.
}

-- | Convenience.
isCapture :: Turn -> Bool
isCapture MkTurn { getQualifiedMove = qualifiedMove }	= Attribute.MoveType.isCapture $ Component.QualifiedMove.getMoveType qualifiedMove

-- | Whether the /turn/ represents a @Pawn@'s initial two-square advance.
isPawnDoubleAdvance
	:: Colour.LogicalColour.LogicalColour	-- ^ Defines the side whose /turn/ is referenced.
	-> Turn
	-> Bool
isPawnDoubleAdvance logicalColour MkTurn {
	getRank			= Attribute.Rank.Pawn,
	getQualifiedMove	= qualifiedMove
} = Component.Move.isPawnDoubleAdvance logicalColour (Component.QualifiedMove.getMove qualifiedMove) && Component.QualifiedMove.getMoveType qualifiedMove == Data.Default.def
isPawnDoubleAdvance _ _	= False

-- | Forwards the request to 'Attribute.Rank.compareByLVA'.
compareByLVA
	:: Attribute.Rank.EvaluateRank
	-> Turn
	-> Turn
	-> Ordering
compareByLVA evaluateRank MkTurn { getRank = rankL } MkTurn { getRank = rankR }	= Attribute.Rank.compareByLVA evaluateRank rankL rankR

{- |
	* Compares /turn/s by <https://www.chessprogramming.org/MVV-LVA>.

	* This orders the most valuable victim of an attack first, but when victims are of equal rank, orders the least valuable aggressor first.

	* N.B.: the order of non-capture moves (including promotions) isn't defined.

	* CAVEAT: no account is made for any defenders of the attacked piece, which might recoup transient gains.
-}
compareByMVVLVA
	:: Attribute.Rank.EvaluateRank
	-> Turn
	-> Turn
	-> Ordering
compareByMVVLVA evaluateRank turnL@MkTurn {
	getQualifiedMove	= qualifiedMoveL
} turnR@MkTurn {
	getQualifiedMove	= qualifiedMoveR
} = case ($ qualifiedMoveL) &&& ($ qualifiedMoveR) $ Attribute.MoveType.getMaybeImplicitlyTakenRank . Component.QualifiedMove.getMoveType of
	(Nothing, Nothing)	-> EQ
	(Nothing, _)		-> GT
	(_, Nothing)		-> LT
	(Just rankL, Just rankR)
		| rankL == rankR	-> lvaComparison
		| otherwise		-> case Data.Ord.comparing evaluateRank rankR rankL {-MVV-} of
			EQ		-> lvaComparison
			ordering	-> ordering	-- MVV-comparison uniquely defines the order.
	where
		lvaComparison	= compareByLVA evaluateRank turnL turnR

