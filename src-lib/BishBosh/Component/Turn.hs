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
import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Attribute.MoveType		as Attribute.MoveType
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
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
data Turn x y	= MkTurn {
	getQualifiedMove	:: Component.QualifiedMove.QualifiedMove x y,
	getRank			:: Attribute.Rank.Rank,	-- ^ The /rank/ of /piece/ that was moved, prior to any promotion.
	getIsRepeatableMove	:: Bool			-- ^ Whether this move can ever recur; without rolling-back.
}

instance (Eq x, Eq y) => Eq (Turn x y) where
	MkTurn {
		getQualifiedMove	= qualifiedMove,
		getRank			= rank
	} == MkTurn {
		getQualifiedMove	= qualifiedMove',
		getRank			= rank'
	} = (qualifiedMove, rank) == (qualifiedMove', rank')	-- 'getIsRepeatableMove' can be derived.

instance (Control.DeepSeq.NFData x, Control.DeepSeq.NFData y) => Control.DeepSeq.NFData (Turn x y) where
	rnf MkTurn {
		getQualifiedMove	= qualifiedMove,
		getRank			= rank,
		getIsRepeatableMove	= isRepeatableMove
	} = Control.DeepSeq.rnf (qualifiedMove, rank, isRepeatableMove)

instance (Show x, Show y) => Show (Turn x y) where
	showsPrec _ MkTurn {
		getQualifiedMove	= qualifiedMove,
		getRank			= rank
--		getIsRepeatableMove	= isRepeatableMove
	} = shows (
		qualifiedMove,
		rank
--		isRepeatableMove	-- Derived.
	 ) -- Represent as a tuple.

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Read	x,
	Read	y
 ) => Read (Turn x y) where
	readsPrec _	= map (Control.Arrow.first $ uncurry mkTurn) . reads

instance Enum y => Property.Reflectable.ReflectableOnX (Turn x y) where
	reflectOnX turn@MkTurn { getQualifiedMove = qualifiedMove } = turn { getQualifiedMove = Property.Reflectable.reflectOnX qualifiedMove }

-- | Smart constructor.
mkTurn
	:: Component.QualifiedMove.QualifiedMove x y
	-> Attribute.Rank.Rank
	-> Turn x y
mkTurn qualifiedMove rank = MkTurn {
	getQualifiedMove	= qualifiedMove,
	getRank			= rank,
	getIsRepeatableMove	= rank /= Attribute.Rank.Pawn {-can't retreat-} && not (
		Attribute.MoveType.isAcyclic $ Component.QualifiedMove.getMoveType qualifiedMove
	) -- Infer.
}

-- | Convenience.
isCapture :: Turn x y -> Bool
isCapture MkTurn { getQualifiedMove = qualifiedMove }	= Attribute.MoveType.isCapture $ Component.QualifiedMove.getMoveType qualifiedMove

-- | Whether the /turn/ represents a @Pawn@'s initial two-square advance.
isPawnDoubleAdvance :: (
	Enum	x,
	Enum	y,
	Eq	y
 )
	=> Attribute.LogicalColour.LogicalColour	-- Defines the side whose /turn/ is referenced.
	-> Turn x y
	-> Bool
isPawnDoubleAdvance logicalColour MkTurn {
	getRank			= Attribute.Rank.Pawn,
	getQualifiedMove	= qualifiedMove
} = Component.Move.isPawnDoubleAdvance logicalColour (Component.QualifiedMove.getMove qualifiedMove) && Component.QualifiedMove.getMoveType qualifiedMove == Data.Default.def
isPawnDoubleAdvance _ _	= False

-- | Forwards the request to 'Attribute.Rank.compareByLVA'.
compareByLVA
	:: Ord rankValue
	=> Attribute.Rank.EvaluateRank rankValue
	-> Turn x y
	-> Turn x y
	-> Ordering
compareByLVA evaluateRank MkTurn { getRank = rankL } MkTurn { getRank = rankR }	= Attribute.Rank.compareByLVA evaluateRank rankL rankR

{- |
	* Compares /turn/s by <https://www.chessprogramming.org/MVV-LVA>.

	* This orders the most valuable victim of an attack first, but when victims are of equal rank, orders the least valuable aggressor first.

	* N.B.: the order of non-capture moves (including promotions) isn't defined.

	* CAVEAT: no account is made for any defenders of the attacked piece, which might recoup transient gains.
-}
compareByMVVLVA
	:: Ord rankValue
	=> Attribute.Rank.EvaluateRank rankValue
	-> Turn x y
	-> Turn x y
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

