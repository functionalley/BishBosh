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

 [@DESCRIPTION@]	Categorises /move/s, & provides ancillary information as required.
-}

module BishBosh.Attribute.MoveType(
-- * Types
-- ** Type-synonyms
--	IsShort,
-- ** Data-types
	MoveType(
		Castle,
		EnPassant
--		Normal
	),
-- * Constants
	tag,
	shortCastle,
	longCastle,
	enPassant,
-- * Functions
	nPiecesMutator,
-- ** Constructors
	mkMaybeNormalMoveType,
	mkNormalMoveType,
-- ** Predicates
	isCastle,
	isEnPassant,
--	isNormal,
	isCapture,
	isPromotion,
	isQuiet,
	isSimple,
	isAcyclic,
-- ** Query
	getMaybeExplicitlyTakenRank,
	getMaybeImplicitlyTakenRank
) where

import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Text.ShowList			as Text.ShowList
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Default
import qualified	Data.List.Extra
import qualified	Data.Maybe

-- | Used to qualify output.
tag :: String
tag	= "moveType"

-- | Self-documentation.
type IsShort	= Bool

-- | Constant value required to denote a /short castle/.
shortCastle :: MoveType
shortCastle	= Castle True

-- | Constant value required to denote a /long castle/.
longCastle :: MoveType
longCastle	= Castle False

-- | Constant.
enPassant :: MoveType
enPassant	= EnPassant

-- | The sum-type of distinct types of /move/.
data MoveType
	= Castle IsShort	-- ^ Castling between the @King@ & one of its @Rook@s.
	| EnPassant		-- ^ Capture by a @Pawn@ of a @Pawn@ as it advanced two squares.
	| Normal {
		getMaybeTakenRank	:: Maybe Attribute.Rank.Rank,
		getMaybePromotionRank	:: Maybe Attribute.Rank.Rank
	}			-- ^ The /rank/ of any opposing /piece/ which was just taken & the /rank/ of any /piece/ to which a @Pawn@ was just promoted.
	deriving Eq

instance Show MoveType where
	showsPrec _ (Castle isShort)	= showString "Castle (short" . Text.ShowList.showsAssociation . shows isShort . showChar ')'
	showsPrec _ EnPassant		= showString "En-passant"
	showsPrec _ Normal {
		getMaybeTakenRank	= maybeTakenRank,
		getMaybePromotionRank	= maybePromotionRank
	} = Text.ShowList.showsAssociationList' $ Data.Maybe.catMaybes [
		fmap ((,) "takenRank" . shows) maybeTakenRank,
		fmap ((,) "promotionRank" . shows) maybePromotionRank
	 ]

instance Read MoveType where
	readsPrec _ s	= case Data.List.Extra.trimStart s of
		'C' : 'a' : 's' : 't' : 'l' : 'e' : s1	-> [
			(Castle isShort, remainder) |
				("(", s2)		<- lex s1,
				("short", s3)		<- lex s2,
				("=", s4)		<- lex s3,
				(isShort, s5)		<- reads s4,
				(")", remainder)	<- lex s5
		 ] -- List-comprehension.
		'E' : 'n' : '-' : 'p' : 'a' : 's' : 's' : 'a' : 'n' : 't' : remainder	-> [(EnPassant, remainder)]
		_ -> [
			(normalMoveType, remainder) |
				("{", s1)			<- lex s,
				(maybeTakenRank, s2)		<- case [
					pair |
						("takenRank", s11)	<- lex s1,
						("=", s12)		<- lex s11,
						pair			<- reads s12
				] of
					[]	-> [(Nothing, s1)]	-- Infer that nothing was taken.
					parsed	-> map (Control.Arrow.first Just) parsed,
				s3	<- return $ case lex s2 of
					[(",", s21)]	-> s21
					_		-> s2,
				(maybePromotionRank, s4)	<- case [
					pair |
						("promotionRank", s31)	<- lex s3,
						("=", s32)		<- lex s31,
						pair			<- reads s32
				] of
					[]	-> [(Nothing, s3)]	-- Infer that there was no promotion.
					parsed	-> map (Control.Arrow.first Just) parsed,
				("}", remainder)		<- lex s4,
				normalMoveType			<- Data.Maybe.maybeToList $ mkMaybeNormalMoveType maybeTakenRank maybePromotionRank
		 ] -- List-comprehension.

instance Control.DeepSeq.NFData MoveType where
	rnf (Castle isShort)	= Control.DeepSeq.rnf isShort
	rnf EnPassant		= ()
	rnf Normal {
		getMaybeTakenRank	= maybeTakenRank,
		getMaybePromotionRank	= maybePromotionRank
	}			= Control.DeepSeq.rnf (maybeTakenRank, maybePromotionRank)

instance Data.Default.Default MoveType where
	def	= Normal {
		getMaybeTakenRank	= Nothing,
		getMaybePromotionRank	= Nothing
	}

instance Attribute.Rank.Promotable MoveType where
	getMaybePromotionRank Normal { getMaybePromotionRank = maybePromotionRank }	= maybePromotionRank
	getMaybePromotionRank _								= Nothing

instance Property.FixedMembership.FixedMembership MoveType where
	members	= EnPassant : map Castle Property.FixedMembership.members ++ [
		Normal {
			getMaybeTakenRank	= maybeTakenRank,
			getMaybePromotionRank	= maybePromotionRank
		} |
			maybeTakenRank		<- Nothing : map Just Attribute.Rank.expendable,
			maybePromotionRank	<- Nothing : map Just Attribute.Rank.promotionProspects
	 ] -- List-comprehension.

-- | Smart constructor for normal move-types.
mkMaybeNormalMoveType
	:: Maybe Attribute.Rank.Rank	-- ^ The /rank/ of any opposing /piece/ which was just taken.
	-> Maybe Attribute.Rank.Rank	-- ^ The /rank/ to which a @Pawn@ was just promoted.
	-> Maybe MoveType		-- ^ Maybe the required /move-type/.
mkMaybeNormalMoveType maybeTakenRank maybePromotionRank
	| maybeTakenRank /= Just Attribute.Rank.King
	, Data.Maybe.maybe True {-nothing promoted-} (
		`elem` Attribute.Rank.promotionProspects
	) maybePromotionRank	= Just Normal {
		getMaybeTakenRank	= maybeTakenRank,
		getMaybePromotionRank	= maybePromotionRank
	}
	| otherwise		= Nothing

-- | Smart-constructor for normal move-types.
mkNormalMoveType
	:: Maybe Attribute.Rank.Rank	-- ^ The /rank/ of any opposing /piece/ which is to be taken.
	-> Maybe Attribute.Rank.Rank	-- ^ The /rank/ to which a @Pawn@ is to be promoted.
	-> MoveType
mkNormalMoveType maybeTakenRank maybePromotionRank	= Control.Exception.assert (
	maybeTakenRank /= Just Attribute.Rank.King && Data.Maybe.maybe True {-nothing promoted-} (
		`elem` Attribute.Rank.promotionProspects
	) maybePromotionRank
 ) Normal {
	getMaybeTakenRank	= maybeTakenRank,
	getMaybePromotionRank	= maybePromotionRank
}

-- | Predicate.
isCastle :: MoveType -> Bool
isCastle (Castle _)	= True
isCastle _		= False

-- | Predicate.
isEnPassant :: MoveType -> Bool
isEnPassant EnPassant	= True
isEnPassant _		= False

-- | Whether the /move/ was neither @EnPassant@ nor @Castle@.
isNormal :: MoveType -> Bool
isNormal (Normal _ _)	= True
isNormal _		= False

-- | Whether a piece was captured, including @Pawn@s taken En-passant.
isCapture :: MoveType -> Bool
{-# INLINE isCapture #-}
isCapture Normal { getMaybeTakenRank = Just _ }	= True
isCapture moveType				= isEnPassant moveType

-- | Whether the /move/ includes @Pawn@-promotion.
isPromotion :: MoveType -> Bool
isPromotion Normal { getMaybePromotionRank = Just _ }	= True
isPromotion _						= False

-- | <https://www.chessprogramming.org/Quiet_Moves>.
isQuiet :: MoveType -> Bool
isQuiet Normal {
	getMaybeTakenRank	= Nothing,
	getMaybePromotionRank	= Nothing
}			= True
isQuiet	moveType	= isCastle moveType

-- | The simplest type of move.
isSimple :: MoveType -> Bool
isSimple Normal {
	getMaybeTakenRank	= Nothing,
	getMaybePromotionRank	= Nothing
}		= True
isSimple _	= False	-- Neither Castling nor En-passant qualifies.

{- |
	* Whether the /move/ can't be a member of a repeated cycle.

	* CAVEAT: one can't infer from a negative result that the move can be repeated, since the mover may have been a @Pawn@.
-}
isAcyclic :: MoveType -> Bool
isAcyclic Normal {
	getMaybeTakenRank	= Nothing,
	getMaybePromotionRank	= Nothing
}		= False
isAcyclic _	= True

-- | Query whether a /piece/ was explicitly taken, excluding @Pawn@s taken En-passant.
getMaybeExplicitlyTakenRank :: MoveType -> Maybe Attribute.Rank.Rank
getMaybeExplicitlyTakenRank Normal { getMaybeTakenRank = maybeTakenRank }	= maybeTakenRank
getMaybeExplicitlyTakenRank _							= Nothing

-- | Query whether a /piece/ was taken either explicitly, or implicitly during En-passant.
getMaybeImplicitlyTakenRank :: MoveType -> Maybe Attribute.Rank.Rank
getMaybeImplicitlyTakenRank EnPassant	= Just Attribute.Rank.Pawn
getMaybeImplicitlyTakenRank moveType	= getMaybeExplicitlyTakenRank moveType

-- | Returns the mutator required to adjust the number of pieces after a move.
nPiecesMutator :: Enum nPieces => MoveType -> (nPieces -> nPieces)
{-# INLINE nPiecesMutator #-}
nPiecesMutator moveType
	| isCapture moveType	= pred
	| otherwise		= id

