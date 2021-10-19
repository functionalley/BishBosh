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

 [@DESCRIPTION@]	<https://www.chessprogramming.org/Warren_D._Smith>.
-}

module BishBosh.Notation.Smith(
-- * Types
-- ** Data-types
	Smith(
--		MkSmith,
		getQualifiedMove
	),
-- * Constants
	notation,
--	enpassantTag,
--	shortCastleTag,
--	longCastleTag,
	regexSyntax,
-- * Functions
-- ** Constructor
	fromQualifiedMove
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.MoveType		as Attribute.MoveType
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Component.QualifiedMove	as Component.QualifiedMove
import qualified	BishBosh.Notation.Notation		as Notation.Notation
import qualified	BishBosh.Notation.PureCoordinate	as Notation.PureCoordinate
import qualified	Control.Arrow
import qualified	Data.Char
import qualified	Data.Default
import qualified	Data.List.Extra
import qualified	Data.Maybe

-- | Define the parameters of the notation, using the minimum permissible values for /x/ & /y/ coordinates.
notation :: Notation.Notation.Notation
notation	= Notation.PureCoordinate.notation	-- CAVEAT: the encoding of coordinates is only coincidentally identical.

-- | Token.
enpassantTag :: Char
enpassantTag	= 'E'

-- | Token.
shortCastleTag :: Char
shortCastleTag	= 'c'

-- | Token.
longCastleTag :: Char
longCastleTag	= 'C'

-- | Defines using a regex, the required syntax.
regexSyntax :: String
regexSyntax	= showString "([a-h][1-8]){2}[" . showString (
	concatMap show Attribute.Rank.range
 ) . showChar enpassantTag . showChar shortCastleTag . showChar longCastleTag . showString "]?[" $ showString (
	Data.List.Extra.upper $ concatMap show Attribute.Rank.promotionProspects
 ) "]?"

-- | Defines a /move/, to enable i/o in /Smith/-notation.
newtype Smith	= MkSmith {
	getQualifiedMove	:: Component.QualifiedMove.QualifiedMove
} deriving Eq

-- | Constructor.
fromQualifiedMove :: Component.QualifiedMove.QualifiedMove -> Smith
fromQualifiedMove	= MkSmith

instance Show Smith where
	showsPrec _ MkSmith { getQualifiedMove = qualifiedMove }	= let
		(move, moveType)	= Component.QualifiedMove.getMove &&& Component.QualifiedMove.getMoveType $ qualifiedMove
	 in Notation.Notation.showsCoordinates notation (
		Component.Move.getSource move
	 ) . Notation.Notation.showsCoordinates notation (
		Component.Move.getDestination move
	 ) . (
		case moveType of
			Attribute.MoveType.Castle isShort	-> showChar $ if isShort
				then shortCastleTag
				else longCastleTag
			Attribute.MoveType.EnPassant		-> showChar enpassantTag
			_ {-normal-}				-> Data.Maybe.maybe id shows (
				Attribute.MoveType.getMaybeExplicitlyTakenRank moveType
			 ) . Data.Maybe.maybe id (
				showString . Data.List.Extra.upper . show
			 ) (
				Attribute.Rank.getMaybePromotionRank moveType
			 )
	 )

-- N.B. this merely validates the syntax, leaving any semantic errors to 'Model.Game.validate'.
instance Read Smith where
	readsPrec _ s	= case Data.List.Extra.trimStart s of
		x : y : x' : y' : remainder	-> [
			(
				fromQualifiedMove $ Component.QualifiedMove.mkQualifiedMove (Component.Move.mkMove source destination) moveType,
				remainder'
			) |
				let mkCoordinatesList	= Data.Maybe.maybeToList . Notation.Notation.mkMaybeCoordinates notation,
				source			<- mkCoordinatesList (x, y),
				destination		<- mkCoordinatesList (x', y'),
				source /= destination,
				(moveType, remainder')	<- case remainder of
					[]		-> [(Data.Default.def, remainder)]
					'c' : s1	-> [(Attribute.MoveType.shortCastle, s1)]
					'C' : s1	-> [(Attribute.MoveType.longCastle, s1)]
					'E' : s1	-> [(Attribute.MoveType.enPassant, s1)]
					c1 : s1		-> (
						\(moveType, remainder')	-> Data.Maybe.maybe [] {-no parse-} (
							return {-List-monad-} . flip (,) remainder'
						) $ uncurry Attribute.MoveType.mkMaybeNormalMoveType moveType
					 ) $ case reads [c1] of
						[(rank, "")]
							| Data.Char.isUpper c1 {-promotion-}	-> ((Nothing, Just rank), s1)
							| otherwise {-lower-case => capture-}	-> Control.Arrow.first (
								(,) $ Just rank
							) $ case s1 of
								c2 : s2
									| Data.Char.isUpper c2	-> case reads [c2] of
										[(promotionRank, "")]	-> (Just promotionRank, s2)
										_			-> (Nothing, s1)
									| otherwise		-> (Nothing, s1)
								[]	-> (Nothing, s1)
						_	-> ((Nothing, Nothing), remainder)
		 ] -- List-comprehension.
		_				-> []	-- No parse.

instance Attribute.Rank.Promotable Smith where
	getMaybePromotionRank MkSmith { getQualifiedMove = qualifiedMove }	= Attribute.Rank.getMaybePromotionRank $ Component.QualifiedMove.getMoveType qualifiedMove

