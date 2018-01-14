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

 [@DESCRIPTION@]	<https://www.chessclub.com/user/chessviewer/smith.html Smith-notation>
-}

module BishBosh.Notation.Smith(
-- * Types
-- ** Data-types
	Smith(
--		MkSmith,
		getQualifiedMove
	),
-- * Constants
--	xOrigin,
--	yOrigin,
	origin,
	regexSyntax,
-- * Functions
--	encode,
	showsCoordinates,
-- ** Constructor
	fromQualifiedMove
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.MoveType		as Attribute.MoveType
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa		as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate		as Cartesian.Ordinate
import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Component.QualifiedMove	as Component.QualifiedMove
import qualified	Control.Arrow
import qualified	Data.Char
import qualified	Data.Default
import qualified	Data.List.Extra
import qualified	Data.Maybe

-- | The /x/-origin.
xOrigin :: Int
xOrigin	= Data.Char.ord 'a'

-- | The /y/-origin.
yOrigin :: Int
yOrigin	= Data.Char.ord '1'

-- | The origin.
origin :: (Int, Int)
origin	= (xOrigin, yOrigin)

-- | Defines using a regex, the required syntax.
regexSyntax :: String
regexSyntax	= showString "([a-h][1-8]){2}[" . showString (
	concatMap show Attribute.Rank.range
 ) . showString "EcC]?[" $ showString (
	Data.List.Extra.upper $ map (head . show) Attribute.Rank.promotionProspects
 ) "]?"

-- | Defines a /move/, to enable i/o in /Smith/-notation.
newtype Smith x y	= MkSmith {
	getQualifiedMove	:: Component.QualifiedMove.QualifiedMove x y
} deriving Eq

-- | Constructor.
fromQualifiedMove :: Component.QualifiedMove.QualifiedMove x y -> Smith x y
fromQualifiedMove	= MkSmith

-- | Encodes the ordinate & abscissa.
encode :: (Enum x, Enum y) => Cartesian.Coordinates.Coordinates x y -> (ShowS, ShowS)
encode	= showChar . Data.Char.chr . (+ (xOrigin - Cartesian.Abscissa.xOrigin)) . fromEnum . Cartesian.Coordinates.getX &&& showChar . Data.Char.chr . (+ (yOrigin - Cartesian.Ordinate.yOrigin)) . fromEnum . Cartesian.Coordinates.getY

-- | Shows the specified /coordinates/.
showsCoordinates :: (Enum x, Enum y) => Cartesian.Coordinates.Coordinates x y -> ShowS
showsCoordinates	= uncurry (.) . encode

instance (Enum x, Enum y) => Show (Smith x y) where
	showsPrec _ MkSmith { getQualifiedMove = qualifiedMove }	= let
		(move, moveType)	= Component.QualifiedMove.getMove &&& Component.QualifiedMove.getMoveType $ qualifiedMove
	 in showsCoordinates (
		Component.Move.getSource move
	 ) . showsCoordinates (
		Component.Move.getDestination move
	 ) . (
		case moveType of
			Attribute.MoveType.Castle isShort	-> showChar $ if isShort
				then 'c'
				else 'C'
			Attribute.MoveType.EnPassant		-> showChar 'E'
			_ {-normal-}				-> Data.Maybe.maybe id shows (
				Attribute.MoveType.getMaybeExplicitlyTakenRank moveType
			 ) . Data.Maybe.maybe id (
				showString . Data.List.Extra.upper . show
			 ) (
				Attribute.Rank.getMaybePromotionRank moveType
			 )
	 )

-- N.B. this merely validates the syntax, leaving any semantic errors to 'Model.Game.validate'.
instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => Read (Smith x y) where
	readsPrec _ s	= case Data.List.Extra.trimStart s of
		x : y : x' : y' : remainder	-> let
			fromSmith x'' y''	= Cartesian.Coordinates.mkMaybeCoordinates (
				toEnum $ Data.Char.ord x'' + (Cartesian.Abscissa.xOrigin - xOrigin)
			 ) (
				toEnum $ Data.Char.ord y'' + (Cartesian.Ordinate.yOrigin - yOrigin)
			 )
		 in [
			(
				fromQualifiedMove $ Component.QualifiedMove.mkQualifiedMove (Component.Move.mkMove source destination) moveType,
				remainder'
			) |
				source			<- Data.Maybe.maybeToList $ fromSmith x y,
				destination		<- Data.Maybe.maybeToList $ fromSmith x' y',
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

instance Attribute.Rank.Promotable (Smith x y) where
	getMaybePromotionRank MkSmith { getQualifiedMove = qualifiedMove }	= Attribute.Rank.getMaybePromotionRank $ Component.QualifiedMove.getMoveType qualifiedMove

