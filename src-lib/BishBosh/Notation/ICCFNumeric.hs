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

 [@DESCRIPTION@]	<https://en.wikipedia.org/wiki/ICCF_numeric_notation>.
-}

module BishBosh.Notation.ICCFNumeric(
-- * Types
-- ** Data-types
	ICCFNumeric(
--		MkICCFNumeric,
		getMove
--		getMaybePromotionRank
	),
-- * Constants
--	xOrigin,
--	yOrigin,
	origin,
	regexSyntax,
	toRank,
-- * Functions
--	encode,
	showsCoordinates,
-- ** Constructors
	mkICCFNumeric,
	mkICCFNumeric'
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.Rank		as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa	as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates	as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate	as Cartesian.Ordinate
import qualified	BishBosh.Component.Move		as Component.Move
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	Control.Arrow
import qualified	Control.Exception
import qualified	Data.Char
import qualified	Data.List.Extra
import qualified	Data.Maybe
import qualified	Data.Tuple

-- | The /x/-origin.
xOrigin :: Int
xOrigin	= Data.Char.ord '1'

-- | The /y/-origin.
yOrigin :: Int
yOrigin	= Data.Char.ord '1'

-- | The origin.
origin :: (Int, Int)
origin	= (xOrigin, yOrigin)

-- | Defines using a regex, the required syntax.
regexSyntax :: String
regexSyntax	= "[1-8]{4}[1-4]?"

-- | Constant translation from integral promotion-specifications to the corresponding /rank/.
toRank :: [(Int, Attribute.Rank.Rank)]
toRank	= zip [1 ..] [
	Attribute.Rank.Queen,
	Attribute.Rank.Rook,
	Attribute.Rank.Bishop,
	Attribute.Rank.Knight
 ]

-- | Defines a /move/, to enable i/o in /ICCF Numeric/-notation.
data ICCFNumeric x y	= MkICCFNumeric {
	getMove			:: Component.Move.Move x y,
	getMaybePromotionRank	:: Maybe Attribute.Rank.Rank
} deriving Eq

-- | Smart constructor.
mkICCFNumeric :: Component.Move.Move x y -> Maybe Attribute.Rank.Rank -> ICCFNumeric x y
mkICCFNumeric move maybePromotionRank
	| Just rank	<- maybePromotionRank
	, rank `notElem` Attribute.Rank.promotionProspects	= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Notation.ICCFNumeric.mkICCFNumeric:\tcan't promote to a " $ shows rank "."
	| otherwise						= MkICCFNumeric {
		getMove			= move,
		getMaybePromotionRank	= maybePromotionRank
	}

-- | Smart constructor.
mkICCFNumeric' :: Attribute.Rank.Promotable promotable => Component.Move.Move x y -> promotable -> ICCFNumeric x y
mkICCFNumeric' move	= mkICCFNumeric move . Attribute.Rank.getMaybePromotionRank

-- | Encodes the ordinate & abscissa.
encode :: (Enum x, Enum y) => Cartesian.Coordinates.Coordinates x y -> (ShowS, ShowS)
encode	= showChar . Data.Char.chr . (+ (xOrigin - Cartesian.Abscissa.xOrigin)) . fromEnum . Cartesian.Coordinates.getX &&& showChar . Data.Char.chr . (+ (yOrigin - Cartesian.Ordinate.yOrigin)) . fromEnum . Cartesian.Coordinates.getY

-- | Shows the specified /coordinates/.
showsCoordinates :: (Enum x, Enum y) => Cartesian.Coordinates.Coordinates x y -> ShowS
showsCoordinates	= uncurry (.) . encode

instance (Enum x, Enum y) => Show (ICCFNumeric x y) where
	showsPrec _ MkICCFNumeric {
		getMove			= move,
		getMaybePromotionRank	= maybePromotionRank
	} = showsCoordinates (
		Component.Move.getSource move
	 ) . showsCoordinates (
		Component.Move.getDestination move
	 ) . Data.Maybe.maybe id (
		shows . Data.Maybe.fromJust . (`lookup` map Data.Tuple.swap toRank)
	 ) maybePromotionRank

-- N.B. this merely validates the syntax, leaving any semantic errors to 'Model.Game.validate'.
instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => Read (ICCFNumeric x y) where
	readsPrec _ s	= case Data.List.Extra.trimStart s of
		x : y : x' : y' : remainder	-> let
			fromICCFNumeric x'' y''	= Cartesian.Coordinates.mkMaybeCoordinates (
				toEnum $ Data.Char.ord x'' + (Cartesian.Abscissa.xOrigin - xOrigin)
			 ) (
				toEnum $ Data.Char.ord y'' + (Cartesian.Ordinate.yOrigin - yOrigin)
			 )
		 in [
			Control.Arrow.first (
				mkICCFNumeric $ Component.Move.mkMove source destination
			) (
				case reads $ take 1 remainder of
					[(digit, _)]
						| Just rank <- lookup digit toRank	-> (Just rank, tail remainder)
						| otherwise				-> (Nothing, remainder)
					_						-> (Nothing, remainder)
			) |
				source		<- Data.Maybe.maybeToList $ fromICCFNumeric x y,
				destination	<- Data.Maybe.maybeToList $ fromICCFNumeric x' y',
				source /= destination
		 ] -- List-comprehension.
		_				-> []	-- No parse.

instance Attribute.Rank.Promotable (ICCFNumeric x y) where
	getMaybePromotionRank	= getMaybePromotionRank

