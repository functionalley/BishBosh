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

 [@DESCRIPTION@]

	* <https://chessprogramming.wikispaces.com/Algebraic+Chess+Notation#Pure coordinate notation>.

	* CAVEAT: <https://en.wikipedia.org/wiki/Chess_notation> defined a variant of this notation.

	* N.B.: used for communication via /CECP/ with /xboard/.
-}

module BishBosh.Notation.Coordinate(
-- * Types
-- ** Data-types
	Coordinate(
--		MkCoordinate,
		getMove
--		getMaybePromotionRank
	),
-- * Constants
--	xOrigin,
--	yOrigin,
	origin,
	regexSyntax,
-- * Functions
--	encode,
	showsCoordinates,
-- ** Constructors
	mkCoordinate,
	mkCoordinate'
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
regexSyntax	= showString "([a-h][1-8]){2}[" $ showString (
	concatMap show Attribute.Rank.promotionProspects
 ) "]?"

-- | Defines a /move/, to enable i/o in /Coordinate/-notation.
data Coordinate x y	= MkCoordinate {
	getMove			:: Component.Move.Move x y,
	getMaybePromotionRank	:: Maybe Attribute.Rank.Rank
} deriving Eq

-- | Smart constructor.
mkCoordinate :: Component.Move.Move x y -> Maybe Attribute.Rank.Rank -> Coordinate x y
mkCoordinate move maybePromotionRank
	| Just rank	<- maybePromotionRank
	, rank `notElem` Attribute.Rank.promotionProspects	= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Notation.Coordinate.Coordinate:\tcan't promote to a " $ shows rank "."
	| otherwise						= MkCoordinate {
		getMove			= move,
		getMaybePromotionRank	= maybePromotionRank
	}

-- | Smart constructor.
mkCoordinate' :: Attribute.Rank.Promotable promotable => Component.Move.Move x y -> promotable -> Coordinate x y
mkCoordinate' move	= mkCoordinate move . Attribute.Rank.getMaybePromotionRank

-- | Encodes the ordinate & abscissa.
encode :: (Enum x, Enum y) => Cartesian.Coordinates.Coordinates x y -> (ShowS, ShowS)
encode	= showChar . Data.Char.chr . (+ (xOrigin - Cartesian.Abscissa.xOrigin)) . fromEnum . Cartesian.Coordinates.getX &&& showChar . Data.Char.chr . (+ (yOrigin - Cartesian.Ordinate.yOrigin)) . fromEnum . Cartesian.Coordinates.getY

-- | Shows the specified /coordinates/.
showsCoordinates :: (Enum x, Enum y) => Cartesian.Coordinates.Coordinates x y -> ShowS
showsCoordinates	= uncurry (.) . encode

instance (Enum x, Enum y) => Show (Coordinate x y) where
	showsPrec _ MkCoordinate {
		getMove			= move,
		getMaybePromotionRank	= maybePromotionRank
	} = showsCoordinates (
		Component.Move.getSource move
	 ) . showsCoordinates (
		Component.Move.getDestination move
	 ) . Data.Maybe.maybe id shows maybePromotionRank

-- N.B. this merely validates the syntax, leaving any semantic errors to 'Model.Game.validate'.
instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => Read (Coordinate x y) where
	readsPrec _ s	= case Data.List.Extra.trimStart s of
		x : y : x' : y' : remainder	-> let
			translate x'' y''	= Cartesian.Coordinates.mkMaybeCoordinates (
				toEnum $ Data.Char.ord x'' + (Cartesian.Abscissa.xOrigin - xOrigin)
			 ) (
				toEnum $ Data.Char.ord y'' + (Cartesian.Ordinate.yOrigin - yOrigin)
			 )
		 in [
			Control.Arrow.first (
				mkCoordinate $ Component.Move.mkMove source destination
			) (
				case reads $ take 1 remainder of
					[(rank, "")]	-> if rank `elem` Attribute.Rank.promotionProspects
						then (Just rank, tail remainder)
						else (Nothing, remainder)
					_	-> (Nothing, remainder)
			) |
				source		<- Data.Maybe.maybeToList $ translate x y,
				destination	<- Data.Maybe.maybeToList $ translate x' y',
				source /= destination
		 ] -- List-comprehension.
		_					-> []	-- No parse.

instance Attribute.Rank.Promotable (Coordinate x y) where
	getMaybePromotionRank	= getMaybePromotionRank

