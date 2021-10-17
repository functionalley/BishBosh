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
	notation,
	regexSyntax,
	toRank,
-- * Functions
-- ** Constructors
	mkICCFNumeric,
	mkICCFNumeric'
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.Rank		as Attribute.Rank
import qualified	BishBosh.Component.Move		as Component.Move
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Notation.Notation	as Notation.Notation
import qualified	Control.Arrow
import qualified	Control.Exception
import qualified	Data.List.Extra
import qualified	Data.Maybe
import qualified	Data.Tuple

-- | Define the parameters of the notation, using the minimum permissible values for /x/ & /y/ coordinates.
notation :: Notation.Notation.Notation
notation	= Notation.Notation.mkNotation $ (id &&& id) '1'

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
data ICCFNumeric	= MkICCFNumeric {
	getMove			:: Component.Move.Move,
	getMaybePromotionRank	:: Maybe Attribute.Rank.Rank
} deriving Eq

-- | Smart constructor.
mkICCFNumeric
	:: Component.Move.Move
	-> Maybe Attribute.Rank.Rank	-- ^ The optional promotion-rank.
	-> ICCFNumeric
mkICCFNumeric move maybePromotionRank
	| Just rank	<- maybePromotionRank
	, rank `notElem` Attribute.Rank.promotionProspects	= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Notation.ICCFNumeric.mkICCFNumeric:\tcan't promote to a " $ shows rank "."
	| otherwise						= MkICCFNumeric {
		getMove			= move,
		getMaybePromotionRank	= maybePromotionRank
	}

-- | Smart constructor.
mkICCFNumeric'
	:: Attribute.Rank.Promotable promotable
	=> Component.Move.Move
	-> promotable	-- ^ The datum from which to extract the optional promotion-rank.
	-> ICCFNumeric
mkICCFNumeric' move	= mkICCFNumeric move . Attribute.Rank.getMaybePromotionRank

instance Show ICCFNumeric where
	showsPrec _ MkICCFNumeric {
		getMove			= move,
		getMaybePromotionRank	= maybePromotionRank
	} = Notation.Notation.showsCoordinates notation (
		Component.Move.getSource move
	 ) . Notation.Notation.showsCoordinates notation (
		Component.Move.getDestination move
	 ) . Data.Maybe.maybe id (
		shows . Data.Maybe.fromJust . (`lookup` map Data.Tuple.swap toRank)
	 ) maybePromotionRank

-- N.B. this merely validates the syntax, leaving any semantic errors to 'Model.Game.validate'.
instance Read ICCFNumeric where
	readsPrec _ s	= case Data.List.Extra.trimStart s of
		x : y : x' : y' : remainder	-> [
			Control.Arrow.first (
				mkICCFNumeric $ Component.Move.mkMove source destination
			) (
				case reads $ take 1 remainder of
					[(digit, _)]
						| Just rank <- lookup digit toRank	-> (Just rank, tail remainder)
						| otherwise				-> (Nothing, remainder)
					_						-> (Nothing, remainder)
			) |
				let mkCoordinatesList	= Data.Maybe.maybeToList . Notation.Notation.mkMaybeCoordinates notation,
				source		<- mkCoordinatesList (x, y),
				destination	<- mkCoordinatesList (x', y'),
				source /= destination
		 ] -- List-comprehension.
		_				-> []	-- No parse.

instance Attribute.Rank.Promotable ICCFNumeric where
	getMaybePromotionRank	= getMaybePromotionRank

