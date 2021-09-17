{-# LANGUAGE CPP #-}
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

	* <https://www.chessprogramming.org/Algebraic_Chess_Notation#Pure_coordinate_notation>.

	* CAVEAT: <https://en.wikipedia.org/wiki/Chess_notation> defined a variant of this notation.

	* N.B.: used for communication via /CECP/ with /xboard/.

	* N.B.: this minimal notation defines the coordinate-system on which Standard Algebraic is based.
-}

module BishBosh.Notation.PureCoordinate(
-- * Types
-- ** Data-types
	PureCoordinate(
--		MkPureCoordinate,
		getMove
--		getMaybePromotionRank
	),
-- * Constants
--	min',
--	xMin,
--	xMax,
--	yMin,
--	yMax,
	origin,
--	xOriginOffset,
--	yOriginOffset,
	regexSyntax,
-- * Functions
	encode,
	showsCoordinates,
	readsCoordinates,
	abscissaParser,
	ordinateParser,
	coordinatesParser,
-- ** Constructors
	mkPureCoordinate,
	mkPureCoordinate'
-- ** Predicates
--	inXRange,
--	inYRange
) where

import			Control.Arrow((&&&), (***))
import qualified	BishBosh.Attribute.Rank		as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa	as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates	as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate	as Cartesian.Ordinate
import qualified	BishBosh.Component.Move		as Component.Move
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Type.Length		as Type.Length
import qualified	Control.Arrow
import qualified	Control.Exception
import qualified	Data.Char
import qualified	Data.List.Extra
import qualified	Data.Maybe

#ifdef USE_POLYPARSE
import qualified	BishBosh.Text.Poly			as Text.Poly
#if USE_POLYPARSE == 1
import qualified	Text.ParserCombinators.Poly.Lazy	as Poly
#else /* Plain */
import qualified	Text.ParserCombinators.Poly.Plain	as Poly
#endif
#else /* Parsec */
import qualified	Text.ParserCombinators.Parsec		as Parsec
import			Text.ParserCombinators.Parsec((<?>))
#endif

-- | The minimum permissible values for /x/ & /y/ coordinates.
min' :: (Char, Char)
xMin, yMin :: Char
min'@(xMin, yMin)	= ('a', '1')

-- | The origin of the coordinate-system.
origin :: (Int, Int)
origin	= Data.Char.ord *** Data.Char.ord $ min'

-- | The offset of the application's internal coordinate-system from this conventional one.
xOriginOffset, yOriginOffset :: Int
(xOriginOffset, yOriginOffset)	= (Cartesian.Abscissa.xOrigin -) *** (Cartesian.Ordinate.yOrigin -) $ origin

-- | The maximum permissible values for /x/ & /y/ coordinates.
xMax, yMax :: Char
(xMax, yMax)	= Data.Char.chr . (
	+ pred {-fence-post-} (fromIntegral Cartesian.Abscissa.xLength)
 ) *** Data.Char.chr . (
	+ pred {-fence-post-} (fromIntegral Cartesian.Ordinate.yLength)
 ) $ origin

-- | Whether the specified character is a valid abscissa.
inXRange :: Char -> Bool
inXRange	= uncurry (&&) . ((>= xMin) &&& (<= xMax))

-- | Whether the specified character is a valid ordinate.
inYRange :: Char -> Bool
inYRange	= uncurry (&&) . ((>= yMin) &&& (<= yMax))

-- | Defines using a regex, the required syntax.
regexSyntax :: String
regexSyntax	= showString "([a-h][1-8]){2}[" $ showString (
	concatMap show Attribute.Rank.promotionProspects
 ) "]?"

#ifdef USE_POLYPARSE
-- | Parse an /x/-coordinate.
abscissaParser :: Enum x => Text.Poly.TextParser x
{-# SPECIALISE abscissaParser :: Text.Poly.TextParser Type.Length.X #-}
abscissaParser	= (
	toEnum . (+ xOriginOffset) . Data.Char.ord
 ) `fmap` Poly.satisfyMsg inXRange "Abscissa"

-- | Parse a /y/-coordinate.
ordinateParser :: Enum y => Text.Poly.TextParser y
{-# SPECIALISE ordinateParser :: Text.Poly.TextParser Type.Length.Y #-}
ordinateParser	= (
	toEnum . (+ yOriginOffset) . Data.Char.ord
 ) `fmap` Poly.satisfyMsg inYRange "Ordinate"

-- | Parse a pair of /coordinates/.
coordinatesParser :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => Text.Poly.TextParser (Cartesian.Coordinates.Coordinates x y)
{-# SPECIALISE coordinatesParser :: Text.Poly.TextParser (Cartesian.Coordinates.Coordinates Type.Length.X Type.Length.Y) #-}
coordinatesParser	= do
	x	<- abscissaParser
	y	<- ordinateParser

	return {-to Parser-monad-} $ Cartesian.Coordinates.mkCoordinates x y
#else /* Parsec */
-- | Parse an /x/-coordinate.
abscissaParser :: Enum x => Parsec.Parser x
{-# SPECIALISE abscissaParser :: Parsec.Parser Type.Length.X #-}
abscissaParser	= toEnum . (+ xOriginOffset) . Data.Char.ord <$> Parsec.satisfy inXRange <?> "Abscissa"

-- | Parse a /y/-coordinate.
ordinateParser :: Enum y => Parsec.Parser y
{-# SPECIALISE ordinateParser :: Parsec.Parser Type.Length.X #-}
ordinateParser	= toEnum . (+ yOriginOffset) . Data.Char.ord <$> Parsec.satisfy inYRange <?> "Ordinate"

-- | Parse a pair of /coordinates/.
coordinatesParser :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => Parsec.Parser (Cartesian.Coordinates.Coordinates x y)
{-# SPECIALISE coordinatesParser :: Parsec.Parser (Cartesian.Coordinates.Coordinates Type.Length.X Type.Length.Y) #-}
coordinatesParser	= Cartesian.Coordinates.mkCoordinates <$> abscissaParser <*> ordinateParser
#endif

-- | Defines a /move/, to enable i/o in /PureCoordinate/-notation.
data PureCoordinate x y	= MkPureCoordinate {
	getMove			:: Component.Move.Move x y,
	getMaybePromotionRank	:: Maybe Attribute.Rank.Rank
} deriving Eq

-- | Smart constructor.
mkPureCoordinate
	:: Component.Move.Move x y
	-> Maybe Attribute.Rank.Rank	-- ^ The optional promotion-rank.
	-> PureCoordinate x y
mkPureCoordinate move maybePromotionRank
	| Just rank	<- maybePromotionRank
	, rank `notElem` Attribute.Rank.promotionProspects	= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Notation.PureCoordinate.mkPureCoordinate:\tcan't promote to a " $ shows rank "."
	| otherwise						= MkPureCoordinate {
		getMove			= move,
		getMaybePromotionRank	= maybePromotionRank
	}

-- | Smart constructor.
mkPureCoordinate'
	:: Attribute.Rank.Promotable promotable
	=> Component.Move.Move x y
	-> promotable	-- ^ The datum from which to extract the optional promotion-rank.
	-> PureCoordinate x y
mkPureCoordinate' move	= mkPureCoordinate move . Attribute.Rank.getMaybePromotionRank

-- | Encodes the ordinate & abscissa.
encode :: (Enum x, Enum y) => Cartesian.Coordinates.Coordinates x y -> (ShowS, ShowS)
encode	= showChar . Data.Char.chr . subtract xOriginOffset . fromEnum . Cartesian.Coordinates.getX &&& showChar . Data.Char.chr . subtract yOriginOffset . fromEnum . Cartesian.Coordinates.getY

-- | Shows the specified /coordinates/.
showsCoordinates :: (Enum x, Enum y) => Cartesian.Coordinates.Coordinates x y -> ShowS
showsCoordinates	= uncurry (.) . encode

-- | Reads coordinates.
readsCoordinates :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => ReadS (Cartesian.Coordinates.Coordinates x y)
readsCoordinates s	= case Data.List.Extra.trimStart s of
	x : y : remainder	-> map (
		flip (,) remainder
	 ) . Data.Maybe.maybeToList $ Cartesian.Coordinates.mkMaybeCoordinates (
		toEnum $ Data.Char.ord x + xOriginOffset
	 ) (
		toEnum $ Data.Char.ord y + yOriginOffset
	 )
	_			-> []	-- Mo parse.

instance (Enum x, Enum y) => Show (PureCoordinate x y) where
	showsPrec _ MkPureCoordinate {
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
 ) => Read (PureCoordinate x y) where
	readsPrec _ s	= case Data.List.Extra.trimStart s of
		x : y : x' : y' : remainder	-> let
			translate x'' y''	= Cartesian.Coordinates.mkMaybeCoordinates (
				toEnum $ Data.Char.ord x'' + xOriginOffset
			 ) (
				toEnum $ Data.Char.ord y'' + yOriginOffset
			 )
		 in [
			Control.Arrow.first (
				mkPureCoordinate $ Component.Move.mkMove source destination
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

instance Attribute.Rank.Promotable (PureCoordinate x y) where
	getMaybePromotionRank	= getMaybePromotionRank

