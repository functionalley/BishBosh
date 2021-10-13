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
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa		as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate		as Cartesian.Ordinate
import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Data.Enum			as Data.Enum
import qualified	BishBosh.Data.Exception			as Data.Exception
import qualified	BishBosh.Type.Length			as Type.Length
import qualified	Control.Arrow
import qualified	Control.Exception
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
origin	= fromEnum *** fromEnum $ min'

-- | The offset of the application's internal coordinate-system from this conventional one.
xOriginOffset, yOriginOffset :: Int
(xOriginOffset, yOriginOffset)	= (Cartesian.Abscissa.xOrigin -) *** (Cartesian.Ordinate.yOrigin -) $ origin

-- | The maximum permissible values for /x/ & /y/ coordinates.
xMax, yMax :: Char
(xMax, yMax)	= toEnum . (
	+ pred {-fence-post-} (fromIntegral Cartesian.Abscissa.xLength)
 ) *** toEnum . (
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
abscissaParser :: Text.Poly.TextParser Type.Length.X
abscissaParser	= Data.Enum.translate (+ xOriginOffset) `fmap` Poly.satisfyMsg inXRange "Abscissa"

-- | Parse a /y/-coordinate.
ordinateParser :: Text.Poly.TextParser Type.Length.Y
ordinateParser	= Data.Enum.translate (+ yOriginOffset) `fmap` Poly.satisfyMsg inYRange "Ordinate"

-- | Parse a pair of /coordinates/.
coordinatesParser :: Text.Poly.TextParser Cartesian.Coordinates.Coordinates
coordinatesParser	= do
	x	<- abscissaParser
	y	<- ordinateParser

	return {-to Parser-monad-} $ Cartesian.Coordinates.mkCoordinates x y
#else /* Parsec */
-- | Parse an /x/-coordinate.
abscissaParser :: Parsec.Parser Type.Length.X
abscissaParser	= Data.Enum.translate (+ xOriginOffset) <$> Parsec.satisfy inXRange <?> "Abscissa"

-- | Parse a /y/-coordinate.
ordinateParser :: Parsec.Parser Type.Length.Y
ordinateParser	= Data.Enum.translate (+ yOriginOffset) <$> Parsec.satisfy inYRange <?> "Ordinate"

-- | Parse a pair of /coordinates/.
coordinatesParser :: Parsec.Parser Cartesian.Coordinates.Coordinates
coordinatesParser	= Cartesian.Coordinates.mkCoordinates <$> abscissaParser <*> ordinateParser
#endif

-- | Defines a /move/, to enable i/o in /PureCoordinate/-notation.
data PureCoordinate	= MkPureCoordinate {
	getMove			:: Component.Move.Move,
	getMaybePromotionRank	:: Maybe Attribute.Rank.Rank
} deriving Eq

-- | Smart constructor.
mkPureCoordinate
	:: Component.Move.Move
	-> Maybe Attribute.Rank.Rank	-- ^ The optional promotion-rank.
	-> PureCoordinate
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
	=> Component.Move.Move
	-> promotable	-- ^ The datum from which to extract the optional promotion-rank.
	-> PureCoordinate
mkPureCoordinate' move	= mkPureCoordinate move . Attribute.Rank.getMaybePromotionRank

-- | Encodes the ordinate & abscissa.
encode :: Cartesian.Coordinates.Coordinates -> (ShowS, ShowS)
encode	= showChar . Data.Enum.translate (subtract xOriginOffset) . Cartesian.Coordinates.getX &&& showChar . Data.Enum.translate (subtract yOriginOffset) . Cartesian.Coordinates.getY

-- | Shows the specified /coordinates/.
showsCoordinates :: Cartesian.Coordinates.Coordinates -> ShowS
showsCoordinates	= uncurry (.) . encode

-- | Reads coordinates.
readsCoordinates :: ReadS Cartesian.Coordinates.Coordinates
readsCoordinates s	= case Data.List.Extra.trimStart s of
	x : y : remainder	-> map (
		flip (,) remainder
	 ) . Data.Maybe.maybeToList $ Cartesian.Coordinates.mkMaybeCoordinates (
		Data.Enum.translate (+ xOriginOffset) x
	 ) (
		Data.Enum.translate (+ yOriginOffset) y
	 )
	_			-> []	-- Mo parse.

instance Show PureCoordinate where
	showsPrec _ MkPureCoordinate {
		getMove			= move,
		getMaybePromotionRank	= maybePromotionRank
	} = showsCoordinates (
		Component.Move.getSource move
	 ) . showsCoordinates (
		Component.Move.getDestination move
	 ) . Data.Maybe.maybe id shows maybePromotionRank

-- N.B. this merely validates the syntax, leaving any semantic errors to 'Model.Game.validate'.
instance Read PureCoordinate where
	readsPrec _ s	= case Data.List.Extra.trimStart s of
		x : y : x' : y' : remainder	-> let
			fromPureCoordinate pair@(cx, cy)
				| not . uncurry (&&) $ (
					uncurry (&&) . (
						(cx >=) &&& (cx <) . Data.Enum.translate (+ fromEnum Cartesian.Abscissa.xLength)
					) . toEnum *** uncurry (&&) . (
						(cy >=) &&& (cy <) . Data.Enum.translate (+ fromEnum Cartesian.Ordinate.yLength)
					) . toEnum
				) origin	= Nothing
				| otherwise	= uncurry Cartesian.Coordinates.mkMaybeCoordinates $ (
					Data.Enum.translate (+ xOriginOffset) *** Data.Enum.translate (+ yOriginOffset)
				) pair
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
				source		<- Data.Maybe.maybeToList $ fromPureCoordinate (x, y),
				destination	<- Data.Maybe.maybeToList $ fromPureCoordinate (x', y'),
				source /= destination
		 ] -- List-comprehension.
		_					-> []	-- No parse.

instance Attribute.Rank.Promotable PureCoordinate where
	getMaybePromotionRank	= getMaybePromotionRank

