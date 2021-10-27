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
	notation,
--	xOriginOffset,
--	yOriginOffset,
	regexSyntax,
-- * Functions
	abscissaParser,
	ordinateParser,
	coordinatesParser,
-- ** Constructors
	mkPureCoordinate,
	mkPureCoordinate'
-- ** Predicates
) where

import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Data.Exception			as Data.Exception
import qualified	BishBosh.Notation.Notation		as Notation.Notation
import qualified	BishBosh.Type.Length			as Type.Length
import qualified	Control.Arrow
import qualified	Control.Exception
import qualified	Data.Char
import qualified	Data.List.Extra
import qualified	Data.Maybe

#ifdef USE_POLYPARSE
import qualified	BishBosh.Text.Poly			as Text.Poly
#	if USE_POLYPARSE == 'L'
import qualified	Text.ParserCombinators.Poly.Lazy	as Poly
#	elif USE_POLYPARSE == 'P'
import qualified	Text.ParserCombinators.Poly.Plain	as Poly
#	else
#		error "USE_POLYPARSE invalid"
#	endif
#else /* Parsec */
import qualified	Text.ParserCombinators.Parsec		as Parsec
import			Text.ParserCombinators.Parsec((<?>))
#endif

-- | Define the parameters of the notation, using the minimum permissible values for /x/ & /y/ coordinates.
notation :: Notation.Notation.Notation
notation	= Notation.Notation.mkNotation ('a', '1')

-- | The offset of the application's internal coordinate-system from this conventional one.
xOriginOffset :: Type.Length.X
yOriginOffset :: Type.Length.Y
(xOriginOffset, yOriginOffset)	= Notation.Notation.getOriginOffset notation

-- | Defines using a regex, the required syntax.
regexSyntax :: String
regexSyntax	= showString "([a-h][1-8]){2}[" $ showString (
	concatMap show Attribute.Rank.promotionProspects
 ) "]?"

#ifdef USE_POLYPARSE
-- | Parse an /x/-coordinate.
abscissaParser :: Text.Poly.TextParser Type.Length.X
abscissaParser	= ((+ xOriginOffset) . fromIntegral . Data.Char.ord) `fmap` Poly.satisfyMsg (Notation.Notation.inXRange notation) "Abscissa"

-- | Parse a /y/-coordinate.
ordinateParser :: Text.Poly.TextParser Type.Length.Y
ordinateParser	= ((+ yOriginOffset) . fromIntegral . Data.Char.ord) `fmap` Poly.satisfyMsg (Notation.Notation.inYRange notation) "Ordinate"

-- | Parse a pair of /coordinates/.
coordinatesParser :: Text.Poly.TextParser Cartesian.Coordinates.Coordinates
coordinatesParser	= do
	x	<- abscissaParser
	y	<- ordinateParser

	return {-to Parser-monad-} $ Cartesian.Coordinates.mkCoordinates x y
#else /* Parsec */
-- | Parse an /x/-coordinate.
abscissaParser :: Parsec.Parser Type.Length.X
abscissaParser	= (+ xOriginOffset) . fromIntegral . Data.Char.ord <$> Parsec.satisfy (Notation.Notation.inXRange notation) <?> "Abscissa"

-- | Parse a /y/-coordinate.
ordinateParser :: Parsec.Parser Type.Length.Y
ordinateParser	= (+ yOriginOffset) . fromIntegral . Data.Char.ord <$> Parsec.satisfy (Notation.Notation.inYRange notation) <?> "Ordinate"

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

instance Show PureCoordinate where
	showsPrec _ MkPureCoordinate {
		getMove			= move,
		getMaybePromotionRank	= maybePromotionRank
	} = Notation.Notation.showsCoordinates notation (
		Component.Move.getSource move
	 ) . Notation.Notation.showsCoordinates notation (
		Component.Move.getDestination move
	 ) . Data.Maybe.maybe id shows maybePromotionRank

-- N.B. this merely validates the syntax, leaving any semantic errors to 'Model.Game.validate'.
instance Read PureCoordinate where
	readsPrec _ s	= case Data.List.Extra.trimStart s of
		x : y : x' : y' : remainder	-> [
			Control.Arrow.first (
				mkPureCoordinate $ Component.Move.mkMove source destination
			) (
				case reads $ take 1 remainder of
					[(rank, "")]	-> if rank `elem` Attribute.Rank.promotionProspects
						then (Just rank, tail remainder)
						else (Nothing, remainder)
					_	-> (Nothing, remainder)
			) |
				let mkCoordinatesList	= Data.Maybe.maybeToList . Notation.Notation.mkMaybeCoordinates notation,
				source		<- mkCoordinatesList (x, y),
				destination	<- mkCoordinatesList (x', y'),
				source /= destination
		 ] -- List-comprehension.
		_					-> []	-- No parse.

instance Attribute.Rank.Promotable PureCoordinate where
	getMaybePromotionRank	= getMaybePromotionRank

