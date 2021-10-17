{-
	Copyright (C) 2021 Dr. Alistair Ward

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

	* The common basis for standard notations.

	* CAVEAT: this module is only concerned with the representation of /x/ & /y/ coordinates, not any additional move-feature like enpassant, promotion or castling.

-}

module BishBosh.Notation.Notation(
-- * Types
-- ** Type-synonyms
--	CoordinatePairC,
	CoordinatePairI,
-- ** Data-types
	Notation(
--		MkNotation,
		getMinC,
		getMaxC,
		getOrigin,
		getOriginOffset
	),
-- * Functions
	encode,
	mkMaybeCoordinates,
	showsCoordinates,
	readsCoordinates,
-- ** Constructors
	mkNotation,
-- ** Predicates
	inXRange,
	inYRange
--	inRange
) where

import			Control.Arrow((&&&), (***))
import qualified	BishBosh.Cartesian.Abscissa	as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates	as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate	as Cartesian.Ordinate
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Type.Length		as Type.Length
import qualified	Control.Exception
import qualified	Data.Char
import qualified	Data.List.Extra
import qualified	Data.Maybe

-- | The characters used in a standard notation to denote a 'CoordinatePairI'.
type CoordinatePairC	= (Char, Char)

-- | A numeric representation of some coordinates in the standard notation.
type CoordinatePairI	= (Type.Length.X, Type.Length.Y)

-- | The parameters defining a generic chess-notation.
data Notation	= MkNotation {
	getMinC		:: CoordinatePairC,	-- ^ The character which a standard notation uses to represent the minimum value on each axis.
	getMaxC		:: CoordinatePairC,	-- ^ The character which a standard notation uses to represent the maximum value on each axis.
	getOrigin	:: CoordinatePairI,	-- ^ The numeric pair corresponding to the character-pair.
	getOriginOffset	:: CoordinatePairI	-- ^ The offset of the application's internal coordinate-system from a standard one.
}

-- | Smart constructor.
mkNotation :: CoordinatePairC -> Notation
mkNotation pair
	| not . uncurry (&&) $ (ok *** ok) pair	= Control.Exception.throw $ Data.Exception.mkInvalidDatum "BishBosh.Notation.Notation.mkNotation:\tASCII character required."
	| otherwise				= notation
	where
		ok	= uncurry (&&) . (Data.Char.isAscii &&& Data.Char.isAlphaNum)

		notation	= MkNotation {
			getMinC		= pair,
			getMaxC		= (
				Data.Char.chr . fromIntegral . (
					+ pred {-fence-post-} Cartesian.Abscissa.xLength
				) *** Data.Char.chr . fromIntegral . (
					+ pred {-fence-post-} Cartesian.Ordinate.yLength
				)
			) $ getOrigin notation,
			getOrigin	= (
				fromIntegral . Data.Char.ord *** fromIntegral . Data.Char.ord
			) $ getMinC notation,
			getOriginOffset	= (
				(Cartesian.Abscissa.xMin -) *** (Cartesian.Ordinate.yMin -)
			) $ getOrigin notation
		}

-- | Whether the specified character falls within a standard notation's range.
inXRange :: Notation -> Char -> Bool
inXRange notation c	= uncurry (&&) $ ((<= c) . fst {-x-} . getMinC &&& (>= c) . fst {-x-} . getMaxC) notation

-- | Whether the specified character falls within a standard notation's range.
inYRange :: Notation -> Char -> Bool
inYRange notation c	= uncurry (&&) $ ((<= c) . snd {-y-} . getMinC &&& (>= c) . snd {-y-} . getMaxC) notation

-- | Whether the specified pair of characters fall within a standard notation's range.
inRange :: Notation -> CoordinatePairC -> Bool
inRange notation	= uncurry (&&) . (
	uncurry (&&) . (
		(>= xMinC) &&& (<= xMaxC)
	) *** uncurry (&&) . (
		(>= yMinC) &&& (<= yMaxC)
	)
 ) where
	((xMinC, yMinC), (xMaxC, yMaxC))	= getMinC &&& getMaxC $ notation

-- | Encodes the ordinate & abscissa.
encode :: Notation -> Cartesian.Coordinates.Coordinates -> (ShowS, ShowS)
encode notation	= showChar . Data.Char.chr . fromIntegral . subtract xOriginOffset . Cartesian.Coordinates.getX &&& showChar . Data.Char.chr . fromIntegral . subtract yOriginOffset . Cartesian.Coordinates.getY where
	(xOriginOffset, yOriginOffset)	= getOriginOffset notation

{- |
	* Attempt to construct coordinates from the specified characters.

	* A check that the specified characters are in the permissible bounds is performed before conversion incase the numeric type has a narrower range than 'Char'.
-}
mkMaybeCoordinates :: Notation -> CoordinatePairC -> Maybe Cartesian.Coordinates.Coordinates
mkMaybeCoordinates notation pair
	| inRange notation pair	= uncurry Cartesian.Coordinates.mkMaybeCoordinates $ (
		(+ xOriginOffset) . fromIntegral . Data.Char.ord *** (+ yOriginOffset) . fromIntegral . Data.Char.ord
	) pair
	| otherwise		= Nothing
	where
		(xOriginOffset, yOriginOffset)	= getOriginOffset notation

-- | Shows the specified /coordinates/.
showsCoordinates :: Notation -> Cartesian.Coordinates.Coordinates -> ShowS
showsCoordinates notation	= uncurry (.) . encode notation

-- | Reads coordinates.
readsCoordinates :: Notation -> ReadS Cartesian.Coordinates.Coordinates
readsCoordinates notation s	= case Data.List.Extra.trimStart s of
	x : y : remainder
		| inRange notation coordinatePairC	-> map (flip (,) remainder) . Data.Maybe.maybeToList $ mkMaybeCoordinates notation coordinatePairC
		| otherwise				-> []	-- No parse.
		where
			coordinatePairC	= (x, y)
	_	-> []	-- Mo parse.

