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

	* Describes the /y/-axis by which the /board/ is indexed.

	* N.B. this coordinate-system is for internal use only, and doesn't attempt to replicate any standard Chess-notation.
-}

module BishBosh.Cartesian.Ordinate(
-- * Constants
	yOrigin,
	yLength,
	yMin,
	yMax,
--	yBounds,
	yRange,
	centre,
-- * Functions
	firstRank,
	lastRank,
	pawnsFirstRank,
	enPassantRank,
	reflect,
	translate,
	maybeTranslate,
-- ** Predicates
	inBounds
) where

import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Cartesian.Abscissa		as Cartesian.Abscissa
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	BishBosh.Types				as T
import qualified	Control.Exception

-- | The constant length of the /y/-axis.
yOrigin :: Int
yOrigin	= Cartesian.Abscissa.xOrigin	-- N.B. it doesn't need to the same.

-- | The constant length of the /y/-axis.
yLength :: T.Distance
yLength	= Cartesian.Abscissa.xLength	-- Because the board's square.

-- | The constant lower bound of the ordinate.
yMin :: Enum y => y
yMin	= toEnum yOrigin

-- | The constant upper bound of the ordinate.
yMax :: Enum y => y
yMax	= toEnum $ yOrigin + fromIntegral (pred {-fence-post-} yLength)

-- | The constant bounds of the ordinate.
yBounds :: Enum y => (y, y)
yBounds	= (yMin, yMax)

-- | The constant list of ordinates.
yRange :: Enum y => [y]
yRange	= uncurry enumFromTo yBounds

{- |
	* The constant centre of the span.

	* CAVEAT: no square actually exists at this fractional value.
-}
centre :: Fractional centre => centre
centre	= fromIntegral (uncurry (+) yBounds :: T.Y) / 2

-- | The /rank/ from which /piece/s conventionally start.
firstRank :: Enum y => Attribute.LogicalColour.LogicalColour -> y
firstRank Attribute.LogicalColour.Black	= yMax
firstRank _				= yMin

-- | The final /rank/; i.e. the one on which a @Pawn@ is promoted.
lastRank :: Enum y => Attribute.LogicalColour.LogicalColour -> y
lastRank	= firstRank . Property.Opposable.getOpposite

-- | The /rank/ from which @Pawn@s conventionally start.
pawnsFirstRank :: Enum y => Attribute.LogicalColour.LogicalColour -> y
{-# INLINE pawnsFirstRank #-}
pawnsFirstRank Attribute.LogicalColour.Black	= pred yMax
pawnsFirstRank _				= succ yMin

-- | The /rank/ from which a @Pawn@ may capture /en-passant/.
enPassantRank :: Enum y => Attribute.LogicalColour.LogicalColour -> y
{-# INLINE enPassantRank #-}
enPassantRank Attribute.LogicalColour.Black	= toEnum $ yOrigin + 3
enPassantRank _					= toEnum $ yOrigin + 4

-- | Reflects about the mid-point of the axis.
reflect :: Enum y => y -> y
reflect	= toEnum . (
	+ (2 * yOrigin + fromIntegral (pred yLength))
 ) . negate . fromEnum

-- | Predicate.
inBounds :: (Enum y, Ord y) => y -> Bool
{-# INLINE inBounds #-}
inBounds y	= y >= yMin && y <= yMax

-- | Translate the specified ordinate.
translate :: (Enum y, Ord y) => (y -> y) -> y -> y
translate transformation	= (\y -> Control.Exception.assert (inBounds y) y) . transformation

-- | Where legal, translate the specified ordinate.
maybeTranslate :: (Enum y, Ord y) => (y -> y) -> y -> Maybe y
maybeTranslate transformation	= (
	\y -> if inBounds y
		then Just y
		else Nothing
 ) . transformation

