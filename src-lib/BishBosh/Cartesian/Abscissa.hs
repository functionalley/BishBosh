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

	* Describes the /x/-axis by which the /board/ is indexed.

	* N.B. this coordinate-system is for internal use only, and doesn't attempt to replicate any standard Chess-notation.
-}

module BishBosh.Cartesian.Abscissa(
-- * Types
	ByAbscissa,
-- * Constants
	xOrigin,
	xLength,
	xMin,
	xMax,
--	xBounds,
	xRange,
	centre,
-- * Functions
	reflect,
	translate,
	maybeTranslate,
	getAdjacents,
-- ** Constructors
	listArrayByAbscissa,
-- ** Predicates
	inBounds
) where

import qualified	BishBosh.Types	as T
import qualified	Control.Exception
import qualified	Data.Array.IArray

-- | The constant length of the /x/-axis.
xOrigin :: Int
xOrigin	= 0

-- | The constant length of the /x/-axis.
xLength :: T.Distance
xLength	= 8

-- | The constant lower bound of the abscissa.
xMin :: Enum x => x
xMin	= toEnum xOrigin

-- | The constant upper bound of the abscissa.
xMax :: Enum x => x
xMax	= toEnum $ xOrigin + fromIntegral (pred {-fence-post-} xLength)

-- | The constant bounds of the abscissa.
xBounds :: Enum x => (x, x)
xBounds	= (xMin, xMax)

-- | The constant list of abscissa.
xRange :: Enum x => [x]
xRange	= uncurry enumFromTo xBounds

{- |
	* The constant centre of the span.

	* CAVEAT: no square actually exists at this fractional value.
-}
centre :: Fractional centre => centre
centre	= fromIntegral (uncurry (+) xBounds :: T.X) / 2

-- | Reflects about the mid-point of the axis.
reflect :: Enum x => x -> x
reflect	= toEnum . (
	+ (2 * xOrigin + fromIntegral (pred xLength))
 ) . negate . fromEnum

-- | Predicate.
inBounds :: (Enum x, Ord x) => x -> Bool
{-# INLINE inBounds #-}
inBounds x	= x >= xMin && x <= xMax

-- | Translate the specified ordinate.
translate :: (Enum x, Ord x) => (x -> x) -> x -> x
translate transformation	= (\x -> Control.Exception.assert (inBounds x) x) . transformation

-- | Where legal, translate the specified abscissa.
maybeTranslate :: (Enum x, Ord x) => (x -> x) -> x -> Maybe x
maybeTranslate transformation	= (
	\x -> if inBounds x
		then Just x
		else Nothing
 ) . transformation

-- | Get the abscissae immediately left & right.
getAdjacents :: (Enum x, Eq x) => x -> [x]
{-# INLINE getAdjacents #-}
getAdjacents x
	| x == xMin	= [succ x]
	| x == xMax	= [pred x]
	| otherwise	= [pred x, succ x]

-- | A boxed array indexed by /coordinates/, of arbitrary elements.
type ByAbscissa x	= Data.Array.IArray.Array {-Boxed-} x

-- | Array-constructor.
listArrayByAbscissa :: (
	Data.Array.IArray.IArray	a e,
	Data.Array.IArray.Ix		x,
	Enum				x
 ) => [e] -> a x e
listArrayByAbscissa	= Data.Array.IArray.listArray xBounds

