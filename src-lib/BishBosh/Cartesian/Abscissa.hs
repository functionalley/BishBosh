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

	* AKA the /file/ of a piece.

	* N.B. this coordinate-system is for internal use only, and doesn't attempt to replicate any standard chess-notation.
-}

module BishBosh.Cartesian.Abscissa(
-- * Constants
	xOrigin,
	xLength,
	xMin,
	xMax,
	xBounds,
	xRange,
	kingsFile,
--	adjacents,
-- * Functions
	toIx,
	fromIx,
	reflect,
	translate,
	maybeTranslate,
	getAdjacents,
--	getAdjacents',
-- ** Constructors
	listArrayByAbscissa,
-- ** Predicates
	inBounds
) where

import			Data.Array.IArray((!))
import qualified	BishBosh.Data.Enum	as Data.Enum
import qualified	BishBosh.Type.Length	as Type.Length
import qualified	Control.Exception
import qualified	Data.Array.IArray

-- | The position of the origin on the /x/-axis.
xOrigin :: Int
xOrigin	= 0

-- | The constant length of the /x/-axis.
xLength :: Type.Length.Distance
xLength	= 8

-- | The constant lower bound of abscissae.
xMin :: Type.Length.X
xMin	= toEnum xOrigin

-- | The constant upper bound of abscissae.
xMax :: Type.Length.X
xMax	= toEnum $ xOrigin + fromIntegral (pred {-fence-post-} xLength)

-- | The constant bounds of abscissae.
xBounds :: (Type.Length.X, Type.Length.X)
xBounds	= (xMin, xMax)

-- | The constant list of all abscissae.
xRange :: [Type.Length.X]
xRange	= uncurry enumFromTo xBounds

-- | The conventional starting /file/ for the @King@ of either /logical colour/.
kingsFile :: Type.Length.X
kingsFile	= fromIx 4

-- | Convert to an array-index.
toIx :: Type.Length.X -> Int
{-# INLINE toIx #-}
toIx	= subtract xOrigin . fromEnum

-- | Convert from an array-index.
fromIx :: Int -> Type.Length.X
{-# INLINE fromIx #-}
fromIx	= toEnum . (+ xOrigin)

-- | Reflects about the mid-point of the axis.
reflect :: Type.Length.X -> Type.Length.X
reflect	= Data.Enum.translate $ (
	+ (2 * xOrigin + fromIntegral (pred xLength))
 ) . negate

-- | Predicate.
inBounds :: Type.Length.X -> Bool
{-# INLINE inBounds #-}
inBounds x	= x >= xMin && x <= xMax

-- | Translate the specified ordinate.
translate :: (Type.Length.X -> Type.Length.X) -> Type.Length.X -> Type.Length.X
translate transformation	= (\x -> Control.Exception.assert (inBounds x) x) . transformation

-- | Where legal, translate the specified abscissa.
maybeTranslate :: (Type.Length.X -> Type.Length.X) -> Type.Length.X -> Maybe Type.Length.X
maybeTranslate transformation	= (
	\x -> if inBounds x
		then Just x
		else Nothing
 ) . transformation

-- | Get the abscissae immediately left & right.
getAdjacents' :: Type.Length.X -> [Type.Length.X]
getAdjacents' x
	| x == xMin	= [succ xMin]
	| x == xMax	= [pred xMax]
	| otherwise	= [pred x, succ x]

-- | The constant abscissae either side of each value.
adjacents :: Data.Array.IArray.Array Type.Length.X [Type.Length.X]
adjacents	= listArrayByAbscissa $ map getAdjacents' xRange

{- |
	* Get the abscissae immediately left & right.

	* N.B.: this is really a compile-time switch between alternative implementations.
-}
getAdjacents :: Type.Length.X -> [Type.Length.X]
getAdjacents	= (adjacents !)

-- | Array-constructor.
listArrayByAbscissa :: Data.Array.IArray.IArray a e => [e] -> a Type.Length.X e
listArrayByAbscissa	= Data.Array.IArray.listArray xBounds

