{-# LANGUAGE CPP, MagicHash #-}
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

	* AKA the /rank/ of a piece.

	* N.B. this coordinate-system is for internal use only, and doesn't attempt to replicate any standard chess-notation.
-}

module BishBosh.Cartesian.Ordinate(
-- * Constants
	yLength,
	yMin,
	yMax,
	yBounds,
	yRange,
-- * Functions
	toIx,
	fromIx,
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

import			Control.Arrow((&&&))
import qualified	BishBosh.Cartesian.Abscissa	as Cartesian.Abscissa
import qualified	BishBosh.Colour.LogicalColour	as Colour.LogicalColour
import qualified	BishBosh.Property.Opposable	as Property.Opposable
import qualified	BishBosh.Type.Length		as Type.Length
import qualified	Control.Exception

#if defined USE_UNBOXED && !(defined USE_NEWTYPE_WRAPPERS || defined USE_NARROW_NUMBERS)
import			GHC.Exts(Int(I#))
import			GHC.Prim((-#))
#endif

-- | The constant length of the /y/-axis.
yLength :: Type.Length.Y
yLength	= fromIntegral Cartesian.Abscissa.xLength	-- I.E.: the board is square.

-- | The constant bounds of ordinates.
yBounds :: (Type.Length.Y, Type.Length.Y)
yMin, yMax :: Type.Length.Y
yBounds@(yMin, yMax)	= (0, yMin + pred {-fence-post-} yLength)

-- | The constant list of all ordinates.
yRange :: [Type.Length.Y]
yRange	= uncurry enumFromTo yBounds

-- | Convert to an array-index.
toIx :: Type.Length.Y -> Int
toIx	= fromIntegral . subtract yMin

-- | Convert from an array-index.
fromIx :: Int -> Type.Length.Y
fromIx	= (+ yMin) . fromIntegral

-- | The /rank/ from which /piece/s conventionally start.
firstRank :: Colour.LogicalColour.LogicalColour -> Type.Length.Y
firstRank Colour.LogicalColour.Black	= yMax
firstRank _				= yMin

-- | The final /rank/; i.e. the one on which a @Pawn@ is promoted.
lastRank :: Colour.LogicalColour.LogicalColour -> Type.Length.Y
lastRank	= firstRank . Property.Opposable.getOpposite

-- | The /rank/ from which @Pawn@s conventionally start.
pawnsFirstRank :: Colour.LogicalColour.LogicalColour -> Type.Length.Y
{-# INLINE pawnsFirstRank #-}
pawnsFirstRank Colour.LogicalColour.Black	= pred yMax
pawnsFirstRank _				= succ yMin

-- | The /rank/ from which a @Pawn@ may capture /en-passant/.
enPassantRank :: Colour.LogicalColour.LogicalColour -> Type.Length.Y
{-# INLINE enPassantRank #-}
enPassantRank Colour.LogicalColour.Black	= fromIx 3
enPassantRank _					= fromIx 4

-- | Reflects about the mid-point of the axis.
reflect :: Type.Length.Y -> Type.Length.Y
#if defined USE_UNBOXED && !(defined USE_NEWTYPE_WRAPPERS || defined USE_NARROW_NUMBERS)
reflect (I# y)	= I# (7# -# y)	-- CAVEAT: hard-coded bounds.
#else
reflect	= (2 * yMin + yMax -)
#endif

-- | Predicate.
inBounds :: Type.Length.Y -> Bool
{-# INLINE inBounds #-}
inBounds	= uncurry (&&) . ((>= yMin) &&& (<= yMax))

-- | Translate the specified ordinate.
translate :: (Type.Length.Y -> Type.Length.Y) -> Type.Length.Y -> Type.Length.Y
translate transformation	= (\y -> Control.Exception.assert (inBounds y) y) . transformation

-- | Where legal, translate the specified ordinate.
maybeTranslate :: (Type.Length.Y -> Type.Length.Y) -> Type.Length.Y -> Maybe Type.Length.Y
maybeTranslate transformation	= (
	\y -> if inBounds y
		then Just y
		else Nothing
 ) . transformation

