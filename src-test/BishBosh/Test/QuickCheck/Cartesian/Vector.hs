{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties.
-}

module BishBosh.Test.QuickCheck.Cartesian.Vector(
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Cartesian.Coordinates()
import			Control.Arrow((&&&), (***))
import qualified	BishBosh.Cartesian.Abscissa	as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates	as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate	as Cartesian.Ordinate
import qualified	BishBosh.Cartesian.Vector	as Cartesian.Vector
import qualified	BishBosh.Property.Opposable	as Property.Opposable
import qualified	BishBosh.Property.Orientated	as Property.Orientated
import qualified	BishBosh.Type.Length		as Type.Length
import qualified	Data.Maybe
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

#ifdef USE_NEWTYPE_WRAPPERS
import			BishBosh.Test.QuickCheck.Type.Length()
#endif

instance Test.QuickCheck.Arbitrary Cartesian.Vector.Vector where
	arbitrary	= do
		source		<- Test.QuickCheck.arbitrary :: Test.QuickCheck.Gen Cartesian.Coordinates.Coordinates
		destination	<- Test.QuickCheck.suchThat Test.QuickCheck.arbitrary (/= source)

		return {-to Gen-monad-} $ Cartesian.Vector.measureDistance source destination

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Cartesian.Vector.Vector -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Vector.prop_getOpposite" . uncurry (==) . (Property.Opposable.getOpposite . Property.Opposable.getOpposite &&& id)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Cartesian.Vector.Vector -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Vector.prop_orthogonal" . not . uncurry (&&) . (Property.Orientated.isDiagonal &&& Property.Orientated.isParallel)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Cartesian.Vector.Vector -> Test.QuickCheck.Property
		f vector	= not (Property.Orientated.isStraight vector) ==> Test.QuickCheck.label "Vector.prop_(not isStraight => not (isDiagonal || isParallel))" . not . uncurry (||) $ (Property.Orientated.isDiagonal &&& Property.Orientated.isParallel) vector
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Cartesian.Vector.Vector -> Test.QuickCheck.Property
		f vector	= Property.Orientated.isStraight vector ==> Test.QuickCheck.label "Vector.prop_(isStraight => isDiagonal /= isParallel)" . uncurry (/=) $ (Property.Orientated.isDiagonal &&& Property.Orientated.isParallel) vector
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 16 } f,
	let
		f :: Cartesian.Vector.Vector -> Test.QuickCheck.Property
		f vector	= Property.Orientated.isParallel vector ==> Test.QuickCheck.label "Vector.prop_(isParallel => isHorizontal /= isVertical)" . uncurry (/=) $ (Property.Orientated.isHorizontal &&& Property.Orientated.isVertical) vector
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 16 } f,
	let
		f :: (Type.Length.X, Type.Length.Y) -> Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f (distanceX, distanceY) coordinates	= Test.QuickCheck.label "Vector.prop_maybeTranslate" $ Cartesian.Coordinates.maybeTranslate (deltaX *** deltaY) coordinates == (
			Cartesian.Coordinates.maybeTranslateX deltaX coordinates >>= Cartesian.Coordinates.maybeTranslateY deltaY
		 ) where
			deltaX :: Type.Length.X -> Type.Length.X
			deltaX = (
				+ (
					(distanceX `mod` Cartesian.Abscissa.xLength) - (Cartesian.Abscissa.xLength `div` 2)
				)
			 ) -- Section.

			deltaY :: Type.Length.Y -> Type.Length.Y
			deltaY	= (
				+ (
					(distanceY `mod` Cartesian.Ordinate.yLength) - (Cartesian.Ordinate.yLength `div` 2)
				)
			 ) -- Section.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Cartesian.Coordinates.Coordinates -> Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f source destination = Test.QuickCheck.label "Vector.prop_measureDistance => translate" $ Cartesian.Vector.translate (
			Cartesian.Vector.measureDistance source destination
		 ) source == destination
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Cartesian.Coordinates.Coordinates -> Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f source destination = Test.QuickCheck.label "Vector.prop_translate" $ Cartesian.Vector.measureDistance source destination == Property.Opposable.getOpposite (
			Cartesian.Vector.measureDistance destination source
		 )
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Cartesian.Vector.Vector -> Test.QuickCheck.Property
		f = Test.QuickCheck.label "Vector.prop_toMaybeDirection" . uncurry (==) . (
			Data.Maybe.isJust . Cartesian.Vector.toMaybeDirection &&& Property.Orientated.isStraight
		 )
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f
 ]

