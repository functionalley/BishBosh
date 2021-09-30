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

import			Control.Arrow((&&&), (***))
import qualified	BishBosh.Cartesian.Abscissa			as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates			as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate			as Cartesian.Ordinate
import qualified	BishBosh.Cartesian.Vector			as Cartesian.Vector
import qualified	BishBosh.Property.Opposable			as Property.Opposable
import qualified	BishBosh.Property.Orientated			as Property.Orientated
import qualified	BishBosh.Test.QuickCheck.Cartesian.Coordinates	as Test.QuickCheck.Cartesian.Coordinates
import qualified	BishBosh.Type.Length				as Type.Length
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

instance (Num distance, Ord distance) => Test.QuickCheck.Arbitrary (Cartesian.Vector.Vector distance) where
--	{-# SPECIALISE instance Test.QuickCheck.Arbitrary Cartesian.Vector.VectorInt #-}
	arbitrary	= do
		source		<- Test.QuickCheck.arbitrary :: Test.QuickCheck.Gen Test.QuickCheck.Cartesian.Coordinates.Coordinates
		destination	<- Test.QuickCheck.suchThat Test.QuickCheck.arbitrary (/= source)

		return {-to Gen-monad-} $ Cartesian.Vector.measureDistance source destination

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Cartesian.Vector.VectorInt -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Vector.prop_getOpposite" . uncurry (==) . (Property.Opposable.getOpposite . Property.Opposable.getOpposite &&& id)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f	:: Cartesian.Vector.VectorInt -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Vector.prop_orthogonal" . not . uncurry (&&) . (Property.Orientated.isDiagonal &&& Property.Orientated.isParallel)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Cartesian.Vector.VectorInt -> Test.QuickCheck.Property
		f vector	= not (Property.Orientated.isStraight vector) ==> Test.QuickCheck.label "Vector.prop_straight" . not . uncurry (||) $ (Property.Orientated.isDiagonal &&& Property.Orientated.isParallel) vector
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: (Type.Length.Distance, Type.Length.Distance) -> Test.QuickCheck.Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f (distanceX, distanceY) coordinates	= Test.QuickCheck.label "Vector.prop_maybeTranslate" $ Cartesian.Coordinates.maybeTranslate (deltaX *** deltaY) coordinates == (
			Cartesian.Coordinates.maybeTranslateX deltaX coordinates >>= Cartesian.Coordinates.maybeTranslateY deltaY
		 ) where
			deltaX	:: Type.Length.X -> Type.Length.X
			deltaX = (
				+ fromIntegral (
					(distanceX `mod` Cartesian.Abscissa.xLength) - (Cartesian.Abscissa.xLength `div` 2)
				)
			 )

			deltaY	:: Type.Length.Y -> Type.Length.Y
			deltaY	= (
				+ fromIntegral (
					(distanceY `mod` Cartesian.Ordinate.yLength) - (Cartesian.Ordinate.yLength `div` 2)
				)
			 )
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Test.QuickCheck.Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f source destination = Test.QuickCheck.label "Vector.prop_measureDistance => translate" $ Cartesian.Vector.translate source (
			Cartesian.Vector.measureDistance source destination	:: Cartesian.Vector.VectorInt
		 ) == destination
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Test.QuickCheck.Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f source destination = Test.QuickCheck.label "Vector.prop_translate" $ Cartesian.Vector.measureDistance source destination == Property.Opposable.getOpposite (
			Cartesian.Vector.measureDistance destination source	:: Cartesian.Vector.VectorInt
		 )
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f
 ]

