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

module BishBosh.Test.QuickCheck.Direction.Direction(
-- * Constants
	results
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Direction.Direction		as Direction.Direction
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	BishBosh.Property.Orientated		as Property.Orientated
import qualified	BishBosh.Property.Reflectable		as Property.Reflectable
import qualified	BishBosh.Property.Rotatable		as Property.Rotatable
import qualified	Data.List.Extra
import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO
import			Test.QuickCheck((==>))

instance Test.QuickCheck.Arbitrary Direction.Direction.Direction where
	arbitrary	= Test.QuickCheck.elements Property.FixedMembership.members

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Direction.Direction.Direction -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Direction.prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: String -> Test.QuickCheck.Property
		f garbage	= Test.QuickCheck.label "Direction.prop_read" $ case (reads garbage :: [(Direction.Direction.Direction, String)]) of
			[_]	-> True
			_	-> True	-- Unless the read-implementation throws an exception.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Direction.Direction.Direction -> String -> Test.QuickCheck.Property
		f direction	= Test.QuickCheck.label "Direction.prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (`elem` "EW") direction
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 512 } f,
	let
		f	:: Direction.Direction.Direction -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Direction.prop_orthogonal" . not . uncurry (&&) . (Property.Orientated.isDiagonal &&& Property.Orientated.isParallel)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Direction.Direction.Direction -> Test.QuickCheck.Property
		f direction	= Property.Orientated.isDiagonal direction ==> Test.QuickCheck.label "Direction.prop_(isDiagonal => not (isHorizontal || isVertical))" . not . uncurry (||) $ (Property.Orientated.isHorizontal &&& Property.Orientated.isVertical) direction
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 16 } f,
	let
		f :: Direction.Direction.Direction -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Direction.prop_(isDiagonal /= isParallel)" . uncurry (/=) . (Property.Orientated.isDiagonal &&& Property.Orientated.isParallel)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 16 } f,
	let
		f :: Direction.Direction.Direction -> Test.QuickCheck.Property
		f direction	= Property.Orientated.isParallel direction ==> Test.QuickCheck.label "Direction.prop_(isParallel => isHorizontal /= isVertical)" . uncurry (/=) $ (Property.Orientated.isHorizontal &&& Property.Orientated.isVertical) direction
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 16 } f,
	let
		f :: Direction.Direction.Direction -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Direction.prop_reflectOnX" . uncurry (==) . (id &&& Property.Reflectable.reflectOnX . Property.Reflectable.reflectOnX)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 16 } f,
	let
		f :: Direction.Direction.Direction -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Direction.prop_reflectOnY" . uncurry (==) . (id &&& Property.Reflectable.reflectOnY . Property.Reflectable.reflectOnY)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 16 } f,
	let
		f :: Direction.Direction.Direction -> Test.QuickCheck.Property
		f direction	= Test.QuickCheck.label "Direction.prop_rotateIdentity" $ all ((== direction) . ($ direction)) [
			Property.Rotatable.rotate90 . Property.Rotatable.rotate90 . Property.Rotatable.rotate90 . Property.Rotatable.rotate90,
			Property.Rotatable.rotate180 . Property.Rotatable.rotate180,
			Property.Rotatable.rotate270 . Property.Rotatable.rotate270 . Property.Rotatable.rotate270 . Property.Rotatable.rotate270
		 ]
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Direction.Direction.Direction -> Test.QuickCheck.Property
		f direction	= Test.QuickCheck.label "Direction.prop_rotate180/identity" . Data.List.Extra.allSame $ map ($ direction) [
			Property.Rotatable.rotate180,
			Property.Rotatable.rotate90 . Property.Rotatable.rotate90,
			Property.Rotatable.rotate270 . Property.Rotatable.rotate270
		 ]
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Direction.Direction.Direction -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Direction.prop_getOpposite" . uncurry (==) . (Property.Opposable.getOpposite . Property.Opposable.getOpposite &&& id)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 16 } f,
	let
		f :: Direction.Direction.Direction -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Direction.prop_getOpposite/Reflectable" . uncurry (==) . (Property.Opposable.getOpposite &&& Property.Reflectable.reflectOnY . Property.Reflectable.reflectOnX)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 16 } f,
	let
		f :: Direction.Direction.Direction -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Direction.prop_areAligned" . uncurry Direction.Direction.areAligned . (id &&& Property.Opposable.getOpposite)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 16 } f
 ]

