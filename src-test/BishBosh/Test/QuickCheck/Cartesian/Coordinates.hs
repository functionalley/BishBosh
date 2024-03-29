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

module BishBosh.Test.QuickCheck.Cartesian.Coordinates(
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Direction.Direction()
import			Control.Arrow((&&&))
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Vector		as Cartesian.Vector
import qualified	BishBosh.Direction.Direction		as Direction.Direction
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	BishBosh.Property.Orientated		as Property.Orientated
import qualified	BishBosh.Property.Reflectable		as Property.Reflectable
import qualified	BishBosh.Property.Rotatable		as Property.Rotatable
import qualified	Data.List
import qualified	Data.List.Extra
import qualified	Test.QuickCheck
import qualified	ToolShed.Test.Ix
import qualified	ToolShed.Test.ReversibleIO
import			Test.QuickCheck((==>))

instance Test.QuickCheck.Arbitrary Cartesian.Coordinates.Coordinates where
	arbitrary	= Test.QuickCheck.elements Property.FixedMembership.members

-- Check that one can interpolate between the coordinates.
isValidMove :: Cartesian.Coordinates.Coordinates -> Cartesian.Coordinates.Coordinates -> Bool
isValidMove source destination	= source /= destination && Property.Orientated.isStraight (Cartesian.Vector.measureDistance source destination)

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results = sequence [
	let
		f :: Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Coordinates.prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: String -> Test.QuickCheck.Property
		f garbage	= Test.QuickCheck.label "Coordinates.prop_read" $ case (
			reads garbage	:: [(Cartesian.Coordinates.Coordinates, String)]
		 ) of
			[_]	-> True
			_	-> True	-- Unless the read-implementation throws an exception.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Cartesian.Coordinates.Coordinates -> String -> Test.QuickCheck.Property
		f coordinates	= Test.QuickCheck.label "Coordinates.prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (const False) coordinates
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Coordinates.prop_ix" . ToolShed.Test.Ix.index
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f coordinates	= Test.QuickCheck.label "Coordinates.prop_enum" $ toEnum (fromEnum coordinates) == coordinates
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f	:: Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Coordinates.prop_reflectOnX" . uncurry (==) . (id &&& Property.Reflectable.reflectOnX . Property.Reflectable.reflectOnX)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Coordinates.prop_reflectOnY" . uncurry (==) . (id &&& Property.Reflectable.reflectOnY . Property.Reflectable.reflectOnY)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f coordinates	= Test.QuickCheck.label "Coordinates.prop_rotateIdentity" $ all ((== coordinates) . ($ coordinates)) [
			Property.Rotatable.rotate90 . Property.Rotatable.rotate90 . Property.Rotatable.rotate90 . Property.Rotatable.rotate90,
			Property.Rotatable.rotate180 . Property.Rotatable.rotate180,
			Property.Rotatable.rotate270 . Property.Rotatable.rotate270 . Property.Rotatable.rotate270 . Property.Rotatable.rotate270
		 ]
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f coordinates	= Test.QuickCheck.label "Coordinates.prop_rotate180/identity" . Data.List.Extra.allSame $ map ($ coordinates) [
			Property.Rotatable.rotate180,
			Property.Rotatable.rotate90 . Property.Rotatable.rotate90,
			Property.Rotatable.rotate270 . Property.Rotatable.rotate270
		 ]
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Coordinates.prop_getOpposite" . uncurry (==) . (Property.Opposable.getOpposite . Property.Opposable.getOpposite &&& id)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 16 } f,
	let
		f :: Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Coordinates.prop_getOpposite/Reflectable" . uncurry (==) . (Property.Opposable.getOpposite &&& Property.Reflectable.reflectOnY . Property.Reflectable.reflectOnX)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 16 } f,
	let
		f :: Direction.Direction.Direction -> Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f direction source	= Test.QuickCheck.label "Coordinates.prop_extrapolate" . (
			\extrapolation -> let
				destination
					| null extrapolation	= source
					| otherwise		= last extrapolation
			 in source == destination || source : extrapolation == reverse (
				destination : map fst {-Coordinates-} (
					Cartesian.Coordinates.interpolate destination source
				)
			 )
		 ) . map fst {-Coordinates-} $ Cartesian.Coordinates.extrapolate source direction
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Cartesian.Coordinates.Coordinates -> Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f source destination	= isValidMove source destination ==> Test.QuickCheck.label "Coordinates.prop_interpolate" $ source : map fst {-Coordinates-} (
			Cartesian.Coordinates.interpolate source destination
		 ) == reverse (
			destination : map fst {-Coordinates-} (
				Cartesian.Coordinates.interpolate destination source
			)
		 )
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: [Cartesian.Coordinates.Coordinates] -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Coordinates.prop_areSquaresIsochromatic" . uncurry (==) . (
			Cartesian.Coordinates.areSquaresIsochromatic &&& uncurry (||) . (
				null &&& (== 1) . length . Data.List.nub . map Cartesian.Coordinates.getLogicalColourOfSquare
			)
		 )
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Coordinates.prop_applyAlongDirectionsFrom" . uncurry (==) . (
			flip (Cartesian.Coordinates.applyAlongDirectionsFrom id) Nothing &&& flip (Cartesian.Coordinates.applyAlongDirectionsFrom id) (Just Property.FixedMembership.members)
		 )
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 16 } f,
	let
		f :: Cartesian.Coordinates.Coordinates -> Direction.Direction.Direction -> Test.QuickCheck.Property
		f source direction	= not (null qualifiedStraightLine) ==> let
			destination	= fst {-coordinates-} $ last qualifiedStraightLine
		 in Test.QuickCheck.label "Coordinates.prop_isBetween(terminals)" . not $ any (Cartesian.Coordinates.isBetween source destination) [source, destination] where
			qualifiedStraightLine	= Cartesian.Coordinates.extrapolate source direction
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 32 } f,
	let
		f :: Cartesian.Coordinates.Coordinates -> Direction.Direction.Direction -> Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f source direction intermediary	= not (null qualifiedStraightLine) ==> let
			destination	= fst {-coordinates-} $ last qualifiedStraightLine
		 in Test.QuickCheck.label "Coordinates.prop_isBetween(symmetrical)" . uncurry (==) $ (
			Cartesian.Coordinates.isBetween source destination &&& Cartesian.Coordinates.isBetween destination source
		 ) intermediary where
			qualifiedStraightLine	= Cartesian.Coordinates.extrapolate source direction
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f
 ]

