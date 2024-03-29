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

module BishBosh.Test.QuickCheck.Component.Move(
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Cartesian.Coordinates()
import			Control.Arrow((&&&))
import qualified	BishBosh.Component.Move		as Component.Move
import qualified	BishBosh.Property.Opposable	as Property.Opposable
import qualified	BishBosh.Property.Orientated	as Property.Orientated
import qualified	BishBosh.Property.Reflectable	as Property.Reflectable
import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO
import			Test.QuickCheck((==>))

instance Test.QuickCheck.Arbitrary Component.Move.Move where
	arbitrary	= fmap (uncurry Component.Move.mkMove) . Test.QuickCheck.suchThat Test.QuickCheck.arbitrary $ uncurry (/=)

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Component.Move.Move -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Move.prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: String -> Test.QuickCheck.Property
		f garbage	= Test.QuickCheck.label "Move.prop_read" $ case (reads garbage :: [(Component.Move.Move, String)]) of
			[_]	-> True
			_	-> True	-- Unless the read-implementation throws an exception.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Component.Move.Move -> String -> Test.QuickCheck.Property
		f move	= Test.QuickCheck.label "Move.prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (const False) move
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Component.Move.Move -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Move.prop_getOpposite" . uncurry (==) . (Property.Opposable.getOpposite . Property.Opposable.getOpposite &&& id)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f	:: Component.Move.Move -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Move.prop_orthogonal" . not . uncurry (&&) . (Property.Orientated.isDiagonal &&& Property.Orientated.isParallel)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Component.Move.Move -> Test.QuickCheck.Property
		f move	= not (Property.Orientated.isStraight move) ==> Test.QuickCheck.label "Move.prop_(not isStraight => not (isDiagonal || isParallel))" . not . uncurry (||) $ (Property.Orientated.isDiagonal &&& Property.Orientated.isParallel) move
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Component.Move.Move -> Test.QuickCheck.Property
		f move	= Property.Orientated.isStraight move ==> Test.QuickCheck.label "Move.prop_(isStraight => isDiagonal /= isParallel)" . uncurry (/=) $ (Property.Orientated.isDiagonal &&& Property.Orientated.isParallel) move
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 16 } f,
	let
		f :: Component.Move.Move -> Test.QuickCheck.Property
		f move	= Property.Orientated.isParallel move ==> Test.QuickCheck.label "Move.prop_(isParallel => isHorizontal /= isVertical)" . uncurry (/=) $ (Property.Orientated.isHorizontal &&& Property.Orientated.isVertical) move
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 16 } f,
	let
		f :: Component.Move.Move -> Test.QuickCheck.Property
		f move	= Test.QuickCheck.label "Move.prop_measureDistance" $ Component.Move.measureDistance (Property.Opposable.getOpposite move) == Property.Opposable.getOpposite (Component.Move.measureDistance move)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Component.Move.Move -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Move.prop_reflectOnX" . uncurry (==) . (id &&& Property.Reflectable.reflectOnX . Property.Reflectable.reflectOnX)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Component.Move.Move -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Move.prop_reflectOnY" . uncurry (==) . (id &&& Property.Reflectable.reflectOnY . Property.Reflectable.reflectOnY)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f
 ]

