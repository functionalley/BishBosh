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

module BishBosh.Test.QuickCheck.Component.QualifiedMove(
-- * Types
-- ** Type-synonyms
--	QualifiedMove,
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Attribute.MoveType()
import			BishBosh.Test.QuickCheck.Component.Move()
import			Control.Arrow((&&&))
import qualified	BishBosh.Component.QualifiedMove	as Component.QualifiedMove
import qualified	BishBosh.Property.Reflectable		as Property.Reflectable
import qualified	BishBosh.Type.Length			as Type.Length
import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO

-- | Defines a concrete type for testing.
type QualifiedMove	= Component.QualifiedMove.QualifiedMove Type.Length.X Type.Length.Y

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => Test.QuickCheck.Arbitrary (Component.QualifiedMove.QualifiedMove x y) where
--	{-# SPECIALISE instance Test.QuickCheck.Arbitrary QualifiedMove #-}
	arbitrary	= uncurry Component.QualifiedMove.mkQualifiedMove `fmap` Test.QuickCheck.arbitrary	-- CAVEAT: the move & moveType are unlikely to be compatible.

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: QualifiedMove -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "QualifiedMove.prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: String -> Test.QuickCheck.Property
		f garbage	= Test.QuickCheck.label "QualifiedMove.prop_read" $ case (reads garbage :: [(QualifiedMove, String)]) of
			[_]	-> True
			_	-> True	-- Unless the read-implementation throws an exception.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: QualifiedMove -> String -> Test.QuickCheck.Property
		f qualifiedMove	= Test.QuickCheck.label "QualifiedMove.prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (const False) qualifiedMove
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: QualifiedMove -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "QualifiedMove.prop_reflectOnX" . uncurry (==) . (id &&& Property.Reflectable.reflectOnX . Property.Reflectable.reflectOnX)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f
 ]
