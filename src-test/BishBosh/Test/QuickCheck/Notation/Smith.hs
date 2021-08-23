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

module BishBosh.Test.QuickCheck.Notation.Smith(
-- * Types
-- ** Type-synonyms
--	Smith,
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Component.QualifiedMove()
import qualified	BishBosh.Component.Piece	as Component.Piece
import qualified	BishBosh.Notation.Smith		as Notation.Smith
import qualified	BishBosh.Types			as T
import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO

-- | Defines a concrete type for testing.
type Smith	= Notation.Smith.Smith T.X T.Y

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => Test.QuickCheck.Arbitrary (Notation.Smith.Smith x y) where
--	{-# SPECIALISE instance Test.QuickCheck.Arbitrary Smith #-}
	arbitrary	= fmap Notation.Smith.fromQualifiedMove Test.QuickCheck.arbitrary

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Smith -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Smith.prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 1024 } f,
	let
		f :: String -> Test.QuickCheck.Property
		f garbage	= Test.QuickCheck.label "Smith.prop_read" $ case (
			reads garbage :: [(Notation.Smith.Smith Int Int, String)]
		 ) of
			[_]	-> True
			_	-> True	-- Unless the read-implementation throws an exception.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Smith -> String -> Test.QuickCheck.Property
		f smith	= Test.QuickCheck.label "Smith.prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (`elem` ("cCE" ++ Component.Piece.showPieces)) smith
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 512 } f
 ]
