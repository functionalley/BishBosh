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

module BishBosh.Test.QuickCheck.State.TurnsByLogicalColour(
-- * Types
-- ** Type-synonyms
--	TurnsByLogicalColour,
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Component.Turn()
import			Control.Arrow((&&&))
import qualified	BishBosh.Cartesian.Abscissa		as Cartesian.Abscissa
import qualified	BishBosh.Component.Piece		as Component.Piece
import qualified	BishBosh.Component.Turn			as Component.Turn
import qualified	BishBosh.Model.Game			as Model.Game
import qualified	BishBosh.State.TurnsByLogicalColour	as State.TurnsByLogicalColour
import qualified	BishBosh.Test.QuickCheck.Model.Game	as Test.QuickCheck.Model.Game
import qualified	BishBosh.Types				as T
import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO

-- | Define a concrete type for testing.
type TurnsByLogicalColour	= State.TurnsByLogicalColour.TurnsByLogicalColour (Component.Turn.Turn T.X T.Y)

instance (
	Show				a,
	Test.QuickCheck.Arbitrary	a
 ) => Test.QuickCheck.Arbitrary (State.TurnsByLogicalColour.TurnsByLogicalColour a) where
--	{-# SPECIALISE instance Test.QuickCheck.Arbitrary TurnsByLogicalColour #-}
	arbitrary	= do
		turns	<- Test.QuickCheck.arbitrary

		let (black, white)	= splitAt (length turns `div` 2) turns

		return {-to Gen-monad-} $ State.TurnsByLogicalColour.fromAssocs [(maxBound, white), (minBound, black)]

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: TurnsByLogicalColour -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "TurnsByLogicalColour.prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: String -> Test.QuickCheck.Property
		f garbage	= Test.QuickCheck.label "TurnsByLogicalColour.prop_read" $ case (reads garbage :: [(TurnsByLogicalColour, String)]) of
			[_]	-> True
			_	-> True	-- Unless the read-implementation throws an exception.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: TurnsByLogicalColour -> String -> Test.QuickCheck.Property
		f turnsByLogicalColour	= Test.QuickCheck.label "TurnsByLogicalColour.prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (`elem` ('/' : concatMap show Component.Piece.range ++ concatMap show [1 .. Cartesian.Abscissa.xLength])) turnsByLogicalColour
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 128 } f,
	let
		f :: Test.QuickCheck.Model.Game.Game -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "TurnsByLogicalColour.prop_getNPlies" . uncurry (==) . (State.TurnsByLogicalColour.getNPlies &&& State.TurnsByLogicalColour.countPlies) . Model.Game.getTurnsByLogicalColour
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f
 ]

