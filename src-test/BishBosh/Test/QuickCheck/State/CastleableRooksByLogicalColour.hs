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

module BishBosh.Test.QuickCheck.State.CastleableRooksByLogicalColour(
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Cartesian.Coordinates()
import			BishBosh.Test.QuickCheck.Colour.LogicalColour()
import			Control.Arrow((&&&))
import qualified	BishBosh.Cartesian.Abscissa			as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates			as Cartesian.Coordinates
import qualified	BishBosh.Colour.LogicalColour			as Colour.LogicalColour
import qualified	BishBosh.Property.FixedMembership		as Property.FixedMembership
import qualified	BishBosh.Property.ForsythEdwards		as Property.ForsythEdwards
import qualified	BishBosh.Property.Reflectable			as Property.Reflectable
import qualified	BishBosh.State.CastleableRooksByLogicalColour	as State.CastleableRooksByLogicalColour
import qualified	Data.List
import qualified	Data.List.Extra
import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO
import			Test.QuickCheck((==>))

instance Test.QuickCheck.Arbitrary State.CastleableRooksByLogicalColour.CastleableRooksByLogicalColour where
	arbitrary	= fmap (
		State.CastleableRooksByLogicalColour.fromAssocs . Data.List.Extra.groupSort
	 ) . Test.QuickCheck.elements $ Data.List.subsequences [
		(logicalColour, x) |
			logicalColour	<- Property.FixedMembership.members,
			x		<- [Cartesian.Abscissa.xMin, Cartesian.Abscissa.xMax]
	 ] -- List-comprehension.

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: State.CastleableRooksByLogicalColour.CastleableRooksByLogicalColour -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "CastleableRooksByLogicalColour.prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: String -> Test.QuickCheck.Property
		f garbage	= Test.QuickCheck.label "CastleableRooksByLogicalColour.prop_read" $ case (reads garbage :: [(State.CastleableRooksByLogicalColour.CastleableRooksByLogicalColour, String)]) of
			[_]	-> True
			_	-> True	-- Unless the read-implementation throws an exception.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: State.CastleableRooksByLogicalColour.CastleableRooksByLogicalColour -> String -> Test.QuickCheck.Property
		f castleableRooksByLogicalColour	= Test.QuickCheck.label "CastleableRooksByLogicalColour.prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (const False) castleableRooksByLogicalColour
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: State.CastleableRooksByLogicalColour.CastleableRooksByLogicalColour -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "CastleableRooksByLogicalColour.prop_reflectOnX" . uncurry (==) . (id &&& Property.Reflectable.reflectOnX . Property.Reflectable.reflectOnX)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 32 } f,
	let
		f :: State.CastleableRooksByLogicalColour.CastleableRooksByLogicalColour -> Test.QuickCheck.Property
		f = Test.QuickCheck.label "CastleableRooksByLogicalColour.prop_fen" . uncurry (==) . (
			State.CastleableRooksByLogicalColour.unify . Property.ForsythEdwards.readFEN . Property.ForsythEdwards.showFEN &&& State.CastleableRooksByLogicalColour.unify
		 )
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 32 } f,
	let
		f :: State.CastleableRooksByLogicalColour.CastleableRooksByLogicalColour -> Test.QuickCheck.Property
		f castleableRooksByLogicalColour = Test.QuickCheck.label "CastleableRooksByLogicalColour.prop_(canCastle /= hasCastled)" . not $ any (
			uncurry (&&) . (
				State.CastleableRooksByLogicalColour.canCastle castleableRooksByLogicalColour &&& State.CastleableRooksByLogicalColour.hasCastled castleableRooksByLogicalColour
			)
		 ) Property.FixedMembership.members
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 32 } f,
	let
		f :: Colour.LogicalColour.LogicalColour -> Cartesian.Coordinates.Coordinates -> State.CastleableRooksByLogicalColour.CastleableRooksByLogicalColour -> Test.QuickCheck.Property
		f logicalColour coordinates castleableRooksByLogicalColour = coordinates `notElem` [
			minBound,
			Cartesian.Coordinates.bottomRight,
			Cartesian.Coordinates.topLeft,
			maxBound
		 ] ==> Test.QuickCheck.label "CastleableRooksByLogicalColour.prop_canCastleWith" . not $ State.CastleableRooksByLogicalColour.canCastleWith castleableRooksByLogicalColour logicalColour coordinates
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64} f,
	let
		f :: Colour.LogicalColour.LogicalColour -> Cartesian.Coordinates.Coordinates -> State.CastleableRooksByLogicalColour.CastleableRooksByLogicalColour -> Test.QuickCheck.Property
		f logicalColour coordinates castleableRooksByLogicalColour = State.CastleableRooksByLogicalColour.canCastleWith castleableRooksByLogicalColour logicalColour coordinates ==> Test.QuickCheck.label "CastleableRooksByLogicalColour.prop_(canCastleWith => canCastle)" $ State.CastleableRooksByLogicalColour.canCastle castleableRooksByLogicalColour logicalColour
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {
		Test.QuickCheck.maxSuccess	= 64,
		Test.QuickCheck.maxDiscardRatio	= 128
	} f,
	let
		f :: Colour.LogicalColour.LogicalColour -> Cartesian.Coordinates.Coordinates -> State.CastleableRooksByLogicalColour.CastleableRooksByLogicalColour -> Test.QuickCheck.Property
		f logicalColour coordinates castleableRooksByLogicalColour = not (State.CastleableRooksByLogicalColour.canCastle castleableRooksByLogicalColour logicalColour) ==> Test.QuickCheck.label "CastleableRooksByLogicalColour.prop_(not . canCastle => not . canCastleWith)" . not $ State.CastleableRooksByLogicalColour.canCastleWith castleableRooksByLogicalColour logicalColour coordinates
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f
 ]

