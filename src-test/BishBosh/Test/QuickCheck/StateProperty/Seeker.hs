{-
	Copyright (C) 2021 Dr. Alistair Ward

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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties.
-}

module BishBosh.Test.QuickCheck.StateProperty.Seeker(
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Model.Game()
import			Control.Arrow((&&&))
import			Data.Array.IArray((!))
import qualified	BishBosh.Attribute.Rank		as Attribute.Rank
import qualified	BishBosh.Cartesian.Coordinates	as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate	as Cartesian.Ordinate
import qualified	BishBosh.Component.Piece	as Component.Piece
import qualified	BishBosh.Model.Game		as Model.Game
import qualified	BishBosh.State.Board		as State.Board
import qualified	BishBosh.StateProperty.Seeker	as StateProperty.Seeker
import qualified	Test.QuickCheck

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Model.Game.Game -> Test.QuickCheck.Property
		f = Test.QuickCheck.label "Seeker.prop_(getNPawnsByFileByLogicalColour => countPawnsByFileByLogicalColour)" . uncurry (==) . (
			State.Board.getNPawnsByFileByLogicalColour &&& StateProperty.Seeker.countPawnsByFileByLogicalColour . State.Board.getCoordinatesByRankByLogicalColour
		 ) . Model.Game.getBoard
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Model.Game.Game -> Test.QuickCheck.Property
		f = Test.QuickCheck.label "Seeker.prop_countPawnsByFileByLogicalColour" . uncurry (==) . (
			StateProperty.Seeker.countPawnsByFileByLogicalColour . State.Board.getMaybePieceByCoordinates &&& StateProperty.Seeker.countPawnsByFileByLogicalColour . State.Board.getCoordinatesByRankByLogicalColour
		 ) . Model.Game.getBoard
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Model.Game.Game -> Test.QuickCheck.Property
		f = Test.QuickCheck.label "Seeker.prop_(summariseNPawnsByLogicalColour vs getNPiecesDifferenceByRank)" . uncurry (==) . (
			uncurry (-) . ((! maxBound) &&& (! minBound)) . StateProperty.Seeker.summariseNPawnsByLogicalColour &&& (! Attribute.Rank.Pawn) . State.Board.getNPiecesDifferenceByRank
		 ) . Model.Game.getBoard
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Model.Game.Game -> Test.QuickCheck.Property
		f game	= Test.QuickCheck.label "Seeker.prop_pawnOrdinates" . all (
			uncurry (&&) . (
				(/= Cartesian.Ordinate.yMin) &&& (/= Cartesian.Ordinate.yMax)
			) . Cartesian.Coordinates.getY . fst {-coordinates-}
		 ) . StateProperty.Seeker.findPieces Component.Piece.isPawn . State.Board.getCoordinatesByRankByLogicalColour $ Model.Game.getBoard game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f
 ]

