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

module BishBosh.Test.QuickCheck.StateProperty.View(
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Model.Game()
import			Control.Arrow((&&&))
import qualified	BishBosh.Component.Piece			as Component.Piece
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.State.Board				as State.Board
import qualified	BishBosh.State.CoordinatesByRankByLogicalColour	as State.CoordinatesByRankByLogicalColour
import qualified	BishBosh.StateProperty.Seeker			as StateProperty.Seeker
import qualified	BishBosh.StateProperty.View			as StateProperty.View
import qualified	Data.List
import qualified	Test.QuickCheck

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Model.Game.Game -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "View.prop_toAssocs(CoordinatesByRankByLogicalColour == getMaybePieceByCoordinates)" . uncurry (==) . (
			normalise . State.Board.getCoordinatesByRankByLogicalColour &&& normalise . State.Board.getMaybePieceByCoordinates
		 ) . Model.Game.getBoard where
			normalise :: StateProperty.Seeker.Seeker seeker => seeker -> [Component.Piece.LocatedPiece]
			normalise	= Data.List.sortOn fst {-coordinates-} . StateProperty.View.toAssocs
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Model.Game.Game -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "View.prop_translate(MaybePieceByCoordinates)" . uncurry (==) . (
			id &&& StateProperty.View.translate {-to self-}
		 ) . State.Board.getMaybePieceByCoordinates . Model.Game.getBoard
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Model.Game.Game -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "View.prop_translate(CoordinatesByRankByLogicalColour)" . uncurry (==) . (
			State.CoordinatesByRankByLogicalColour.deconstruct &&& State.CoordinatesByRankByLogicalColour.deconstruct . StateProperty.View.translate {-to self-}
		 ) . State.CoordinatesByRankByLogicalColour.sortCoordinates . State.Board.getCoordinatesByRankByLogicalColour . Model.Game.getBoard
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f
 ]

