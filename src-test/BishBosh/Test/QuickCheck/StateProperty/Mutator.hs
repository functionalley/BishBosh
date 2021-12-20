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

module BishBosh.Test.QuickCheck.StateProperty.Mutator(
-- * Constants
	results,
-- * Functions
--	normalise
) where

import			BishBosh.Test.QuickCheck.Component.Piece()
import			BishBosh.Test.QuickCheck.Model.Game()
import			Control.Arrow((&&&))
import			Test.QuickCheck ((==>))
import qualified	BishBosh.Cartesian.Coordinates			as Cartesian.Coordinates
import qualified	BishBosh.Component.Move				as Component.Move
import qualified	BishBosh.Component.QualifiedMove		as Component.QualifiedMove
import qualified	BishBosh.Component.Piece			as Component.Piece
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.Property.Null				as Property.Null
import qualified	BishBosh.State.Board				as State.Board
import qualified	BishBosh.State.CoordinatesByRankByLogicalColour	as State.CoordinatesByRankByLogicalColour
import qualified	BishBosh.State.MaybePieceByCoordinates		as State.MaybePieceByCoordinates
import qualified	BishBosh.StateProperty.Mutator			as StateProperty.Mutator
import qualified	BishBosh.StateProperty.View			as StateProperty.View
import qualified	Data.Default
import qualified	Data.Maybe
import qualified	Test.QuickCheck

-- | Facilitate comparison.
normalise :: State.CoordinatesByRankByLogicalColour.CoordinatesByRankByLogicalColour -> State.CoordinatesByRankByLogicalColour.BareCoordinatesByRankByLogicalColour
normalise	= State.CoordinatesByRankByLogicalColour.deconstruct . State.CoordinatesByRankByLogicalColour.sortCoordinates

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Maybe Component.Piece.Piece -> Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f maybePiece coordinates = let
			defineCoordinates :: StateProperty.Mutator.Mutator mutator => mutator -> mutator
			defineCoordinates	= StateProperty.Mutator.defineCoordinates maybePiece coordinates
		 in Test.QuickCheck.label "Mutator.prop_defineCoordinates" . uncurry (==) $ (normalise . StateProperty.View.translate . defineCoordinates . State.Board.getMaybePieceByCoordinates &&& normalise . defineCoordinates . State.Board.getCoordinatesByRankByLogicalColour) Data.Default.def
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 128 } f,
	let
		f :: Model.Game.Game -> Test.QuickCheck.Property
		f game = not (
			uncurry (||) $ (Model.Game.isTerminated &&& Property.Null.isNull) game
		 ) ==> Test.QuickCheck.label "Mutator.prop_movePiece" . all (
			\qualifiedMove -> let
				(move, moveType)	= Component.QualifiedMove.getMove &&& Component.QualifiedMove.getMoveType $ qualifiedMove
				board			= Model.Game.getBoard game

				movePiece :: StateProperty.Mutator.Mutator mutator => mutator -> mutator
				movePiece	= StateProperty.Mutator.movePiece move moveType (
					Data.Maybe.fromJust . State.MaybePieceByCoordinates.dereference (State.Board.getMaybePieceByCoordinates board) . Component.Move.getSource $ Component.QualifiedMove.getMove qualifiedMove
				 )
			in uncurry (==) $ (
				normalise . StateProperty.View.translate . movePiece . State.Board.getMaybePieceByCoordinates &&& normalise . movePiece . State.Board.getCoordinatesByRankByLogicalColour
			) board
		 ) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 128 } f
 ]

