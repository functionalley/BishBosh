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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary'.
-}

module BishBosh.Test.QuickCheck.Notation.MoveNotation (
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Cartesian.Coordinates()
import			BishBosh.Test.QuickCheck.Model.Game()
import			Control.Arrow((&&&), (|||))
import qualified	BishBosh.Attribute.MoveType		as Attribute.MoveType
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Component.EitherQualifiedMove	as Component.EitherQualifiedMove
import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Component.Piece		as Component.Piece
import qualified	BishBosh.Component.QualifiedMove	as Component.QualifiedMove
import qualified	BishBosh.Component.Turn			as Component.Turn
import qualified	BishBosh.Model.Game			as Model.Game
import qualified	BishBosh.Notation.MoveNotation		as Notation.MoveNotation
import qualified	BishBosh.Notation.Notation		as Notation.Notation
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Property.Null			as Property.Null
import qualified	BishBosh.State.Board			as State.Board
import qualified	BishBosh.State.MaybePieceByCoordinates	as State.MaybePieceByCoordinates
import qualified	Data.Maybe
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

instance Test.QuickCheck.Arbitrary Notation.MoveNotation.MoveNotation where
	arbitrary	= Test.QuickCheck.elements Property.FixedMembership.members

results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Notation.MoveNotation.MoveNotation -> Cartesian.Coordinates.Coordinates -> Test.QuickCheck.Property
		f moveNotation	= Test.QuickCheck.label "MoveNotation.prop_(read.show)Coordinates" . uncurry (==) . (
			Notation.Notation.readsCoordinates (Notation.MoveNotation.getNotation moveNotation) . Notation.MoveNotation.showNotation moveNotation &&& return . flip (,) ""
		 )
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 32 } f,
	let
		f :: Notation.MoveNotation.MoveNotation -> Model.Game.Game -> Test.QuickCheck.Property
		f moveNotation game	= not (Property.Null.isNull game) ==> Test.QuickCheck.label "MoveNotation.prop_(read.show)QualifiedMove" $ case Notation.MoveNotation.readsQualifiedMove moveNotation $ Notation.MoveNotation.showNotation moveNotation turn of
			[(eitherQualifiedMove, "")]	-> uncurry (&&) $ (
				(== move) . Component.EitherQualifiedMove.getMove &&& (
					Data.Maybe.maybe (not $ Attribute.MoveType.isPromotion moveType) (== rank) ||| (== moveType)
				) . Component.EitherQualifiedMove.getPromotionRankOrMoveType
			 ) eitherQualifiedMove
			_				-> False
			where
				turn			= Data.Maybe.fromJust $ Model.Game.maybeLastTurn game
				(move, moveType)	= Component.QualifiedMove.getMove &&& Component.QualifiedMove.getMoveType $ Component.Turn.getQualifiedMove turn
				(Just rank)		= fmap Component.Piece.getRank . State.MaybePieceByCoordinates.dereference (Component.Move.getDestination move) . State.Board.getMaybePieceByCoordinates $ Model.Game.getBoard game

	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 1024 } f
 ]

