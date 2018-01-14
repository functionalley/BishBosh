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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties.
-}

module BishBosh.Test.QuickCheck.State.Position(
-- * Constants
	results
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.MoveType		as Attribute.MoveType
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Component.QualifiedMove	as Component.QualifiedMove
import qualified	BishBosh.Model.Game			as Model.Game
import qualified	BishBosh.State.EnPassantAbscissa	as State.EnPassantAbscissa
import qualified	BishBosh.State.Position			as State.Position
import qualified	BishBosh.Test.QuickCheck.Model.Game	as Test.QuickCheck.Model.Game
import qualified	Data.List
import qualified	Test.QuickCheck

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Test.QuickCheck.Model.Game.Game -> Test.QuickCheck.Property
		f = Test.QuickCheck.label "Position.prop_getMaybeEnPassantAbscissa" . uncurry (==) . (
			fmap (
				Cartesian.Coordinates.getX . Component.Move.getDestination . Component.QualifiedMove.getMove	-- Extract the ordinate.
			) . Data.List.find (
				Attribute.MoveType.isEnPassant . Component.QualifiedMove.getMoveType
			) . Model.Game.findQualifiedMovesAvailableToNextPlayer &&& fmap State.EnPassantAbscissa.getAbscissa . State.Position.getMaybeEnPassantAbscissa . Model.Game.mkPosition
		 )
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 1024 } f
 ]

