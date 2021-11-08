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

 [@DESCRIPTION@]	Sum the relative values of /rank/ of piece, based on their location on the board, at a specific stage in the game.
-}

module BishBosh.Component.Accountant(
-- * Type-class
	Accountant(..)
) where

import qualified	BishBosh.Component.PieceSquareByCoordinatesByRank	as Component.PieceSquareByCoordinatesByRank
import qualified	BishBosh.Type.Count					as Type.Count
import qualified	BishBosh.Type.Mass					as Type.Mass

-- | An interface which may be implemented by data which can total piece-square values.
class Accountant accountant where
	-- | Calculate the total value of the /coordinates/ occupied by the /piece/s of either side.
	sumPieceSquareValueByLogicalColour
		:: Component.PieceSquareByCoordinatesByRank.PieceSquareByCoordinatesByRank
		-> Type.Count.NPieces	-- ^ The number of pieces (of any logical colour or rank) remaining on the board; used to gauge progress through the game.
		-> accountant
		-> [Type.Mass.Base]	-- CAVEAT: can't return '[Type.Mass.PieceSquareValue]' because it's bounded.

