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

 [@DESCRIPTION@]	Permits construction & destruction of views of the board.
-}

module BishBosh.StateProperty.View(
-- * Type-classes
	View(..),
-- * Functions
	toAssocs,
	translate
) where

import qualified	BishBosh.Component.Piece	as Component.Piece
import qualified	BishBosh.StateProperty.Seeker	as StateProperty.Seeker

-- | An interface which may be implemented by data which represent a view of the board.
class View view where
	-- | Constructor.
	fromAssocs	:: [Component.Piece.LocatedPiece] -> view

-- | Deconstructor.
toAssocs :: StateProperty.Seeker.Seeker seeker => seeker -> [Component.Piece.LocatedPiece]
toAssocs = StateProperty.Seeker.findAllPieces

-- | Convert between implementations.
translate :: (StateProperty.Seeker.Seeker seeker, View view) => seeker -> view
translate	= fromAssocs . toAssocs

