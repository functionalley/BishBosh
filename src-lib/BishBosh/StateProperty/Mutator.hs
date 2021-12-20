{-# LANGUAGE MultiParamTypeClasses #-}
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

 [@DESCRIPTION@]	Permits a board to be mutated.
-}

module BishBosh.StateProperty.Mutator(
-- * Type-classes
	Mutator(..),
-- * Functions
-- ** Mutators
	placePiece,
	placeFirstPiece,
	placeAllPieces,
	removePiece
) where

import qualified	BishBosh.Attribute.MoveType		as Attribute.MoveType
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Component.Piece		as Component.Piece
import qualified	BishBosh.Property.Empty			as Property.Empty

{- |
	* An interface which may be implemented by data which can mutate the board.

	* CAVEAT: doesn't validate requests, so @King@s can be placed /in check/ & @Pawn@s can be placed behind their starting rank or unpromoted on their last /rank/.
-}
class Mutator mutator where
	{- |
		* Defines the specified /coordinates/, by either placing or removing a /piece/.

		* CAVEAT: this function should only be used to construct custom scenarios, since /piece/s don't normally spring into existence.

		* CAVEAT: this function isn't called during normal play.
	-}
	defineCoordinates
		:: Maybe Component.Piece.Piece		-- ^ The optional /piece/ to place (or remove if @Nothing@ is specified).
		-> Cartesian.Coordinates.Coordinates	-- ^ The /coordinates/ to define.
		-> mutator
		-> mutator

	-- | Move the specified piece, accounting for promotions & casualties.
	movePiece
		:: Component.Move.Move
		-> Attribute.MoveType.MoveType
		-> Component.Piece.Piece	-- ^ The /piece/ which moved, rather than the (ptentially promoted) one that arrived.
		-> mutator
		-> mutator

{- |
	* Place a /piece/ at the specified /coordinates/.

	* CAVEAT: any /piece/ previously at the specified /coordinates/ will be obliterated.
-}
placePiece
	:: Mutator mutator
	=> Component.Piece.Piece
	-> Cartesian.Coordinates.Coordinates
	-> mutator
	-> mutator
placePiece piece	= defineCoordinates $ Just piece

-- | Place the first /piece/.
placeFirstPiece :: (
	Property.Empty.Empty	mutator,
	Mutator			mutator
 )
	=> Component.Piece.Piece
	-> Cartesian.Coordinates.Coordinates
	-> mutator
placeFirstPiece piece coordinates	= placePiece piece coordinates Property.Empty.empty

-- | Place /pieces/ from scratch.
placeAllPieces :: (
	Property.Empty.Empty	mutator,
	Mutator			mutator
 )
	=> [(Component.Piece.Piece, Cartesian.Coordinates.Coordinates)]
	-> mutator
placeAllPieces	= foldr (uncurry placePiece) Property.Empty.empty

-- | Remove a /piece/ from the /board/.
removePiece
	:: Mutator mutator
	=> Cartesian.Coordinates.Coordinates
	-> mutator
	-> mutator
removePiece	= defineCoordinates Nothing

