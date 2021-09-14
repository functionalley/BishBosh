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

 [@DESCRIPTION@]	Controls the growth of dynamic data collected during the search for a move.
-}

module BishBosh.Search.EphemeralData(
-- * Type-classes
	EphemeralData(..),
	MaybeEphemeralData(..)
) where

import qualified	BishBosh.Input.SearchOptions	as Input.SearchOptions
import qualified	BishBosh.Type.Count		as Type.Count
import			Prelude(Int)

-- | An interface for short-lived data.
class EphemeralData a where
	getSize		:: a -> Int			-- ^ Get the current size of the collection.
	euthanise	:: Type.Count.NPlies -> a -> a	-- ^ Prune items older than the specified number of plies, from the data.

-- | For data which can be killed.
class MaybeEphemeralData a where
	maybeEuthanise
		:: Type.Count.NPlies				-- ^ The age at which to die.
		-> Input.SearchOptions.MaybeRetireAfterNMoves	-- ^ The optional age at which to retire killer-moves.
		-> Input.SearchOptions.MaybeRetireAfterNMoves	-- ^ The optional age at which to retire transpositions.
		-> a
		-> a
