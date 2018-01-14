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

 [@DESCRIPTION@]	Permits maintenance of data collected during the search for a move.
-}

module BishBosh.Search.EphemeralData(
-- * Type-classes
	EphemeralData(..)
) where

import qualified	BishBosh.Component.Move	as Component.Move

-- | An interface for short-lived data.
class EphemeralData a where
	getSize		:: a -> Int				-- ^ Get the current size of the collection.
	euthanise	:: Component.Move.NPlies -> a -> a	-- ^ Prune old data from the collection.

