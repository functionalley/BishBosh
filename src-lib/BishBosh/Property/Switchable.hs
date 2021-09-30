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

 [@DESCRIPTION@]	Defines a class to which IO data-types which can be switched between binary states, can conform.
-}

module BishBosh.Property.Switchable(
-- * Type-classes
	Switchable(..),
-- * Functions
	flick
 ) where

import qualified	Data.Foldable

-- | For data which operate like binary switches.
class Switchable a where
	on	:: IO a		-- ^ Returns a switch in the /on/ state, regardless of any previous state.

	toggle	:: a -> IO a	-- ^ Switch the binary state.

	switchOff	:: a -> IO a	-- ^ Turn the switch off, which has no effect if already in the /off/ state.
	switchOff a
		| isOn a	= toggle a
		| otherwise	= return {-to IO-monad-} a

	isOn	:: a -> Bool	-- ^ Predicate: whether the switch is currently in the /on/ state.

	isOff	:: a -> Bool	-- ^ Predicate: whether the switch is currently in the /off/ state.
	isOff	= not . isOn

-- | Toggle the switch the specified number of times.
flick :: Switchable switchable => Int -> switchable -> IO switchable
flick n switchable	= Data.Foldable.foldrM ($) switchable $ replicate n toggle
