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

 [@DESCRIPTION@]

	* <https://www.chessprogramming.org/Forsyth-Edwards_Notation>.

	* <https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation>.
-}

module BishBosh.Property.ForsythEdwards(
-- * Type-classes
	ReadsFEN(..),
	ShowsFEN(..),
-- * Constants
	showsNullField,
	showsSeparator,
-- * Functions
	readFEN,
	showFEN
) where

-- | An alternative to 'Read'.
class ReadsFEN a where
	readsFEN	:: ReadS a	-- ^ Read a datum from FEN.

-- | An alternative to 'Show'.
class ShowsFEN a where
	showsFEN	:: a -> ShowS	-- ^ Stringify a FEN-datum.

-- | Read from FEN.
readFEN	:: ReadsFEN a => String -> a
readFEN s	= case readsFEN s of
	[(a, _)]	-> a
	_		-> error . showString "BishBosh.Property.ForsythEdwards.readFEN:\tfailed to parse " $ shows s "."

-- | Display in FEN.
showFEN	:: ShowsFEN a => a -> String
showFEN a	= showsFEN a ""

-- | The standard way to denote the absence of a field.
showsNullField :: ShowS
showsNullField	= showChar '-'

-- | The standard separator between fields in FEN.
showsSeparator :: ShowS
showsSeparator	= showChar ' '

