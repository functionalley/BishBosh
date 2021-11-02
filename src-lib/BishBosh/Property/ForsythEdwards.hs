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
-- * Types
-- ** Type-synonyms
--	FEN,
-- * Type-classes
	ReadsFEN(..),
	ShowsFEN(..),
-- * Functions
	readFEN,
	showFEN
) where

import qualified	BishBosh.Property.ExtendedPositionDescription	as Property.ExtendedPositionDescription

-- | An alternative to 'Read'.
class Property.ExtendedPositionDescription.ReadsEPD a => ReadsFEN a where
	readsFEN	:: ReadS a	-- ^ Read a datum from FEN.
	readsFEN	= Property.ExtendedPositionDescription.readsEPD	-- Default implementation.

-- | An alternative to 'Show'.
class Property.ExtendedPositionDescription.ShowsEPD a => ShowsFEN a where
	showsFEN	:: a -> ShowS	-- ^ Stringify a FEN-datum.
	showsFEN	= Property.ExtendedPositionDescription.showsEPD	-- Default implementation.

-- | Self-documentation.
type FEN	= String

-- | Read from FEN.
readFEN	:: ReadsFEN a => FEN -> a
readFEN fen	= case readsFEN fen of
	[(a, _)]	-> a
	_		-> error . showString "BishBosh.Property.ForsythEdwards.readFEN:\tfailed to parse " $ shows fen "."

-- | Display in FEN.
showFEN	:: ShowsFEN a => a -> FEN
showFEN a	= showsFEN a ""

