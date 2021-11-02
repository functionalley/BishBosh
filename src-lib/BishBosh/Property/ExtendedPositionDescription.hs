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

	* <https://www.chessprogramming.org/Extended_Position_Description>.
-}

module BishBosh.Property.ExtendedPositionDescription(
-- * Types
-- ** Type-synonyms
	EPD,
-- * Type-classes
	ReadsEPD(..),
	ShowsEPD(..),
-- * Constants
	tag,
	rankSeparator,
	showsNullField,
	showsSeparator,
-- * Functions
	readEPD,
	showEPD
) where

-- | An alternative to 'Read'.
class ReadsEPD a where
	readsEPD	:: ReadS a	-- ^ Read a datum from EPD.

-- | An alternative to 'Show'.
class ShowsEPD a where
	showsEPD	:: a -> ShowS	-- ^ Stringify a EPD-datum.

-- | Self-documentation.
type EPD	= String

-- | Input-format.
tag :: String
tag	= "epd"

rankSeparator :: Char
rankSeparator	= '/'

-- | Read from EPD.
readEPD	:: ReadsEPD a => EPD -> a
readEPD epd	= case readsEPD epd of
	[(a, _)]	-> a
	_		-> error . showString "BishBosh.Property.ExtendedPositionDescription.readEPD:\tfailed to parse " $ shows epd "."

-- | Display in EPD.
showEPD	:: ShowsEPD a => a -> EPD
showEPD a	= showsEPD a ""

-- | The standard way to denote the absence of a field.
showsNullField :: ShowS
showsNullField	= showChar '-'

-- | The standard separator between fields in EPD.
showsSeparator :: ShowS
showsSeparator	= showChar ' '

