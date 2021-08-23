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

 [@DESCRIPTION@]	Exports functions to facilitate change of case.
-}

module BishBosh.Text.Case(
-- * Functions
--	translateInitial,
--	toLowerInitial,
	toUpperInitial
) where

import qualified	Data.Char

-- | Convert the initial letter of the specified string.
translateInitial :: (Char -> Char) -> String -> String
translateInitial f (c : cs)	= f c : cs
translateInitial _ _		= []

-- | Convert the initial letter of the specified string to lower-case.
toLowerInitial :: String -> String
toLowerInitial	= translateInitial Data.Char.toLower

-- | Convert the initial letter of the specified string to upper-case.
toUpperInitial :: String -> String
toUpperInitial	= translateInitial Data.Char.toUpper
