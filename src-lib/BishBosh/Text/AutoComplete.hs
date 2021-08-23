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

 [@DESCRIPTION@]	Autocomplete a truncated word.
-}

module BishBosh.Text.AutoComplete (
-- * Functions
	autoComplete
 ) where

import qualified	BishBosh.Data.List	as Data.List
import qualified	Control.Arrow
import qualified	Data.Char
import qualified	Data.List.Extra

{- |
	* Replace the first word from the specified string, with any item from the specified list, of which it's an unambiguously prefix.

	* N.B.: the comparison is case-insensitive, but the replacement preserves the case supplied in the choices.
-}
autoComplete
	:: [String]	-- ^ Choices
	-> String	-- ^ Words, the first of which may be replaced if an unambiguous completion can be found.
	-> String
autoComplete choices	= uncurry (++) . Control.Arrow.first (
	Data.List.unabbreviate Data.List.Extra.lower choices
 ) . break Data.Char.isSpace . Data.List.Extra.trimStart

