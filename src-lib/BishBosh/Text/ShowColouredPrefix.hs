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

 [@DESCRIPTION@]	Define standard prefixes for log-messages.
-}

module BishBosh.Text.ShowColouredPrefix(
-- * Constants
	showsPrefixInfo,
	showsPrefixWarning,
	showsPrefixError
 ) where

import qualified	BishBosh.Colour.ANSIColourCode	as Colour.ANSIColourCode
import qualified	BishBosh.Colour.PhysicalColour	as Colour.PhysicalColour
import qualified	BishBosh.Text.ShowPrefix	as Text.ShowPrefix

-- | Show the prefix used to denote an information-message.
showsPrefixInfo :: ShowS
showsPrefixInfo	= Colour.ANSIColourCode.bracket (
	Colour.ANSIColourCode.selectGraphicsRendition False $ Colour.ANSIColourCode.mkFgColourCode Colour.PhysicalColour.green
 ) $ Text.ShowPrefix.showsPrefixInfo ""

-- | Show the prefix used to denote a warning-message.
showsPrefixWarning :: ShowS
showsPrefixWarning	= Colour.ANSIColourCode.bracket (
	Colour.ANSIColourCode.selectGraphicsRendition True $ Colour.ANSIColourCode.mkFgColourCode Colour.PhysicalColour.yellow
 ) $ Text.ShowPrefix.showsPrefixWarning ""

-- | Show the prefix used to denote an error-message.
showsPrefixError :: ShowS
showsPrefixError	= Colour.ANSIColourCode.bracket (
	Colour.ANSIColourCode.selectGraphicsRendition True $ Colour.ANSIColourCode.mkFgColourCode Colour.PhysicalColour.red
 ) $ Text.ShowPrefix.showsPrefixError ""

