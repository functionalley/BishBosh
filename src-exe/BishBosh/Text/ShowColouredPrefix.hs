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

import qualified	BishBosh.Attribute.ANSIColourCode	as Attribute.ANSIColourCode
import qualified	BishBosh.Attribute.PhysicalColour	as Attribute.PhysicalColour
import qualified	BishBosh.Text.ShowPrefix		as Text.ShowPrefix

-- | Show the prefix used to denote an information-message.
showsPrefixInfo :: ShowS
showsPrefixInfo	= Attribute.ANSIColourCode.bracket (
	Attribute.ANSIColourCode.selectGraphicsRendition False $ Attribute.ANSIColourCode.mkFgColourCode Attribute.PhysicalColour.green
 ) $ Text.ShowPrefix.showsPrefixInfo ""

-- | Show the prefix used to denote a warning-message.
showsPrefixWarning :: ShowS
showsPrefixWarning	= Attribute.ANSIColourCode.bracket (
	Attribute.ANSIColourCode.selectGraphicsRendition True $ Attribute.ANSIColourCode.mkFgColourCode Attribute.PhysicalColour.yellow
 ) $ Text.ShowPrefix.showsPrefixWarning ""

-- | Show the prefix used to denote an error-message.
showsPrefixError :: ShowS
showsPrefixError	= Attribute.ANSIColourCode.bracket (
	Attribute.ANSIColourCode.selectGraphicsRendition True $ Attribute.ANSIColourCode.mkFgColourCode Attribute.PhysicalColour.red
 ) $ Text.ShowPrefix.showsPrefixError ""

