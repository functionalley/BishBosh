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

 [@DESCRIPTION@]	Defines the encoding of colours for rendering on a terminal; <https://en.wikipedia.org/wiki/ANSI_escape_code>.
-}

module BishBosh.Colour.ANSIColourCode(
-- * Types
-- ** Type-synonyms
--	ANSIColourCode(),
--	IsBold,
	GraphicsRendition,
-- * Functions
	selectGraphicsRendition,
	bracket,
-- ** Constructors
	mkFgColourCode,
	mkBgColourCode
) where

import qualified	BishBosh.Colour.PhysicalColour	as Colour.PhysicalColour
import qualified	Data.Default

-- | A colour-code as used by terminal-emulators.
newtype ANSIColourCode	= MkANSIColourCode {
	deconstruct	:: Int
}

instance Show ANSIColourCode where
	showsPrec precedence MkANSIColourCode { deconstruct = i }	= showsPrec precedence i

instance Data.Default.Default ANSIColourCode where
	def	= MkANSIColourCode 0

-- | The font-weight of a character.
type IsBold	= Bool

-- | An escape-sequence used to control a terminal.
type GraphicsRendition	= String

-- | Constructor: offset the specified colour-code, so that it applies to the foreground.
mkFgColourCode :: Colour.PhysicalColour.PhysicalColour -> ANSIColourCode
mkFgColourCode	= MkANSIColourCode . (+ 30) . fromEnum {-CAVEAT: relies on the PhysicalColour's constructor-order-}

-- | Constructor: offset the specified colour-code, so that it applies to the background.
mkBgColourCode :: Colour.PhysicalColour.PhysicalColour -> ANSIColourCode
mkBgColourCode	= MkANSIColourCode . (+ 40) . fromEnum {-CAVEAT: relies on the PhysicalColour's constructor-order-}

-- | Generate the escape-sequence required to change a terminal to the specified physical colour.
selectGraphicsRendition :: IsBold -> ANSIColourCode -> GraphicsRendition
selectGraphicsRendition isBold parameter	= showString "\x1b[" . shows parameter $ (if isBold then showString ";1" else id) "m"

-- | Render the specified string according to instructions, then revert to the default.
bracket :: GraphicsRendition -> String -> ShowS
bracket graphicsRendition s	= showString graphicsRendition . showString s . showString (
	selectGraphicsRendition False Data.Default.def
 )

