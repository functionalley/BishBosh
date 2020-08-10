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

	* Defines the data-type which represents the physical (rather than logical) colour of the /board/ & of /piece/s.

	* The number of physical colours used to represent the /board/ & /piece/s can be greater than the two /logical colour/s required,
	but is limited in practice, since the terminal (optionally) used to render the image, typically can't cope with with a large number.
-}

module BishBosh.Attribute.PhysicalColour(
-- * Types
-- ** Type-synonyms
--	ANSIColourCode,
-- ** Data-types
	PhysicalColour(..),
-- * Constants
	black,
	red,
	green,
	yellow,
	blue,
	magenta,
	cyan,
	white,
	range,
-- * Functions
--	toANSIColourCode,
	mkFgColourCode,
	mkBgColourCode,
	selectGraphicsRendition,
	bracket
) where

import qualified	Control.DeepSeq
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	Text.XML.HXT.Arrow.Pickle.Schema

-- | Defines the physical colours which can typically be rendered by a terminal.
data PhysicalColour
	= Black
	| Red
	| Green
	| Yellow
	| Blue
	| Magenta
	| Cyan
	| White
	deriving (Enum, Eq, Read, Show)

instance Control.DeepSeq.NFData PhysicalColour where
	rnf _	= ()

instance Bounded PhysicalColour where
	minBound	= black
	maxBound	= white

instance HXT.XmlPickler PhysicalColour where
	xpickle	= HXT.xpWrap (read, show) . HXT.xpTextDT . Text.XML.HXT.Arrow.Pickle.Schema.scEnum $ map show range

-- | Constant.
black :: PhysicalColour
black	= Black

-- | Constant.
red :: PhysicalColour
red	= Red

-- | Constant.
green :: PhysicalColour
green	= Green

-- | Constant.
yellow :: PhysicalColour
yellow	= Yellow

-- | Constant.
blue :: PhysicalColour
blue	= Blue

-- | Constant.
magenta :: PhysicalColour
magenta	= Magenta

-- | Constant.
cyan :: PhysicalColour
cyan	= Cyan

-- | Constant.
white :: PhysicalColour
white	= White

-- | The constant complete range of values.
range :: [PhysicalColour]
range	= [minBound .. maxBound]

-- | A colour-code, as used by terminal-emulators; <https://en.wikipedia.org/wiki/ANSI_escape_code>.
type ANSIColourCode	= Int

-- | Offset the specified colour-code, so that it applies to the foreground.
mkFgColourCode :: PhysicalColour -> ANSIColourCode
mkFgColourCode	= (+ 30) . toANSIColourCode

-- | Offset the specified colour-code, so that it applies to the background.
mkBgColourCode :: PhysicalColour -> ANSIColourCode
mkBgColourCode	= (+ 40) . toANSIColourCode

-- | Translate.
toANSIColourCode :: PhysicalColour -> ANSIColourCode
toANSIColourCode	= fromEnum	-- CAVEAT: the order of the data-constructors has been defined with this in mind.

-- | Generate the escape-sequence required to change a terminal to the specified physical colour.
selectGraphicsRendition :: Bool -> ANSIColourCode -> String
selectGraphicsRendition isBold parameter	= showString "\x1b[" . shows parameter $ (if isBold then showString ";1" else id) "m"

-- | Render the specified string according to instructions, then revert to default.
bracket :: String -> String -> ShowS
bracket graphicsRendition s	= showString graphicsRendition . showString s . showString (
	selectGraphicsRendition False 0
 )

