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

module BishBosh.Colour.PhysicalColour(
-- * Types
-- ** Data-types
	PhysicalColour(),
-- * Constants
--	range,
	black,
	red,
	green,
	yellow,
	blue,
	magenta,
	cyan,
	white
) where

import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	Control.DeepSeq
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT
import qualified	Text.XML.HXT.Arrow.Pickle.Schema

{- |
	* Defines the sum-type of physical colours which can typically be rendered by a terminal.

	* CAVEAT: the constructor-order both facilitates conversion to an ANSI Colour-code & the derivation of the complementary colour.
-}
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

instance Property.Opposable.Opposable PhysicalColour where
	getOpposite physicalColour	= toEnum $ fromEnum (maxBound :: PhysicalColour) - fromEnum physicalColour	-- N.B. the complementary colour

-- | The constant complete range of values.
range :: [PhysicalColour]
range	= [minBound .. maxBound]

instance Property.FixedMembership.FixedMembership PhysicalColour where
	members	= range

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

