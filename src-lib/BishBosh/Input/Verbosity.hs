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
	The levels of program-output.
	N.B.: the data-type is coincidentally similar to 'Distribution.Verbosity.Internal.VerbosityLevel'.
-}

module BishBosh.Input.Verbosity(
-- * Types
-- ** Data-types
	Verbosity(),
-- * Constants
	tag,
	range
) where

import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT
import qualified	Text.XML.HXT.Arrow.Pickle.Schema

-- | Used to qualify XML.
tag :: String
tag	= "verbosity"

-- | Define the sum-type of levels of program-output.
data Verbosity
	= Silent
	| Normal
	| Verbose
	| Deafening
	deriving (Enum, Eq, Ord, Read, Show)

instance Bounded Verbosity where
	minBound	= Silent
	maxBound	= Deafening

instance Data.Default.Default Verbosity where
	def	= Normal

instance HXT.XmlPickler Verbosity where
	xpickle	= HXT.xpWrap (read, show) . HXT.xpAttr tag . HXT.xpTextDT . Text.XML.HXT.Arrow.Pickle.Schema.scEnum $ map show range	-- CAVEAT: whether it'll be used as an XML-attribute or an XML-element isn't currently known.

instance Control.DeepSeq.NFData Verbosity where
	rnf _	= ()

-- | The constant complete range of values.
range :: [Verbosity]
range	= [minBound .. maxBound]

instance Property.FixedMembership.FixedMembership Verbosity where
	members	= range

