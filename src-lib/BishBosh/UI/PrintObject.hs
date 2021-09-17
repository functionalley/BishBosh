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

 [@DESCRIPTION@]	Defines the static data a user can request at runtime.
-}

module BishBosh.UI.PrintObject (
-- * Types
-- ** Data-types
	PrintObject(..),
-- * Constants
	configurationTag,
	helpTag,
	range,
-- * Functions
	autoComplete
 ) where

import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Text.AutoComplete		as Text.AutoComplete
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Data.List.Extra

-- | Input-format.
configurationTag :: String
configurationTag	= "configuration"

-- | Input-format.
helpTag :: String
helpTag			= "help"

-- | A sum-type of objects a user may want to print at runtime.
data PrintObject
	= Configuration
	| Help
	deriving Eq

instance Control.DeepSeq.NFData PrintObject where
	rnf _	= ()

instance Show PrintObject where
	show Configuration	= configurationTag
	show Help		= helpTag

instance Read PrintObject where
	readsPrec _ s	= case Control.Arrow.first Data.List.Extra.lower `map` lex s of
		[("configuration", remainder)]	-> [(Configuration, remainder)]
		[("help", remainder)]		-> [(Help, remainder)]
		_				-> []	-- No parse.

-- | The constant list of possible values.
range :: [PrintObject]
range	= [Configuration, Help]

instance Property.FixedMembership.FixedMembership PrintObject where
	members	= range

-- | Replace the first word of the specified string with the name of the object to print, of which it is an unambiguous case-insensitive prefix.
autoComplete :: ShowS
autoComplete	= Text.AutoComplete.autoComplete [
	configurationTag,
	helpTag
 ]

