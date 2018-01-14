{-# LANGUAGE ScopedTypeVariables #-}
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

 [@DESCRIPTION@]	Define standard message-prefixes.
-}

module BishBosh.Text.Show(
-- * Constants
	showsInfoPrefix,
	showsWarningPrefix,
	showsErrorPrefix,
 ) where

import qualified	BishBosh.Attribute.PhysicalColour	as Attribute.PhysicalColour
import qualified	BishBosh.Text.ShowList			as Text.ShowList

-- | Show the prefix used to denote an information-message.
showsInfoPrefix :: ShowS
showsInfoPrefix	= Attribute.PhysicalColour.bracket (
	Attribute.PhysicalColour.selectGraphicsRendition False $ Attribute.PhysicalColour.mkFgColourCode Attribute.PhysicalColour.green
 ) $ Text.ShowList.showsInfoPrefix ""

-- | Show the prefix used to denote a warning-message.
showsWarningPrefix :: ShowS
showsWarningPrefix	= Attribute.PhysicalColour.bracket (
	Attribute.PhysicalColour.selectGraphicsRendition True $ Attribute.PhysicalColour.mkFgColourCode Attribute.PhysicalColour.yellow
 ) $ Text.ShowList.showsWarningPrefix ""

-- | Show the prefix used to denote a error-message.
showsErrorPrefix :: ShowS
showsErrorPrefix	= Attribute.PhysicalColour.bracket (
	Attribute.PhysicalColour.selectGraphicsRendition True $ Attribute.PhysicalColour.mkFgColourCode Attribute.PhysicalColour.red
 ) $ Text.ShowList.showsErrorPrefix ""

