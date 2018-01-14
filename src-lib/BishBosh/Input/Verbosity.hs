{-# OPTIONS_GHC -fno-warn-orphans #-}
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

 [@DESCRIPTION@]	Instance-definitions for 'Distribution.Verbosity.Verbosity'.
-}

module BishBosh.Input.Verbosity(
-- * Constants
	tag,
	range
) where

import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Distribution.Verbosity
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	Text.XML.HXT.Arrow.Pickle.Schema

-- | Used to qualify XML.
tag :: String
tag	= "verbosity"

instance Data.Default.Default Distribution.Verbosity.Verbosity where
	def	= Distribution.Verbosity.normal

instance HXT.XmlPickler Distribution.Verbosity.Verbosity where
	xpickle	= HXT.xpWrap (read, show) . HXT.xpAttr tag . HXT.xpTextDT . Text.XML.HXT.Arrow.Pickle.Schema.scEnum $ map show range	-- CAVEAT: whether it'll be used as an XML-attribute or an XML-element isn't currently known.

instance Control.DeepSeq.NFData Distribution.Verbosity.Verbosity where
	rnf _	= ()

-- | The constant complete range of values.
range :: [Distribution.Verbosity.Verbosity]
range	= [minBound .. maxBound]

