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

 [@DESCRIPTION@]	Defines the algorithm used to sort the immediately available capture-/move/s, before evaluating the fitness of the resulting positions.
-}

module BishBosh.Attribute.CaptureMoveSortAlgorithm(
-- * Types
-- ** Data-types
	CaptureMoveSortAlgorithm(..),
-- * Constants
	tag
--	range
) where

import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT
import qualified	Text.XML.HXT.Arrow.Pickle.Schema

-- | Used to qualify XML.
tag :: String
tag	= "captureMoveSortAlgorithm"

-- | The sum-type of algorithms by which moves may be sorted.
data CaptureMoveSortAlgorithm
	= MVVLVA	-- ^ <https://www.chessprogramming.org/MVV-LVA>.
	| SEE		-- ^ <https://www.chessprogramming.org/Static_Exchange_Evaluation>.
	deriving (Eq, Read, Show)

instance Control.DeepSeq.NFData CaptureMoveSortAlgorithm where
	rnf _	= ()

instance Data.Default.Default CaptureMoveSortAlgorithm where
	def	= MVVLVA

-- | Constant.
range :: [CaptureMoveSortAlgorithm]
range	= [MVVLVA, SEE]

instance Property.FixedMembership.FixedMembership CaptureMoveSortAlgorithm where
	members	= range

instance HXT.XmlPickler CaptureMoveSortAlgorithm where
	xpickle	= HXT.xpAttr tag . HXT.xpWrap (read, show) . HXT.xpTextDT . Text.XML.HXT.Arrow.Pickle.Schema.scEnum $ map show range

