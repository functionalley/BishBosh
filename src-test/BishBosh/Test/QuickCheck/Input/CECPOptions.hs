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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary'.
-}

module BishBosh.Test.QuickCheck.Input.CECPOptions() where

import			BishBosh.Test.QuickCheck.Input.CECPFeatures()
import qualified	BishBosh.Input.CECPOptions	as Input.CECPOptions
import qualified	Test.QuickCheck

instance Test.QuickCheck.Arbitrary Input.CECPOptions.CECPOptions where
	arbitrary	= Input.CECPOptions.mkCECPOptions <$> Test.QuickCheck.arbitrary {-analyseMode-} <*> Test.QuickCheck.arbitrary {-displaySAN-} <*> Test.QuickCheck.arbitrary {-editMode-} <*> Test.QuickCheck.arbitrary {-forceMode-} <*> fmap (fmap $ toEnum . abs) Test.QuickCheck.arbitrary {-Maybe pause-} <*> Test.QuickCheck.arbitrary {-ponderMode-} <*> Test.QuickCheck.arbitrary {-postMode-} <*> Test.QuickCheck.elements [1 .. 3] {-protocolVersion-} <*> Test.QuickCheck.arbitrary {-CECPFeatures-}

