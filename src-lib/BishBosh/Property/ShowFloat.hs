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

 [@DESCRIPTION@]	An interface for floating-point data, which provides an alterative to 'Show'.
-}

module BishBosh.Property.ShowFloat(
-- * Type-classes
	ShowFloat(..),
-- * Functions
	showsFloatToN',
	showsFloatToN
) where

import qualified	BishBosh.Type.Count	as Type.Count
import qualified	Data.Ratio
import qualified	Numeric

-- | Render the specified data to the specified number of decimal digits.
showsFloatToN' :: RealFloat a => Type.Count.NDecimalDigits -> a -> ShowS
showsFloatToN' nDecimalDigits	= Numeric.showFFloat (Just $ fromIntegral nDecimalDigits)

-- | An alternative to 'Show', for floating-point data.
class ShowFloat a where
	showsFloat	:: (Double -> ShowS) -> a -> ShowS

instance ShowFloat Double where
	showsFloat	= id

instance ShowFloat Float where
	showsFloat fromDouble	= fromDouble . realToFrac

instance Integral r => ShowFloat (Data.Ratio.Ratio r) where
	showsFloat fromDouble	= fromDouble . realToFrac

-- | Render the specified data to the specified number of decimal digits.
showsFloatToN :: ShowFloat a => Type.Count.NDecimalDigits -> a -> ShowS
showsFloatToN nDecimalDigits	= showsFloat $ showsFloatToN' nDecimalDigits

