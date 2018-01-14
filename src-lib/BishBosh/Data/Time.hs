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
-}

module BishBosh.Data.Time(
-- * Functions
	showsTimeAsSeconds,
	measureElapsedTime
 ) where

import qualified	Data.Time.Clock
import qualified	Text.Printf

-- | Shows a time-difference as a number of seconds, to the required precision.
showsTimeAsSeconds :: Text.Printf.PrintfArg i => i -> Data.Time.Clock.NominalDiffTime -> ShowS
showsTimeAsSeconds nDecimalDigits time	= showString $ Text.Printf.printf "%.*fs" nDecimalDigits (realToFrac time :: Double)

-- | Measure the time which has elapsed since that specified.
measureElapsedTime :: Data.Time.Clock.UTCTime -> IO Data.Time.Clock.NominalDiffTime
measureElapsedTime startUTCTime	= (`Data.Time.Clock.diffUTCTime` startUTCTime) `fmap` Data.Time.Clock.getCurrentTime

