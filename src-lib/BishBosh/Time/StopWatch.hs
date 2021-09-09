{-
	Copyright (C) 2021 Dr. Alistair Ward

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

 [@DESCRIPTION@]	Defines a stop-watch for use by a single player; so two independent instances will be required to construct a game-clock.
-}

module BishBosh.Time.StopWatch(
-- * Types
-- ** Data-types
	StopWatch(
--		MkStopWatch,
--		deconstruct
	),
-- * Functions
	getElapsedTime,
-- ** Constructor
	mkStoppedWatch
 ) where

import			Control.Arrow((&&&), (|||))
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Property.ShowFloat	as Property.ShowFloat
import qualified	BishBosh.Property.Switchable	as Property.Switchable
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Default
import qualified	Data.Time.Clock

-- | The watch is either running & records the time at which it was started, or is stopped & records the elapsed duration.
newtype StopWatch	= MkStopWatch {
	deconstruct	:: Either Data.Time.Clock.UTCTime Data.Time.Clock.NominalDiffTime	-- ^ Either the start-time or the previously elapsed duration.
} deriving (Eq, Show)

instance Control.DeepSeq.NFData StopWatch where
	rnf MkStopWatch { deconstruct = e }	= Control.DeepSeq.rnf ||| Control.DeepSeq.rnf $ e

instance Data.Default.Default StopWatch where
	def	= mkStoppedWatch 0

instance Property.ShowFloat.ShowFloat StopWatch where
	showsFloat fromDouble	= fromDouble . getElapsedTime

instance Property.Switchable.Switchable StopWatch where
	on	= Property.Switchable.toggle Data.Default.def

	toggle MkStopWatch { deconstruct = e }	= do
		(diffUTCTime', addUTCTime')	<- fmap (Data.Time.Clock.diffUTCTime &&& flip Data.Time.Clock.addUTCTime) Data.Time.Clock.getCurrentTime	-- Partially apply the functions to the current time.

		return {-to IO-monad-} . MkStopWatch $ (Right . diffUTCTime' ||| Left . addUTCTime' . negate) e	-- Complete the function-application. N.B.: Right & Left are reflected.

	isOn MkStopWatch { deconstruct = Left _ }	= True
	isOn _						= False

-- | Constructor.
mkStoppedWatch :: Data.Time.Clock.NominalDiffTime -> StopWatch
mkStoppedWatch	= MkStopWatch . Right

-- | Extract the elapsed time from a stopped watch.
getElapsedTime :: Fractional f => StopWatch -> f
getElapsedTime MkStopWatch {
	deconstruct	= Right nominalDiffTime
}			= realToFrac nominalDiffTime
getElapsedTime _	= Control.Exception.throw $ Data.Exception.mkRequestFailure "BishBosh.Time.StopWatch.getElapsedTime:\tthe watch is still running."

