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

 [@DESCRIPTION@]	Static tests.
-}

module BishBosh.Test.HUnit.Time.GameClock(
-- * Constants
	testCases
) where

import qualified	BishBosh.Time.GameClock			as Time.GameClock
import qualified	BishBosh.Time.StopWatch			as Time.StopWatch
import qualified	BishBosh.Property.SelfValidating	as Property.SelfValidating
import qualified	BishBosh.Property.Switchable		as Property.Switchable
import qualified	Control.Concurrent
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	System.Random
import qualified	Test.HUnit
import			Test.HUnit((@?))

-- | Check the sanity of the implementation, by validating a list of static test-cases.
testCases :: Test.HUnit.Test
testCases	= Test.HUnit.test $ map Test.HUnit.TestCase [
	do
		stoppedGameClock	<- Property.Switchable.switchOff =<< Property.Switchable.on

		Property.Switchable.isOff (stoppedGameClock :: Time.GameClock.GameClock) @? "Property.Switchable.switchOff failed.",
	do
		runningGameClock	<- flick 2

		Property.Switchable.isOn runningGameClock @? "Property.Switchable.Property.Switchable.flick (double) failed.",
	do
		runningGameClock	<- flick 3

		Property.Switchable.isOn runningGameClock @? "Property.Switchable.Property.Switchable.flick (triple) failed.",
	do
		runningGameClock	<- flick 3

		Property.SelfValidating.isValid runningGameClock @? "Property.Switchable.Property.SelfValidating.isValid failed.",
	let
		delayedFlick :: [Int] -> Time.GameClock.GameClock -> IO Time.GameClock.GameClock
		delayedFlick (t : ts) gameClock	= do
			Control.Concurrent.threadDelay t

			Property.Switchable.toggle gameClock >>= delayedFlick ts
		delayedFlick _ gameClock	= return {-to IO-monad-} gameClock
	 in do
		randomGenerator		<- System.Random.getStdGen
		runningWatch		<- Property.Switchable.on
		stoppedGameClock	<- Property.Switchable.switchOff =<< delayedFlick (take 16 $ System.Random.randomRs (1, 100000 {-uS-}) randomGenerator) =<< Property.Switchable.on
		stoppedWatch		<- Property.Switchable.switchOff runningWatch

		let
			relativeError :: Rational
			relativeError	= pred . (/ Time.StopWatch.getElapsedTime stoppedWatch) . Data.Foldable.sum . Data.Array.IArray.amap Time.StopWatch.getElapsedTime $ Time.GameClock.deconstruct stoppedGameClock

		abs relativeError < recip 10000 @? showString "Time.GameClock.GameClock:\trelative error between sum of game-clock times & stop-watch time = " (
			 shows (realToFrac relativeError :: Double) "."
		 )
 ] where
	flick :: Int -> IO Time.GameClock.GameClock
	flick n	= Property.Switchable.on >>= Property.Switchable.flick n

