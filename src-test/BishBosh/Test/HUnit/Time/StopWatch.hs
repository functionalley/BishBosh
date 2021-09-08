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

module BishBosh.Test.HUnit.Time.StopWatch(
-- * Constants
	testCases
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Time.StopWatch		as Time.StopWatch
import qualified	BishBosh.Property.Switchable	as Property.Switchable
import qualified	Data.Default
import qualified	Test.HUnit
import			Test.HUnit((@?))

-- | Check the sanity of the implementation, by validating a list of static test-cases.
testCases :: Test.HUnit.Test
testCases	= Test.HUnit.test $ map Test.HUnit.TestCase [
	do
		stoppedWatch'	<- Property.Switchable.switchOff =<< Property.Switchable.on

		Property.Switchable.isOff (stoppedWatch' :: Time.StopWatch.StopWatch) @? "Property.Switchable.switchOff failed.",
	do
		runningWatch	<- Property.Switchable.flick 2 =<< Property.Switchable.on

		Property.Switchable.isOn (runningWatch :: Time.StopWatch.StopWatch) @? "Property.Switchable.Property.Switchable.flick (double) failed.",
	do
		runningWatch	<- Property.Switchable.flick 3 stoppedWatch

		uncurry (/=) (
			($ stoppedWatch) &&& ($ runningWatch) $ Property.Switchable.isOn
		 ) @? "Property.Switchable.Property.Switchable.flick (triple) failed.",
	do
		stoppedWatch'	<- Property.Switchable.flick 2 stoppedWatch

		uncurry (==) (
			($ stoppedWatch) &&& ($ stoppedWatch') $ Property.Switchable.isOn
		 ) @? "Property.Switchable.Property.Switchable.flick (double) failed."
 ] where
	stoppedWatch :: Time.StopWatch.StopWatch
	stoppedWatch	= Data.Default.def

