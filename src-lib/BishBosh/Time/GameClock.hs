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

 [@DESCRIPTION@]	Defines two stop-watches, exactly one of which is running at any time.
-}

module BishBosh.Time.GameClock(
-- * Types
-- ** Data-types
	GameClock(
--		MkGameClock,
		deconstruct
	),
-- * Functions
	showsElapsedTimes
 ) where

import			Control.Arrow((***))
import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Data.Exception			as Data.Exception
import qualified	BishBosh.Property.ShowFloat		as Property.ShowFloat
import qualified	BishBosh.Property.SelfValidating	as Property.SelfValidating
import qualified	BishBosh.Property.Switchable		as Property.Switchable
import qualified	BishBosh.Text.ShowList			as Text.ShowList
import qualified	BishBosh.Time.StopWatch			as Time.StopWatch
import qualified	BishBosh.Type.Count			as Type.Count
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.Default
import qualified	Data.Foldable

-- | Models a game-clock, in which each player owns a personal stop-watch, exactly one of which is running at any one time.
newtype GameClock	= MkGameClock {
	deconstruct	:: Attribute.LogicalColour.ArrayByLogicalColour Time.StopWatch.StopWatch -- ^ Contains one stop-watch for each of two players.
}

instance Property.Switchable.Switchable GameClock where
	on	= fmap (
		MkGameClock . Attribute.LogicalColour.listArrayByLogicalColour . (
			Data.Default.def :	-- A stopped watch for Black.
		) . return {-to List-monad-}
	 ) Property.Switchable.on		-- A running watch for White.

	toggle gameClock
		| errorMessages@(_ : _)	<- Property.SelfValidating.findInvalidity gameClock	= Control.Exception.throwIO . Data.Exception.mkInsufficientData . showString "Duel.Process.Intermediary.initialise:\tinvalid gameClock; " $ show errorMessages
		| otherwise									= fmap (
			MkGameClock . Attribute.LogicalColour.listArrayByLogicalColour
		) . mapM Property.Switchable.toggle . Data.Array.IArray.elems $ deconstruct gameClock

	switchOff	= fmap (
		MkGameClock . Attribute.LogicalColour.listArrayByLogicalColour
	 ) . mapM Property.Switchable.switchOff . Data.Array.IArray.elems . deconstruct	-- CAVEAT: this invalidates the clock, since a subsequent call to 'toggle' would activate both stop-watches.

	isOn	= Data.Foldable.any Property.Switchable.isOn . deconstruct	-- CAVEAT: includes the dysfunctional state in which both sides are running.

	isOff	= Data.Foldable.all Property.Switchable.isOff . deconstruct

instance Property.SelfValidating.SelfValidating GameClock where
	findInvalidity	= Property.SelfValidating.findErrors [
		((/= 1) . length . filter Property.Switchable.isOn . Data.Array.IArray.elems . deconstruct,	"The two stop-watches must be in opposite states")
	 ]

-- | Show the elapsed times.
showsElapsedTimes :: Type.Count.NDecimalDigits -> GameClock -> IO ShowS
showsElapsedTimes nDecimalDigits	= fmap (
	Text.ShowList.showsAssociationList' . map (
		show *** Property.ShowFloat.showsFloatToN nDecimalDigits
	) . Data.Array.IArray.assocs . deconstruct
 ) . Property.Switchable.switchOff

