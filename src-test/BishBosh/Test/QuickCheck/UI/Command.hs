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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties.
-}

module BishBosh.Test.QuickCheck.UI.Command (
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.UI.PrintObject()
import			BishBosh.Test.QuickCheck.UI.SetObject()
import qualified	BishBosh.UI.Command	as UI.Command
import qualified	Test.QuickCheck

instance Test.QuickCheck.Arbitrary UI.Command.Command where
	arbitrary	= Test.QuickCheck.oneof [
		Test.QuickCheck.elements [
			UI.Command.Hint,
			UI.Command.Quit,
			UI.Command.Resign,
			UI.Command.Restart,
			UI.Command.Save,
			UI.Command.Swap
		], -- Nullary commands
		UI.Command.Print `fmap` Test.QuickCheck.arbitrary,
		fmap UI.Command.RollBack . Test.QuickCheck.elements $ Nothing : map Just [1 .. 10],
		UI.Command.Set `fmap` Test.QuickCheck.arbitrary
	 ]

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: UI.Command.Command -> Test.QuickCheck.Property
		f command	= Test.QuickCheck.label "Command.prop_io" $ UI.Command.readsCommand (UI.Command.showsCommand command "") == Right (command, "")
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f
 ]

