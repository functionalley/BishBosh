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
-- * Types
-- ** Type-synonyms
--	Command,
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.UI.PrintObject()
import			BishBosh.Test.QuickCheck.UI.SetObject()
import qualified	BishBosh.Types		as T
import qualified	BishBosh.UI.Command	as UI.Command
-- import qualified	Control.Arrow
import qualified	Test.QuickCheck

-- | Define a concrete type for testing.
type Command	= UI.Command.Command T.X T.Y

instance Test.QuickCheck.Arbitrary (UI.Command.Command x y) where
	arbitrary	= Test.QuickCheck.oneof [
		return {-to Gen-monad-} UI.Command.Hint,
		fmap UI.Command.Print Test.QuickCheck.arbitrary,
		return {-to Gen-monad-} UI.Command.Quit,
		return {-to Gen-monad-} UI.Command.Resign,
		return {-to Gen-monad-} UI.Command.Restart,
		fmap (UI.Command.RollBack . fmap (`mod` 10)) Test.QuickCheck.arbitrary,
		return {-to Gen-monad-} UI.Command.Save,
		fmap UI.Command.Set Test.QuickCheck.arbitrary,
		return {-to Gen-monad-} UI.Command.Swap
	 ]

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Command -> Test.QuickCheck.Property
		f command	= Test.QuickCheck.label "Command.prop_io" $ UI.Command.readsCommand (UI.Command.showsCommand command "") == Right (command, "")
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f
 ]

