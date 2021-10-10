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

 [@DESCRIPTION@]	Defines the fields a user can mutate.
-}

module BishBosh.UI.SetObject (
-- * Types
-- ** Data-types
	SetObject(..),
-- * Functions
	autoComplete,
-- ** Constructors
	mkSearchDepth
 ) where

import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Input.SearchOptions	as Input.SearchOptions
import qualified	BishBosh.Text.AutoComplete	as Text.AutoComplete
import qualified	BishBosh.Type.Count		as Type.Count
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.List.Extra

-- | The fields a user can mutate; currently there's only one.
newtype SetObject	= SearchDepth Type.Count.NPlies	deriving Eq

instance Control.DeepSeq.NFData SetObject where
	rnf (SearchDepth searchDepth)		= Control.DeepSeq.rnf searchDepth

instance Show SetObject where
	showsPrec _ (SearchDepth searchDepth)	= showString Input.SearchOptions.searchDepthTag . showChar ' ' . shows searchDepth

instance Read SetObject where
	readsPrec precedence s	= case Control.Arrow.first Data.List.Extra.lower `map` lex s of
		[("searchdepth", s')]		-> Control.Arrow.first (mkSearchDepth . fromInteger) `map` readsPrec precedence s'
		_				-> []	-- No parse.

-- | Smart constructor.
mkSearchDepth :: Type.Count.NPlies -> SetObject
mkSearchDepth searchDepth
	| searchDepth < Input.SearchOptions.minimumSearchDepth	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.UI.SetObject.mkSearchDepth:\t" $ shows Input.SearchOptions.searchDepthTag " must be positive."
	| otherwise						= SearchDepth searchDepth

-- | Replace the first word of the specified string with the name of a command of which it is an unambiguous case-insensitive prefix.
autoComplete :: ShowS
autoComplete	= Text.AutoComplete.autoComplete [Input.SearchOptions.searchDepthTag]

