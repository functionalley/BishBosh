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

 [@DESCRIPTION@]	Defines the things a user can mutate at runtime.
-}

module BishBosh.UI.SetObject (
-- * Types
-- ** Data-types
	SetObject(..),
-- * Constants
	searchDepthTag,
-- * Functions
	autoComplete,
-- ** Constructors
	mkEPD,
	mkSearchDepth
 ) where


import qualified	BishBosh.Data.Exception				as Data.Exception
import qualified	BishBosh.Input.SearchOptions			as Input.SearchOptions
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.Property.ExtendedPositionDescription	as Property.ExtendedPositionDescription
import qualified	BishBosh.Text.AutoComplete			as Text.AutoComplete
import qualified	BishBosh.Type.Count				as Type.Count
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.List.Extra

-- | Input-format.
searchDepthTag :: String
searchDepthTag		= Input.SearchOptions.searchDepthTag

-- | The sum-type of fields a user can mutate.
data SetObject
	= EPD Model.Game.Game		-- ^ Define the position.
	| SearchDepth Type.Count.NPlies	-- ^ Set the number of plies to to search ahead for the optimal move.
	deriving Eq

instance Control.DeepSeq.NFData SetObject where
	rnf (EPD epd)			= Control.DeepSeq.rnf epd
	rnf (SearchDepth searchDepth)	= Control.DeepSeq.rnf searchDepth

instance Show SetObject where
	showsPrec _ (EPD epd)			= showString Property.ExtendedPositionDescription.tag . showChar ' ' . Property.ExtendedPositionDescription.showsEPD epd
	showsPrec _ (SearchDepth searchDepth)	= showString searchDepthTag . showChar ' ' . shows searchDepth

instance Read SetObject where
	readsPrec precedence s	= case Control.Arrow.first Data.List.Extra.lower `map` lex s of
		[("epd", epd)]		-> Control.Arrow.first EPD `map` Property.ExtendedPositionDescription.readsEPD epd
		[("searchdepth", s')]	-> Control.Arrow.first (mkSearchDepth . fromInteger) `map` readsPrec precedence s'
		_			-> []	-- No parse.

-- | Constructor.
mkEPD :: Model.Game.Game -> SetObject
mkEPD	= EPD

-- | Smart constructor.
mkSearchDepth :: Type.Count.NPlies -> SetObject
mkSearchDepth searchDepth
	| searchDepth < Input.SearchOptions.minimumSearchDepth	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.UI.SetObject.mkSearchDepth:\t" $ shows Input.SearchOptions.searchDepthTag " must be positive."
	| otherwise						= SearchDepth searchDepth

-- | Replace the first word of the specified string with the name of a command of which it is an unambiguous case-insensitive prefix.
autoComplete :: ShowS
autoComplete	= Text.AutoComplete.autoComplete [Property.ExtendedPositionDescription.tag, searchDepthTag]

