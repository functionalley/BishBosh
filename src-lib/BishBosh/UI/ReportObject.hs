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

 [@DESCRIPTION@]	Defines the dynamic data a user can request at runtime.
-}

module BishBosh.UI.ReportObject (
-- * Types
-- ** Data-types
	ReportObject(..),
-- * Constants
--	availableMovesTag,
--	boardTag,
--	epdTag,
--	fenTag,
--	gameTag,
--	maxPositionInstancesTag,
--	movesTag,
--	pgnTag,
--	reversiblePlyCountTag,
	range,
-- * Functions
	autoComplete
 ) where

import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Text.AutoComplete		as Text.AutoComplete
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Data.List.Extra

-- | Input-format.
availableMovesTag :: String
availableMovesTag	= "availableMoves"

-- | Input-format.
boardTag :: String
boardTag		= "board"

-- | Input-format.
epdTag :: String
epdTag			= "epd"

-- | Input-format.
fenTag :: String
fenTag			= "fen"

-- | Input-format.
gameTag :: String
gameTag			= "game"

-- | Input-format.
maxPositionInstancesTag :: String
maxPositionInstancesTag	= "maxPositionInstances"

-- | Input-format.
movesTag :: String
movesTag		= showString Component.Move.tag "s"

-- | Input-format.
pgnTag :: String
pgnTag			= "pgn"

-- | Input-format.
reversiblePlyCountTag :: String
reversiblePlyCountTag	= "reversiblePlyCount"

-- | A sum-type of objects a user may want to print at runtime.
data ReportObject
	= AvailableMoves
	| Board
	| EPD
	| FEN
	| Game
	| MaxPositionInstances
	| Moves
	| PGN
	| ReversiblePlyCount
	deriving Eq

instance Control.DeepSeq.NFData ReportObject where
	rnf _	= ()

instance Show ReportObject where
	show AvailableMoves		= availableMovesTag
	show Board			= boardTag
	show EPD			= epdTag
	show FEN			= fenTag
	show Game			= gameTag
	show MaxPositionInstances	= maxPositionInstancesTag
	show Moves			= movesTag
	show PGN			= pgnTag
	show ReversiblePlyCount		= reversiblePlyCountTag

instance Read ReportObject where
	readsPrec _ s	= case Control.Arrow.first Data.List.Extra.lower `map` lex s of
		[("availablemoves", remainder)]		-> [(AvailableMoves, remainder)]
		[("board", remainder)]			-> [(Board, remainder)]
		[("epd", remainder)]			-> [(EPD, remainder)]
		[("fen", remainder)]			-> [(FEN, remainder)]
		[("game", remainder)]			-> [(Game, remainder)]
		[("maxpositioninstances", remainder)]	-> [(MaxPositionInstances, remainder)]
		[("moves", remainder)]			-> [(Moves, remainder)]
		[("pgn", remainder)]			-> [(PGN, remainder)]
		[("reversibleplycount", remainder)]	-> [(ReversiblePlyCount, remainder)]
		_					-> []	-- No parse.

-- | The constant list of possible values.
range :: [ReportObject]
range	= [AvailableMoves, Board, EPD, FEN, Game, MaxPositionInstances, Moves, PGN, ReversiblePlyCount]

instance Property.FixedMembership.FixedMembership ReportObject where
	members	= range

-- | Replace the first word of the specified string with the name of the object to print, of which it is an unambiguous case-insensitive prefix.
autoComplete :: ShowS
autoComplete	= Text.AutoComplete.autoComplete [
	availableMovesTag,
	boardTag,
	epdTag,
	fenTag,
	gameTag,
	maxPositionInstancesTag,
	movesTag,
	pgnTag,
	reversiblePlyCountTag
 ]

