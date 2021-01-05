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

 [@DESCRIPTION@]	Defines the things a user can print.
-}

module BishBosh.UI.PrintObject (
-- * Types
-- ** Data-types
	PrintObject(..),
-- * Constants
	boardTag,
	configurationTag,
	epdTag,
	fenTag,
	gameTag,
	helpTag,
	movesTag,
	pgnTag,
	range,
-- * Functions
	autoComplete
 ) where

import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Data.Char
import qualified	Data.List
import qualified	Data.List.Extra

-- | Input-format.
boardTag :: String
boardTag		= "board"

-- | Input-format.
configurationTag :: String
configurationTag	= "configuration"

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
helpTag :: String
helpTag			= "help"

-- | Input-format.
movesTag :: String
movesTag		= "moves"

-- | Input-format.
pgnTag :: String
pgnTag			= "pgn"

-- | The type of an object that the user may want to be printed.
data PrintObject
	= Board
	| Configuration
	| EPD
	| FEN
	| Game
	| Help
	| Moves
	| PGN
	deriving Eq

instance Control.DeepSeq.NFData PrintObject where
	rnf _	= ()

instance Show PrintObject where
	showsPrec _ printObject	= showString $ case printObject of
		Board		-> boardTag
		Configuration	-> configurationTag
		EPD		-> epdTag
		FEN		-> fenTag
		Game		-> gameTag
		Help		-> helpTag
		Moves		-> movesTag
		PGN		-> pgnTag

instance Read PrintObject where
	readsPrec _ s	= case Control.Arrow.first Data.List.Extra.lower `map` lex s of
		[("board", remainder)]		-> [(Board, remainder)]
		[("configuration", remainder)]	-> [(Configuration, remainder)]
		[("epd", remainder)]		-> [(EPD, remainder)]
		[("fen", remainder)]		-> [(FEN, remainder)]
		[("game", remainder)]		-> [(Game, remainder)]
		[("help", remainder)]		-> [(Help, remainder)]
		[("moves", remainder)]		-> [(Moves, remainder)]
		[("pgn", remainder)]		-> [(PGN, remainder)]
		_				-> []	-- No parse.

-- | The constant unordered list of possible values.
range :: [PrintObject]
range	= [Board, Configuration, EPD, FEN, Game, Help, Moves, PGN]

-- | Replace the first word of the specified string with the name of a command of which it is an unambiguous case-insensitive prefix.
autoComplete :: ShowS
autoComplete	= uncurry (++) . Control.Arrow.first (
	\word -> case [
		tag |
			tag	<- [
				boardTag,
				configurationTag,
				epdTag,
				fenTag,
				gameTag,
				helpTag,
				movesTag
			],
			Data.List.Extra.lower word `Data.List.isPrefixOf` Data.List.Extra.lower tag
	] of
		[tag]	-> tag
		_	-> word
 ) . break Data.Char.isSpace . Data.List.Extra.trimStart

