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

 [@DESCRIPTION@]	Defines the commands a user can issue.
-}

module BishBosh.UI.Command (
-- * Types
-- ** Data-types
	Command(..),
-- * Constants
	commandPrefix,
--	hintTag,
	printTag,
--	quitTag,
--	resignTag,
--	restartTag,
--	rollBackTag,
--	saveTag,
	setTag,
--	swapTag,
--	alternationTag,
--	printArgs,
--	setArgs,
--	commands,
	usageMessage,
-- * Functions
	readsCommand,
	showsCommand,
	autoComplete
 ) where

import qualified	BishBosh.Component.Move		as Component.Move
import qualified	BishBosh.Input.Options		as Input.Options
import qualified	BishBosh.Input.SearchOptions	as Input.SearchOptions
import qualified	BishBosh.UI.PrintObject		as UI.PrintObject
import qualified	BishBosh.UI.SetObject		as UI.SetObject
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Data.Char
import qualified	Data.List
import qualified	Data.List.Extra
import qualified	Data.Maybe
import qualified	Text.Printf

-- | Used to distinguish a command from a move.
commandPrefix :: Char
commandPrefix	= ':'

-- | Input-format.
hintTag :: String
hintTag		= "hint"

-- | Input-format.
printTag :: String
printTag	= "print"

-- | Input-format.
quitTag :: String
quitTag		= "quit"

-- | Input-format.
resignTag :: String
resignTag	= "resign"

-- | Input-format.
restartTag :: String
restartTag	= "restart"

-- | Input-format.
rollBackTag :: String
rollBackTag	= "rollBack"

-- | Input-format.
saveTag :: String
saveTag		= "save"

-- | Input-format.
setTag :: String
setTag		= "set"

-- | Input-format.
swapTag :: String
swapTag		= "swap"

-- | The symbol used to denote alternation.
alternationTag :: String
alternationTag	= "|"

-- | Show the arguments of a command.
printArgs :: String
printArgs	= Data.List.intercalate alternationTag $ map show UI.PrintObject.range

-- | The format of the argument to the command 'set'.
setArgs :: String
setArgs	= Data.List.intercalate alternationTag [
	showString Input.SearchOptions.searchDepthTag " <Int>"
 ]

-- | The commands that a user may issue.
data Command x y
	= Hint						-- ^ Request a move-suggestion.
	| Print UI.PrintObject.PrintObject		-- ^ Show the value of the specified object.
	| Quit						-- ^ Terminate ths application.
	| Resign					-- ^ Admit defeat.
	| Restart					-- ^ Abandon the current game, & start afresh.
	| RollBack (Maybe Component.Move.NMoves)	-- ^ Roll-back the specified number of plies.
	| Save						-- ^ Persist the current game-state.
	| Set UI.SetObject.SetObject			-- ^ I.E. mutate a configuration-value.
	| Swap						-- ^ Swap evaluation-options between the two sides.
	deriving (Eq, Show)

instance Control.DeepSeq.NFData (Command x y) where
	rnf (Print printObject)	= Control.DeepSeq.rnf printObject
	rnf (Set setObject)	= Control.DeepSeq.rnf setObject
	rnf _			= ()

-- | The data required to compose the usage-message for the available /command/s.
commands :: [(String, Maybe String, String)]
commands	= [
	(
		hintTag,
		Nothing,
		"Request a move-suggestion"
	), (
		printTag,
		Just printArgs,
		"Print the specified data"
	), (
		quitTag,
		Nothing,
		"Quit"
	), (
		resignTag,
		Nothing,
		"Resign"
	), (
		restartTag,
		Nothing,
		"Restart the game, preserving the current configuration"
	), (
		rollBackTag,
		Just "[<Int>]",
		"The number of plies to roll-back"
	), (
		saveTag,
		Nothing,
		"Save"
	), (
		setTag,
		Just setArgs,
		showString "Mutate " Input.Options.tag
	), (
		swapTag,
		Nothing,
		showString "Swap " $ shows Input.Options.tag " between the two sides"
	)
 ]

-- | A message defining the syntax of the available /command/s.
usageMessage :: String
usageMessage	= showString (
	Text.Printf.printf (showString indent $ showChar ' ' format) commandFieldWidth "Command" objectFieldWidth "Object" "Definition"
 ) $ concatMap (
	\(command, maybeArg, definition)	-> Text.Printf.printf (
		showChar '\n' . showString indent . showChar commandPrefix $ showString format "."
	) commandFieldWidth command objectFieldWidth (
		Data.Maybe.fromMaybe "" maybeArg
	) definition
 ) commands where
	indent, format :: String
	indent	= replicate 2 ' '
	format	= "%-*s%-*s%s"

	commandFieldWidth, objectFieldWidth :: Int
	commandFieldWidth	= succ . maximum $ map (\(tag, _, _) -> length tag) commands
	objectFieldWidth	= succ $ maximum [length arg | (_, Just arg, _) <- commands]

-- | Reads a /command/.
readsCommand :: String -> Either String (Command x y, String)
readsCommand s	= case Control.Arrow.first Data.List.Extra.lower `map` lex s of
	[("hint", s')]		-> Right (Hint, s')
	[("help", s')]		-> Right (Print UI.PrintObject.Help, s')	-- Include a specific abbreviation.
	[("print", s')]		-> case reads $ UI.PrintObject.autoComplete s' of
		[pair]	-> Right $ Control.Arrow.first Print pair
		_	-> Left . showString "failed to read the object to " . showString printTag . showString " from " . shows s' . showString ". Usage: \"" . showChar commandPrefix . showString printTag . showChar ' ' $ showString printArgs "\""
	[("quit", s')]		-> Right (Quit, s')
	[("resign", s')]	-> Right (Resign, s')
	[("restart", s')]	-> Right (Restart, s')
	[("save", s')]		-> Right (Save, s')
	[("set", s')]		-> case reads $ UI.SetObject.autoComplete s' of
		[pair]	-> Right $ Control.Arrow.first Set pair
		_	-> Left . showString "failed to read the object to " . showString setTag . showString " from " . shows s' . showString ". Usage: \"" . showChar commandPrefix . showString setTag . showString " (" $ showString setArgs ")\""
	[("rollback", s')]	-> case Data.List.Extra.trimStart s' of
		[]	-> Right (RollBack Nothing, s')
		s''	-> case reads s'' of
			[(nMoves, s''')]	-> Right (RollBack (Just nMoves), s''')
			_			-> Left . showString "failed to read the integral number of moves to " . showString rollBackTag . showString " from " $ show s''
	[("swap", s')]		-> Right (Swap, s')
	[]			-> Left "no command received"
	_			-> Left . showString "failed to read a command from " $ show s

-- | Shows a /command/.
showsCommand :: Command x y -> ShowS
showsCommand command	= case command of
	Hint			-> showString hintTag
	Print printObject	-> showString printTag . showChar ' ' . shows printObject
	Quit			-> showString quitTag
	Resign			-> showString resignTag
	Restart			-> showString restartTag
	RollBack maybeNMoves	-> showString rollBackTag . Data.Maybe.maybe id (\nMoves -> showChar ' ' . shows nMoves) maybeNMoves
	Save			-> showString saveTag
	Set setObject		-> showString setTag . showChar ' ' . shows setObject
	Swap			-> showString swapTag

-- | Replace the first word of the specified string with the name of a command of which it is an unambiguous case-insensitive prefix.
autoComplete :: ShowS
autoComplete	= uncurry (++) . Control.Arrow.first (
	\word -> case [
		tag |
			(tag, _, _)	<- ("help", Nothing, "") : commands,
			Data.List.Extra.lower word `Data.List.isPrefixOf` Data.List.Extra.lower tag
	] of
		[tag]	-> tag
		_	-> word
 ) . break Data.Char.isSpace . Data.List.Extra.trimStart

