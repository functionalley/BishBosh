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

 [@DESCRIPTION@]	To facilitate processing of command-line options.
-}

module BishBosh.Input.CommandLineOption(
-- * Types
-- ** Type-synonyms
	Flag,
-- ** Data-types
	CommandLineOption(
--		IOAction,
--		OptionsMutator
	),
-- * Constants
	longFlagPrefix,
-- * Functions
	partition3,
	getArgs,
--	read',
	readArg,
	readBoundedIntegral,
-- ** Constructors
	mkIOAction,
	mkOptionsMutator,
	mkConfigLocationParameter
) where

import qualified	BishBosh.Data.Exception	as Data.Exception
import qualified	Control.Exception
import qualified	Data.List
import qualified	Data.Maybe

-- | Synonym.
type Flag	= String

-- | The varieties of command-line option.
data CommandLineOption options
	= IOAction (IO ())			-- ^ A command-line option which requests an IO-action.
	| OptionsMutator (options -> options)	-- ^ A command-line option which directly specifies a configuration-parameter.
	| ConfigLocationParameter String	-- ^ A command-line option which specifies the location of some configuration.

-- | Construct an 'IOAction' command-line option, from the required action.
mkIOAction :: IO () -> CommandLineOption options
mkIOAction	= IOAction

-- | Construct an 'OptionsMutator' command-line option from a handler-function.
mkOptionsMutator :: (options -> options) -> CommandLineOption options
mkOptionsMutator	= OptionsMutator

-- | Construct a 'ConfigLocationParameter' command-line option, from the command-line flag used to denote it, & a string explaining its argument-type.
mkConfigLocationParameter :: String -> CommandLineOption options
mkConfigLocationParameter	= ConfigLocationParameter

-- | Partition a list of 'CommandLineOption' according to their data-constructor.
partition3 :: [CommandLineOption options] -> ([IO ()], [options -> options], Maybe String)
partition3 categories	= (
	[action | IOAction action <- categories] {-list-comprehension-},
	[mutator | OptionsMutator mutator <- categories] {-list-comprehension-},
	Data.Maybe.listToMaybe [s | ConfigLocationParameter s <- categories]	-- List-comprehension.
 )

-- | The prefix used to denote the long form of a command-line flag.
longFlagPrefix :: Flag
longFlagPrefix	= "--"

{- |
	* Return the list of arguments extracted from the command-line, which match the specified long flag preceded by "--".

	* CAVEAT:
		to match a long flag, all unique abbreviations must also be supplied.
		doesn't cope with short flags preceded by '-'.
-}
getArgs :: [Flag] -> [String] -> [String]
getArgs flags	= slave where
	slave []	= []
	slave (x : xs)
		| x `elem` flags	= case xs of
			s : remainder	-> s : slave remainder {-recurse-}
			[]		-> Control.Exception.throw . Data.Exception.mkInsufficientData . showString "option " $ shows x " requires an argument."
		| any (
			(`Data.List.isPrefixOf` x) . (++ "=")
		) flags			= case dropWhile (/= '=') x of
			_ : remainder	-> remainder : slave xs
			[]		-> Control.Exception.throw . Data.Exception.mkNullDatum . showString "option " $ shows x " requires a non-null argument."
		| otherwise		= slave xs	-- Recurse.

-- | On failure to parse the specified string, returns an explanatory error-message.
read' :: Read a => String -> String -> a
read' errorMessage s	= case reads s of
	[(x, "")]	-> x
	_		-> Control.Exception.throw . Data.Exception.mkParseFailure . showString errorMessage $ shows s "."

-- | On failure to parse a command-line argument, returns an explanatory error.
readArg :: Read a => String -> a
readArg	= read' "failed to parse command-line argument "

-- | Reads a bounded integral from the command-line, guarding against overflow.
readBoundedIntegral :: Integral i => String -> i
readBoundedIntegral s
	| fromIntegral bounded /= unbounded	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.CommandLineOption.readBoundedIntegral:\tintegral value exceeds permissible bounds; " $ shows unbounded "."
	| otherwise				= bounded
	where
		unbounded	= readArg s
		bounded		= fromInteger unbounded

