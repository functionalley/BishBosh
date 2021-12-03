{-# LANGUAGE LambdaCase #-}
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
--		ConfigLocationParameter,
--		IOAction,
--		OptionsMutator
	),
-- * Constants
	longFlagPrefix,
-- * Functions
	categorise,
	getArgs,
--	read',
	readArg,
	readBoundedIntegral,
-- ** Constructors
	mkConfigLocationParameter,
	mkIOAction,
	mkContextualIOAction,
	mkOptionsMutator
) where

import qualified	BishBosh.Data.Exception				as Data.Exception
import qualified	BishBosh.Input.CategorisedCommandLineOptions	as Input.CategorisedCommandLineOptions
import qualified	BishBosh.Property.Empty				as Property.Empty
import qualified	Control.Exception
import qualified	Data.List
import qualified	System.FilePath

-- | Synonym.
type Flag	= String

-- | The sum-type of categories of command-line option.
data CommandLineOption options
	= ConfigLocationParameter System.FilePath.FilePath					-- ^ A command-line option which specifies the location of some configuration.
	| IOAction Input.CategorisedCommandLineOptions.IOAction					-- ^ A command-line option which requests an IO-action. N.B. it doesn't have access to the configuration.
	| ContextualIOAction (Input.CategorisedCommandLineOptions.ContextualIOAction options)	-- ^ A command-line option which requests an IO-action, which has access to the configuration.
	| OptionsMutator (Input.CategorisedCommandLineOptions.OptionsMutator options)		-- ^ A command-line option which directly specifies a configuration-parameter.

-- | Constructor.
mkConfigLocationParameter :: System.FilePath.FilePath -> CommandLineOption options
mkConfigLocationParameter	= ConfigLocationParameter

-- | Constructor.
mkIOAction :: Input.CategorisedCommandLineOptions.IOAction -> CommandLineOption options
mkIOAction	= IOAction

-- | Constructor.
mkContextualIOAction :: Input.CategorisedCommandLineOptions.ContextualIOAction options -> CommandLineOption options
mkContextualIOAction	= ContextualIOAction

-- | Constructor.
mkOptionsMutator :: Input.CategorisedCommandLineOptions.OptionsMutator options -> CommandLineOption options
mkOptionsMutator	= OptionsMutator

{- |
	* Partition a list of /CommandLineOption/s according to their data-constructor.

	* N.B.: preserves the order of all specifications sharing a data-constructor.
-}
categorise :: [CommandLineOption options] -> Input.CategorisedCommandLineOptions.CategorisedCommandLineOptions options
categorise	= foldr (
	\case
		ConfigLocationParameter f	-> Input.CategorisedCommandLineOptions.setConfigLocation f	-- CAVEAT: overwrites any previous specification.
		IOAction a			-> Input.CategorisedCommandLineOptions.prependIOAction a
		ContextualIOAction a		-> Input.CategorisedCommandLineOptions.prependContextualIOAction a
		OptionsMutator m		-> Input.CategorisedCommandLineOptions.prependOptionsMutator m
 ) Property.Empty.empty

-- | The prefix used to denote the long form of a command-line flag.
longFlagPrefix :: Flag
longFlagPrefix	= "--"

{- |
	* Return the list of arguments extracted from the specified command-line, which match the specified flag.

	* CAVEAT:
		All unique abbreviations must also be supplied.
		Doesn't cope with short flags preceded by '-'.
-}
getArgs
	:: [Flag]	-- ^ The list acceptible abbreviations for the required flag, each preceded by "--".
	-> [String]	-- ^ The command-line arguments to search.
	-> [String]	-- ^ The matching argument-values.
getArgs flags	= slave where
	slave :: [String] -> [String]
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

-- | Parse the specified string, throwing an exception on failure.
read' :: Read a => String -> String -> a
read' errorMessage s	= case reads s of
	[(x, "")]	-> x
	_		-> Control.Exception.throw . Data.Exception.mkParseFailure . showString errorMessage $ shows s "."

-- | Parse the specified string, returning the specified explanatory error-message on failure.
readArg :: Read a => String -> a
readArg	= read' "failed to parse command-line argument "

-- | Reads a bounded integral value from the command-line, guarding against overflow.
readBoundedIntegral :: Integral i => String -> i
readBoundedIntegral s
	| fromIntegral bounded /= unbounded	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.CommandLineOption.readBoundedIntegral:\tintegral value exceeds permissible bounds; " $ shows unbounded "."
	| otherwise				= bounded
	where
		unbounded	= readArg s
		bounded		= fromInteger unbounded

