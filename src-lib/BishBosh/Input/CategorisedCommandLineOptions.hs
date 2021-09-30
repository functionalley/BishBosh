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

 [@DESCRIPTION@]

	* Categorises command-line options according to their nature:

		** The optional path to a single configuration-file.

		** Requests for information which doesn't require access to the configuration.

		** Requests for information which requires access to the configuration.

		** Options-mutators.

	* These categories are processed in a specific order, & some terminate the application.
-}

module BishBosh.Input.CategorisedCommandLineOptions(
-- * Types
-- ** Type-synonyms
	IOAction,
	ContextualIOAction,
	OptionsMutator,
--	Transformation,
-- ** Data-types
	CategorisedCommandLineOptions(
--		MkCategorisedCommandLineOptions,
		getMaybeConfigLocationParameter,
		getIOActions,
		getContextualIOActions,
		getOptionsMutators
	),
-- * Functions
-- ** Mutators
	setConfigLocation,
	prependIOAction,
	prependContextualIOAction,
	prependOptionsMutator
) where

import qualified	BishBosh.Property.Empty	as Property.Empty
import qualified	System.FilePath

-- | Type-synonym.
type IOAction	= IO ()

-- | Type-synonym.
type ContextualIOAction options	= options -> IOAction

-- | Type-synonym.
type OptionsMutator options	= options -> options

-- | The set of ordered command-line options partitioned into categories.
data CategorisedCommandLineOptions options	= MkCategorisedCommandLineOptions {
	getMaybeConfigLocationParameter	:: Maybe System.FilePath.FilePath,	-- ^ A command-line option which specifies the location of some configuration.
	getIOActions			:: [IOAction],				-- ^ Command-line options which request an IO-action. N.B. they don't have access to the configuration.
	getContextualIOActions		:: [ContextualIOAction options],	-- ^ Command-line options which request an IO-action, which has access to the configuration.
	getOptionsMutators		:: [OptionsMutator options]		-- ^ Command-line options which directly specify configuration-parameters.
}

instance Property.Empty.Empty (CategorisedCommandLineOptions options) where
	empty	= MkCategorisedCommandLineOptions {
		getMaybeConfigLocationParameter	= Property.Empty.empty,
		getIOActions			= Property.Empty.empty,
		getContextualIOActions		= Property.Empty.empty,
		getOptionsMutators		= Property.Empty.empty
	}

-- | Transformation
type Transformation options	= CategorisedCommandLineOptions options -> CategorisedCommandLineOptions options

-- | Mutator. CAVEAT: overwrites any previous specification.
setConfigLocation :: System.FilePath.FilePath -> Transformation options
setConfigLocation filePath categorisedCommandLineOptions	= categorisedCommandLineOptions { getMaybeConfigLocationParameter = Just filePath }

-- | Mutator.
prependIOAction :: IOAction -> Transformation options
prependIOAction ioAction categorisedCommandLineOptions	= categorisedCommandLineOptions { getIOActions = ioAction : getIOActions categorisedCommandLineOptions }

-- | Mutator.
prependContextualIOAction :: ContextualIOAction options -> Transformation options
prependContextualIOAction contextualIOAction categorisedCommandLineOptions	= categorisedCommandLineOptions { getContextualIOActions = contextualIOAction : getContextualIOActions categorisedCommandLineOptions }

-- | Mutator.
prependOptionsMutator :: OptionsMutator options -> Transformation options
prependOptionsMutator optionsMutator categorisedCommandLineOptions	= categorisedCommandLineOptions { getOptionsMutators = optionsMutator : getOptionsMutators categorisedCommandLineOptions }

