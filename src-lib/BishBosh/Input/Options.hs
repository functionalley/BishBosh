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

 [@DESCRIPTION@]

	* Defines configurable options.

	* CAVEAT: whilst naively, the flexibility of each automated player owning their own independent /EvaluationOptions/, seems attractive,
	that choice then requires maintainance of two independently quantified (& large) /PositionHashQuantifiedGameTree/s.
-}

module BishBosh.Input.Options(
-- * Types
-- ** Type-synonyms
--	Transformation,
	RandomSeed,
-- ** Data-types
	Options(
--		MkOptions,
		getMaybeMaximumPlies,
		getMaybeRandomSeed,
		getEvaluationOptions,
		getSearchOptions,
		getIOOptions
	),
-- * Constants
	tag,
	maximumPliesTag,
	randomSeedTag,
-- * Functions
-- ** Constructor
	mkOptions,
-- ** Mutators
	setMaybeOutputConfigFilePath,
	setMaybeRandomSeed,
	setMaybePersistence,
	setVerbosity,
	setEitherNativeUIOrCECPOptions,
	setMaybePrintMoveTree,
	swapSearchDepth
) where

import			BishBosh.Data.Bool()		-- For 'HXT.xpickle'.
import qualified	BishBosh.Data.Exception			as Data.Exception
import qualified	BishBosh.Input.EvaluationOptions	as Input.EvaluationOptions
import qualified	BishBosh.Input.IOOptions		as Input.IOOptions
import qualified	BishBosh.Input.PGNOptions		as Input.PGNOptions
import qualified	BishBosh.Input.SearchOptions		as Input.SearchOptions
import qualified	BishBosh.Input.UIOptions		as Input.UIOptions
import qualified	BishBosh.Input.Verbosity		as Input.Verbosity
import qualified	BishBosh.Property.Arboreal		as Property.Arboreal
import qualified	BishBosh.Property.ShowFloat		as Property.ShowFloat
import qualified	BishBosh.Text.ShowList			as Text.ShowList
import qualified	BishBosh.Type.Count			as Type.Count
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Default
import qualified	Data.Maybe
import qualified	System.FilePath
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT

-- | Used to qualify XML.
tag :: String
tag			= "options"

-- | Used to qualify XML.
maximumPliesTag :: String
maximumPliesTag		= "maximumPlies"

-- | Used to qualify XML.
randomSeedTag :: String
randomSeedTag		= "randomSeed"

-- | A seed from which to construct a pseudo-random number-generator.
type RandomSeed	= Int

-- | Defines the application's options.
data Options	= MkOptions {
	getMaybeMaximumPlies	:: Maybe Type.Count.NPlies,			-- ^ The maximum number of plies before the game is terminated; required for profiling the application.
	getMaybeRandomSeed	:: Maybe RandomSeed,				-- ^ Optionally seed the pseudo-random number-generator to produce a repeatable sequence.
	getEvaluationOptions	:: Input.EvaluationOptions.EvaluationOptions,	-- ^ The single set of options by which all automated /move/s are evaluated.
	getSearchOptions	:: Input.SearchOptions.SearchOptions,		-- ^ The options by which to automatically select /move/s.
	getIOOptions		:: Input.IOOptions.IOOptions			-- ^ The /ioOptions/ by which to receive commands & present results.
} deriving (Eq, Show)

instance Control.DeepSeq.NFData Options where
	rnf MkOptions {
		getMaybeMaximumPlies	= maybeMaximumPlies,
		getMaybeRandomSeed	= maybeRandomSeed,
		getEvaluationOptions	= evaluationOptions,
		getSearchOptions	= searchOptions,
		getIOOptions		= ioOptions
	} = Control.DeepSeq.rnf (maybeMaximumPlies, maybeRandomSeed, evaluationOptions, searchOptions, ioOptions)

instance Property.ShowFloat.ShowFloat Options where
	showsFloat fromDouble MkOptions {
		getMaybeMaximumPlies	= maybeMaximumPlies,
		getMaybeRandomSeed	= maybeRandomSeed,
		getEvaluationOptions	= evaluationOptions,
		getSearchOptions	= searchOptions,
		getIOOptions		= ioOptions
	} = Text.ShowList.showsAssociationList' . Data.Maybe.maybe id (
		(:) . (,) maximumPliesTag . shows
	 ) maybeMaximumPlies $ Data.Maybe.maybe id (
		(:) . (,) randomSeedTag . shows
	 ) maybeRandomSeed [
		(
			Input.EvaluationOptions.tag,
			Property.ShowFloat.showsFloat fromDouble evaluationOptions
		), (
			Input.SearchOptions.tag,
			shows searchOptions
		), (
			Input.IOOptions.tag,
			shows ioOptions
		)
	 ]

instance Data.Default.Default Options where
	def = MkOptions {
		getMaybeMaximumPlies	= Nothing,
		getMaybeRandomSeed	= Nothing,
		getEvaluationOptions	= Data.Default.def,
		getSearchOptions	= Data.Default.def,
		getIOOptions		= Data.Default.def
	}

instance HXT.XmlPickler Options where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d, e) -> mkOptions a b c d e,	-- Construct.
		\MkOptions {
			getMaybeMaximumPlies	= maybeMaximumPlies,
			getMaybeRandomSeed	= maybeRandomSeed,
			getEvaluationOptions	= evaluationOptions,
			getSearchOptions	= searchOptions,
			getIOOptions		= ioOptions
		} -> (
			maybeMaximumPlies,
			maybeRandomSeed,
			evaluationOptions,
			searchOptions,
			ioOptions
		) -- Deconstruct.
	 ) $ HXT.xp5Tuple (
		HXT.xpAttrImplied maximumPliesTag HXT.xpickle {-NPlies-}
	 ) (
		HXT.xpAttrImplied randomSeedTag HXT.xpInt
	 ) HXT.xpickle {-EvaluationOptions-} HXT.xpickle {-SearchOptions-} HXT.xpickle {-IOOptions-}

-- | Smart constructor.
mkOptions
	:: Maybe Type.Count.NPlies	-- ^ The maximum number of plies before the game is terminated; required for profiling the application.
	-> Maybe RandomSeed		-- ^ Optionally seed the pseudo-random number-generator to produce a repeatable sequence.
	-> Input.EvaluationOptions.EvaluationOptions
	-> Input.SearchOptions.SearchOptions
	-> Input.IOOptions.IOOptions
	-> Options
mkOptions maybeMaximumPlies maybeRandomSeed evaluationOptions searchOptions ioOptions
	| Just maximumPlies	<- maybeMaximumPlies
	, maximumPlies <= 0	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.Options.mkOptions:\t" . showString maximumPliesTag . Text.ShowList.showsAssociation $ shows maximumPlies " must exceed zero."
	| Input.SearchOptions.getSortOnStandardOpeningMoveFrequency searchOptions && null (
		Input.IOOptions.getPGNOptionsList ioOptions
	)			= Control.Exception.throw . Data.Exception.mkIncompatibleData . showString "BishBosh.Input.Options.mkOptions:\tcan't implement '" . showString Input.SearchOptions.tag . showChar '.' . showString Input.SearchOptions.sortOnStandardOpeningMoveFrequencyTag . showString "' without any '" . showString Input.PGNOptions.tag . showChar '.' $ showString Input.PGNOptions.databaseFilePathTag "'"
	| otherwise	= MkOptions {
		getMaybeMaximumPlies	= maybeMaximumPlies,
		getMaybeRandomSeed	= maybeRandomSeed,
		getEvaluationOptions	= evaluationOptions,
		getSearchOptions	= searchOptions,
		getIOOptions		= ioOptions
	}

-- | The type of a function used to transform 'Options'.
type Transformation	= Options -> Options

-- | Mutator.
setMaybeOutputConfigFilePath :: Maybe System.FilePath.FilePath -> Transformation
setMaybeOutputConfigFilePath maybeOutputConfigFilePath options@MkOptions { getIOOptions	= ioOptions }	= options {
	getIOOptions	= Input.IOOptions.setMaybeOutputConfigFilePath maybeOutputConfigFilePath ioOptions
}

-- | Mutator.
setMaybeRandomSeed :: Maybe RandomSeed -> Transformation
setMaybeRandomSeed maybeRandomSeed options	= options {
	getMaybeRandomSeed	= maybeRandomSeed
}

-- | Mutator.
setMaybePersistence :: Maybe (System.FilePath.FilePath, Bool) -> Transformation
setMaybePersistence maybePersistence options@MkOptions { getIOOptions = ioOptions }	= options {
	getIOOptions	= ioOptions {
		Input.IOOptions.getMaybePersistence	= maybePersistence
	}
}

-- | Mutator.
setVerbosity :: Input.Verbosity.Verbosity -> Transformation
setVerbosity verbosity options@MkOptions { getIOOptions = ioOptions }	= options {
	getIOOptions	= Input.IOOptions.setVerbosity verbosity ioOptions
}

-- | Mutator.
setEitherNativeUIOrCECPOptions :: Input.UIOptions.EitherNativeUIOrCECPOptions -> Transformation
setEitherNativeUIOrCECPOptions eitherNativeUIOrCECPOptions options@MkOptions { getIOOptions = ioOptions }	= options {
	getIOOptions	= Input.IOOptions.setEitherNativeUIOrCECPOptions eitherNativeUIOrCECPOptions ioOptions
}

-- | Mutator.
setMaybePrintMoveTree :: Maybe Property.Arboreal.Depth -> Transformation
setMaybePrintMoveTree maybePrintMoveTree options@MkOptions { getIOOptions = ioOptions }	= options {
	getIOOptions	= Input.IOOptions.setMaybePrintMoveTree maybePrintMoveTree ioOptions
}

-- | Mutator.
swapSearchDepth :: Transformation
swapSearchDepth options@MkOptions { getSearchOptions = searchOptions }	= options {
	getSearchOptions	= Input.SearchOptions.swapSearchDepth searchOptions
}

