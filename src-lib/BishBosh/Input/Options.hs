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
data Options column pieceSquareValue row x y	= MkOptions {
	getMaybeMaximumPlies	:: Maybe Type.Count.NPlies,						-- ^ The maximum number of plies before the game is terminated; required for profiling the application.
	getMaybeRandomSeed	:: Maybe RandomSeed,							-- ^ Optionally seed the pseudo-random number-generator to produce a repeatable sequence.
	getEvaluationOptions	:: Input.EvaluationOptions.EvaluationOptions pieceSquareValue x y,	-- ^ The single set of options by which all automated /move/s are evaluated.
	getSearchOptions	:: Input.SearchOptions.SearchOptions,					-- ^ The options by which to automatically select /move/s.
	getIOOptions		:: Input.IOOptions.IOOptions row column					-- ^ The /ioOptions/ by which to receive commands & present results.
} deriving (Eq, Show)

instance (
	Control.DeepSeq.NFData	column,
	Control.DeepSeq.NFData	pieceSquareValue,
	Control.DeepSeq.NFData	row,
	Control.DeepSeq.NFData	x,
	Control.DeepSeq.NFData	y
 ) => Control.DeepSeq.NFData (Options column pieceSquareValue row x y) where
	rnf MkOptions {
		getMaybeMaximumPlies	= maybeMaximumPlies,
		getMaybeRandomSeed	= maybeRandomSeed,
		getEvaluationOptions	= evaluationOptions,
		getSearchOptions	= searchOptions,
		getIOOptions		= ioOptions
	} = Control.DeepSeq.rnf (maybeMaximumPlies, maybeRandomSeed, evaluationOptions, searchOptions, ioOptions)

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Real	pieceSquareValue,
	Show	column,
	Show	pieceSquareValue,
	Show	row
 ) => Property.ShowFloat.ShowFloat (Options column pieceSquareValue row x y) where
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

instance (Num column, Num row) => Data.Default.Default (Options column pieceSquareValue row x y) where
	def = MkOptions {
		getMaybeMaximumPlies	= Nothing,
		getMaybeRandomSeed	= Nothing,
		getEvaluationOptions	= Data.Default.def,
		getSearchOptions	= Data.Default.def,
		getIOOptions		= Data.Default.def
	}

instance (
	Enum		x,
	Enum		y,
	Fractional	pieceSquareValue,
	HXT.XmlPickler	column,
	HXT.XmlPickler	row,
	Integral	column,
	Integral	row,
	Ord		pieceSquareValue,
	Ord		x,
	Ord		y,
	Real		pieceSquareValue,
	Show		column,
	Show		pieceSquareValue,
	Show		row
 ) => HXT.XmlPickler (Options column pieceSquareValue row x y) where
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
	-> Input.EvaluationOptions.EvaluationOptions pieceSquareValue x y
	-> Input.SearchOptions.SearchOptions
	-> Input.IOOptions.IOOptions row column
	-> Options column pieceSquareValue row x y
mkOptions maybeMaximumPlies maybeRandomSeed evaluationOptions searchOptions ioOptions
	| Just maximumPlies	<- maybeMaximumPlies
	, maximumPlies <= 0	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.Options.mkOptions:\t" . showString maximumPliesTag . Text.ShowList.showsAssociation $ shows maximumPlies " must exceed zero."
	| otherwise	= MkOptions {
		getMaybeMaximumPlies	= maybeMaximumPlies,
		getMaybeRandomSeed	= maybeRandomSeed,
		getEvaluationOptions	= evaluationOptions,
		getSearchOptions	= searchOptions,
		getIOOptions		= ioOptions
	}

-- | The type of a function used to transform 'Options'.
type Transformation column pieceSquareValue row x y	= Options column pieceSquareValue row x y -> Options column pieceSquareValue row x y

-- | Mutator.
setMaybeOutputConfigFilePath :: Maybe System.FilePath.FilePath -> Transformation column pieceSquareValue row x y
setMaybeOutputConfigFilePath maybeOutputConfigFilePath options@MkOptions { getIOOptions	= ioOptions }	= options {
	getIOOptions	= Input.IOOptions.setMaybeOutputConfigFilePath maybeOutputConfigFilePath ioOptions
}

-- | Mutator.
setMaybeRandomSeed :: Maybe RandomSeed -> Transformation column pieceSquareValue row x y
setMaybeRandomSeed maybeRandomSeed options	= options {
	getMaybeRandomSeed	= maybeRandomSeed
}

-- | Mutator.
setMaybePersistence :: Maybe (System.FilePath.FilePath, Bool) -> Transformation column pieceSquareValue row x y
setMaybePersistence maybePersistence options@MkOptions { getIOOptions = ioOptions }	= options {
	getIOOptions	= ioOptions {
		Input.IOOptions.getMaybePersistence	= maybePersistence
	}
}

-- | Mutator.
setVerbosity :: Input.Verbosity.Verbosity -> Transformation column pieceSquareValue row x y
setVerbosity verbosity options@MkOptions { getIOOptions = ioOptions }	= options {
	getIOOptions	= Input.IOOptions.setVerbosity verbosity ioOptions
}

-- | Mutator.
setEitherNativeUIOrCECPOptions :: Input.UIOptions.EitherNativeUIOrCECPOptions row column -> Transformation column pieceSquareValue row x y
setEitherNativeUIOrCECPOptions eitherNativeUIOrCECPOptions options@MkOptions { getIOOptions = ioOptions }	= options {
	getIOOptions	= Input.IOOptions.setEitherNativeUIOrCECPOptions eitherNativeUIOrCECPOptions ioOptions
}

-- | Mutator.
setMaybePrintMoveTree :: Maybe Property.Arboreal.Depth -> Transformation column pieceSquareValue row x y
setMaybePrintMoveTree maybePrintMoveTree options@MkOptions { getIOOptions = ioOptions }	= options {
	getIOOptions	= Input.IOOptions.setMaybePrintMoveTree maybePrintMoveTree ioOptions
}

-- | Mutator.
swapSearchDepth :: Transformation column pieceSquareValue row x y
swapSearchDepth options@MkOptions { getSearchOptions = searchOptions }	= options {
	getSearchOptions	= Input.SearchOptions.swapSearchDepth searchOptions
}

