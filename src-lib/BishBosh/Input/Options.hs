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

 [@DESCRIPTION@]	Defines configurable options.
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
import			BishBosh.Data.Integral()	-- For 'HXT.XmlPickler NMoves'.
import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Data.Exception			as Data.Exception
import qualified	BishBosh.Input.EvaluationOptions	as Input.EvaluationOptions
import qualified	BishBosh.Input.IOOptions		as Input.IOOptions
import qualified	BishBosh.Input.SearchOptions		as Input.SearchOptions
import qualified	BishBosh.Input.UIOptions		as Input.UIOptions
import qualified	BishBosh.Input.Verbosity		as Input.Verbosity
import qualified	BishBosh.Property.ShowFloat		as Property.ShowFloat
import qualified	BishBosh.Property.Tree			as Property.Tree
import qualified	BishBosh.Text.ShowList			as Text.ShowList
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
data Options column criterionWeight pieceSquareValue rankValue row x y	= MkOptions {
	getMaybeMaximumPlies	:: Maybe Component.Move.NMoves,									-- ^ The maximum number of plies before the game is terminated; required for profiling the application.
	getMaybeRandomSeed	:: Maybe RandomSeed,										-- ^ Optionally seed the pseudo-random number-generator to produce a repeatable sequence.
	getEvaluationOptions	:: Input.EvaluationOptions.EvaluationOptions criterionWeight pieceSquareValue rankValue x y,	-- ^ The options by which to automatically evaluate /move/s.
	getSearchOptions	:: Input.SearchOptions.SearchOptions,								-- ^ The options by which to automatically select /move/s.
	getIOOptions		:: Input.IOOptions.IOOptions row column								-- ^ The /ioOptions/ by which to receive commands & present results.
} deriving (Eq, Show)

instance (
	Control.DeepSeq.NFData	column,
	Control.DeepSeq.NFData	criterionWeight,
	Control.DeepSeq.NFData	pieceSquareValue,
	Control.DeepSeq.NFData	rankValue,
	Control.DeepSeq.NFData	row,
	Control.DeepSeq.NFData	x,
	Control.DeepSeq.NFData	y
 ) => Control.DeepSeq.NFData (Options column criterionWeight pieceSquareValue rankValue row x y) where
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
	Real	criterionWeight,
	Real	pieceSquareValue,
	Real	rankValue,
	Show	column,
	Show	pieceSquareValue,
	Show	row
 ) => Property.ShowFloat.ShowFloat (Options column criterionWeight pieceSquareValue rankValue row x y) where
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

instance (
	Fractional	rankValue,
	Num		criterionWeight,
	Num		column,
	Num		row,
	Ord		rankValue,
	Show		rankValue
 ) => Data.Default.Default (Options column criterionWeight pieceSquareValue rankValue row x y) where
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
	Fractional	rankValue,
	HXT.XmlPickler	column,
	HXT.XmlPickler	criterionWeight,
	HXT.XmlPickler	rankValue,
	HXT.XmlPickler	row,
	Integral	column,
	Integral	row,
	Num		criterionWeight,
	Ord		pieceSquareValue,
	Ord		rankValue,
	Ord		x,
	Ord		y,
	Real		criterionWeight,
	Real		pieceSquareValue,
	Show		column,
	Show		criterionWeight,
	Show		pieceSquareValue,
	Show		rankValue,
	Show		row
 ) => HXT.XmlPickler (Options column criterionWeight pieceSquareValue rankValue row x y) where
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
		HXT.xpAttrImplied maximumPliesTag HXT.xpInt {-NMoves-}
	 ) (
		HXT.xpAttrImplied randomSeedTag HXT.xpInt
	 ) HXT.xpickle {-EvaluationOptions-} HXT.xpickle {-SearchOptions-} HXT.xpickle {-IOOptions-}

-- | Smart constructor.
mkOptions
	:: Maybe Component.Move.NMoves	-- ^ The maximum number of plies before the game is terminated; required for profiling the application.
	-> Maybe RandomSeed		-- ^ Optionally seed the pseudo-random number-generator to produce a repeatable sequence.
	-> Input.EvaluationOptions.EvaluationOptions criterionWeight pieceSquareValue rankValue x y
	-> Input.SearchOptions.SearchOptions
	-> Input.IOOptions.IOOptions row column
	-> Options column criterionWeight pieceSquareValue rankValue row x y
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
type Transformation column criterionWeight pieceSquareValue rankValue row x y	= Options column criterionWeight pieceSquareValue rankValue row x y -> Options column criterionWeight pieceSquareValue rankValue row x y

-- | Mutator.
setMaybeOutputConfigFilePath :: Maybe System.FilePath.FilePath -> Transformation column criterionWeight pieceSquareValue rankValue row x y
setMaybeOutputConfigFilePath maybeOutputConfigFilePath options@MkOptions { getIOOptions	= ioOptions }	= options {
	getIOOptions	= Input.IOOptions.setMaybeOutputConfigFilePath maybeOutputConfigFilePath ioOptions
}

-- | Mutator.
setMaybeRandomSeed :: Maybe RandomSeed -> Transformation column criterionWeight pieceSquareValue rankValue row x y
setMaybeRandomSeed maybeRandomSeed options	= options {
	getMaybeRandomSeed	= maybeRandomSeed
}

-- | Mutator.
setMaybePersistence :: Maybe (System.FilePath.FilePath, Bool) -> Transformation column criterionWeight pieceSquareValue rankValue row x y
setMaybePersistence maybePersistence options@MkOptions { getIOOptions = ioOptions }	= options {
	getIOOptions	= ioOptions {
		Input.IOOptions.getMaybePersistence	= maybePersistence
	}
}

-- | Mutator.
setVerbosity :: Input.Verbosity.Verbosity -> Transformation column criterionWeight pieceSquareValue rankValue row x y
setVerbosity verbosity options@MkOptions { getIOOptions = ioOptions }	= options {
	getIOOptions	= Input.IOOptions.setVerbosity verbosity ioOptions
}

-- | Mutator.
setEitherNativeUIOrCECPOptions :: Input.UIOptions.EitherNativeUIOrCECPOptions row column -> Transformation column criterionWeight pieceSquareValue rankValue row x y
setEitherNativeUIOrCECPOptions eitherNativeUIOrCECPOptions options@MkOptions { getIOOptions = ioOptions }	= options {
	getIOOptions	= Input.IOOptions.setEitherNativeUIOrCECPOptions eitherNativeUIOrCECPOptions ioOptions
}

-- | Mutator.
setMaybePrintMoveTree :: Maybe Property.Tree.Depth -> Transformation column criterionWeight pieceSquareValue rankValue row x y
setMaybePrintMoveTree maybePrintMoveTree options@MkOptions { getIOOptions = ioOptions }	= options {
	getIOOptions	= Input.IOOptions.setMaybePrintMoveTree maybePrintMoveTree ioOptions
}

-- | Mutator.
swapSearchDepth :: Transformation column criterionWeight pieceSquareValue rankValue row x y
swapSearchDepth options@MkOptions { getSearchOptions = searchOptions }	= options {
	getSearchOptions	= Input.SearchOptions.swapSearchDepth searchOptions
}

