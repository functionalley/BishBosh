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

 [@DESCRIPTION@]	Defines configurable options related to i/o.
-}

module BishBosh.Input.IOOptions(
-- * Types
-- ** Type-synonyms
--	PersistenceSpecification,
--	Transformation,
-- ** Data-types
	IOOptions(
--		MkIOOptions,
		getMaybeOutputConfigFilePath,
		getMaybeMaximumPGNNames,
		getPGNOptionsList,
		getMaybePersistence,
		getUIOptions
	),
-- * Constants
	tag,
	outputConfigFilePathTag,
--	maximumPGNNamesTag,
--	persistenceTag,
--	filePathTag,
--	automaticTag,
-- * Functions
	persist,
-- ** Constructor
	mkIOOptions,
-- ** Mutators
	setMaybeOutputConfigFilePath,
	setEitherNativeUIOrCECPOptions,
	setMaybePrintMoveTree,
	updateCECPFeature,
	deleteCECPFeature,
	setVerbosity
) where

import qualified	BishBosh.Data.Exception			as Data.Exception
import qualified	BishBosh.Data.Foldable			as Data.Foldable
import qualified	BishBosh.Input.CECPFeatures		as Input.CECPFeatures
import qualified	BishBosh.Input.PGNOptions		as Input.PGNOptions
import qualified	BishBosh.Input.UIOptions		as Input.UIOptions
import qualified	BishBosh.Input.Verbosity		as Input.Verbosity
import qualified	BishBosh.Property.Arboreal		as Property.Arboreal
import qualified	BishBosh.Text.ShowColouredPrefix	as Text.ShowColouredPrefix
import qualified	BishBosh.Text.ShowList			as Text.ShowList
import qualified	BishBosh.Text.ShowPrefix		as Text.ShowPrefix
import qualified	BishBosh.Type.Count			as Type.Count
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Control.Monad
import qualified	Data.Default
import qualified	Data.Maybe
import qualified	System.FilePath
import qualified	System.IO
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT

-- | Used to qualify XML.
tag :: String
tag			= "ioOptions"

-- | Used to qualify XML.
maximumPGNNamesTag :: String
maximumPGNNamesTag	= "maximumPGNNames"

-- | Used to qualify XML.
outputConfigFilePathTag :: String
outputConfigFilePathTag	= "outputConfigFilePath"

-- | Used to qualify XML.
persistenceTag :: String
persistenceTag		= "persistence"

-- | Used to qualify XML.
filePathTag :: String
filePathTag		= "filePath"

-- | Used to qualify XML.
automaticTag :: String
automaticTag		= "automatic"

-- | The path to a file, into which game-state can be persisted (obliterating any existing content), & whether to save this state automatically after each move.
type PersistenceSpecification	= (System.FilePath.FilePath, Bool)

-- | Defines options related to i/o.
data IOOptions	= MkIOOptions {
	getMaybeOutputConfigFilePath	:: Maybe System.FilePath.FilePath,	-- ^ An optional path to a file, into which the unprocessed configuration, formatted as XML, should be written (obliterating any existing file-contents).
	getMaybeMaximumPGNNames		:: Maybe Type.Count.NGames,		-- ^ The maximum number of names to display, of matching games from the PGN-database; @Nothing@ implies unlimited. CAVEAT: pedantically, it's a number of names not a number of games.
	getPGNOptionsList		:: [Input.PGNOptions.PGNOptions],	-- ^ How to construct each PGN-database.
	getMaybePersistence		:: Maybe PersistenceSpecification,	-- ^ Optional path to a file, into which game-state can be persisted (obliterating any existing content), & whether to save this state automatically after each move.
	getUIOptions			:: Input.UIOptions.UIOptions		-- ^ Options which define the user-interface.
} deriving Eq

instance Control.DeepSeq.NFData IOOptions where
	rnf MkIOOptions {
		getMaybeOutputConfigFilePath	= maybeOutputConfigFilePath,
		getMaybeMaximumPGNNames		= maybeMaximumPGNNames,
		getPGNOptionsList		= pgnOptionsList,
		getMaybePersistence		= maybePersistence,
		getUIOptions			= uiOptions
	} = Control.DeepSeq.rnf (
		maybeOutputConfigFilePath,
		maybeMaximumPGNNames,
		pgnOptionsList,
		maybePersistence,
		uiOptions
	 )

instance Show IOOptions where
	showsPrec _ MkIOOptions {
		getMaybeOutputConfigFilePath	= maybeOutputConfigFilePath,
		getMaybeMaximumPGNNames		= maybeMaximumPGNNames,
		getPGNOptionsList		= pgnOptionsList,
		getMaybePersistence		= maybePersistence,
		getUIOptions			= uiOptions
	} = Text.ShowList.showsAssociationList' . Data.Maybe.maybe id (
		(:) . (,) outputConfigFilePathTag . shows
	 ) maybeOutputConfigFilePath . Data.Maybe.maybe id (
		(:) . (,) maximumPGNNamesTag . shows
	 ) maybeMaximumPGNNames $ Data.Maybe.maybe id (
		(:) . (,) persistenceTag . shows
	 ) maybePersistence [
		(
			showString Input.PGNOptions.tag "List",
			shows pgnOptionsList
		), (
			Input.UIOptions.tag,
			shows uiOptions
		)
	 ]

instance Data.Default.Default IOOptions where
	def = MkIOOptions {
		getMaybeOutputConfigFilePath	= Nothing,
		getMaybeMaximumPGNNames		= Nothing,
		getPGNOptionsList		= [],
		getMaybePersistence		= Nothing,
		getUIOptions			= Data.Default.def
	}

instance HXT.XmlPickler IOOptions where
	xpickle	= HXT.xpDefault Data.Default.def . HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d, e) -> mkIOOptions a b c d e,	-- Construct.
		\MkIOOptions {
			getMaybeOutputConfigFilePath	= maybeOutputConfigFilePath,
			getMaybeMaximumPGNNames		= maybeMaximumPGNNames,
			getPGNOptionsList		= pgnOptionsList,
			getMaybePersistence		= maybePersistence,
			getUIOptions			= uiOptions
		} -> (
			maybeOutputConfigFilePath,
			maybeMaximumPGNNames,
			pgnOptionsList,
			maybePersistence,
			uiOptions
		) -- Deconstruct.
	 ) $ HXT.xp5Tuple (
		HXT.xpOption $ HXT.xpTextAttr outputConfigFilePathTag {-can't be null-}
	 ) (
		HXT.xpAttrImplied maximumPGNNamesTag HXT.xpickle
	 ) HXT.xpickle {-PGNOptions-} (
		HXT.xpOption $ HXT.xpElem persistenceTag (
			HXT.xpTextAttr filePathTag `HXT.xpPair` HXT.xpDefault True (HXT.xpAttr automaticTag HXT.xpickle {-Bool-})
		)
	 ) HXT.xpickle {-UIOptions-}

-- | Smart constructor.
mkIOOptions
	:: Maybe System.FilePath.FilePath	-- ^ An optional path to a file, into which the unprocessed configuration, formatted as XML, should be written (obliterating any existing file-contents).
	-> Maybe Type.Count.NGames		-- ^ The optional maximum number of names, of matching PGN-games, to display; @Nothing@ implies unlimited.
	-> [Input.PGNOptions.PGNOptions]	-- ^ How to find & process PGN-databases.
	-> Maybe PersistenceSpecification	-- ^ Optional path to a file, into which game-state can be persisted (obliterating any existing content), & whether to save this state automatically after each move.
	-> Input.UIOptions.UIOptions
	-> IOOptions
mkIOOptions maybeOutputConfigFilePath maybeMaximumPGNNames pgnOptionsList maybePersistence uiOptions
	| Data.Maybe.maybe False (
		not . System.FilePath.isValid {-i.e. non-null on POSIX-}
	) maybeOutputConfigFilePath		= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Input.IOOptions.mkIOOptions:\tinvalid " $ showString outputConfigFilePathTag "."
	| Data.Maybe.isJust maybeMaximumPGNNames
	, null pgnOptionsList			= Control.Exception.throw . Data.Exception.mkIncompatibleData . showString "BishBosh.Input.IOOptions.mkIOOptions:\tSpecification of " $ shows maximumPGNNamesTag " is only irrelevant when at least one PGN-database has been referenced."
	| Just maximumPGNNames	<- maybeMaximumPGNNames
	, maximumPGNNames < 0			= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.IOOptions.mkIOOptions:\tThe maximum number of names, of matching PGN-games to display, can't be negative; " $ shows maximumPGNNames "."
	| not $ null duplicateFilePaths		= Control.Exception.throw . Data.Exception.mkDuplicateData . showString "BishBosh.Input.IOOptions.mkIOOptions:\tduplicate " . showString Input.PGNOptions.databaseFilePathTag . Text.ShowList.showsAssociation $ shows duplicateFilePaths "."
	| Data.Maybe.maybe False (
		not . System.FilePath.isValid {-i.e. non-null on POSIX-} . fst {-file-path-}
	) maybePersistence			= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Input.IOOptions.mkIOOptions:\tinvalid path for " $ showString persistenceTag "."
	| otherwise	= MkIOOptions {
		getMaybeOutputConfigFilePath	= System.FilePath.normalise <$> maybeOutputConfigFilePath,
		getMaybeMaximumPGNNames		= maybeMaximumPGNNames,
		getPGNOptionsList		= pgnOptionsList,
		getMaybePersistence		= Control.Arrow.first System.FilePath.normalise <$> maybePersistence,
		getUIOptions			= uiOptions
	}
	where
		duplicateFilePaths	= Data.Foldable.findDuplicates $ map (System.FilePath.normalise . Input.PGNOptions.getDatabaseFilePath) pgnOptionsList

-- | Persist the specified game to file.
persist
	:: Show game
	=> IOOptions
	-> Bool	-- ^ Verbose.
	-> game
	-> IO () 
persist MkIOOptions {
	getMaybePersistence	= maybePersistence
} verbose game = Data.Maybe.maybe (
	return {-to IO-monad-} ()
 ) (
	\(filePath, automatic) -> Control.Monad.when automatic . Control.Exception.catch (
		do
			System.IO.withFile filePath System.IO.WriteMode (`System.IO.hPrint` game)

			Control.Monad.when verbose . System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixInfo . showString "the game-state has been saved in " $ shows filePath "."
	) $ \e -> System.IO.hPutStrLn System.IO.stderr . Text.ShowPrefix.showsPrefixError $ show (e :: Control.Exception.SomeException)
 ) maybePersistence

-- | The type of a function used to transform 'IOOptions'.
type Transformation	= IOOptions -> IOOptions

-- | Mutator.
setMaybeOutputConfigFilePath :: Maybe System.FilePath.FilePath -> Transformation
setMaybeOutputConfigFilePath maybeOutputConfigFilePath ioOptions
	| Data.Maybe.maybe False (
		not . System.FilePath.isValid {-i.e. non-null on POSIX-}
	) maybeOutputConfigFilePath	= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Input.IOOptions.setMaybeOutputConfigFilePath:\tinvalid " $ showString outputConfigFilePathTag "."
	| otherwise	= ioOptions {
		getMaybeOutputConfigFilePath	= maybeOutputConfigFilePath
	}

-- | Mutator.
setEitherNativeUIOrCECPOptions :: Input.UIOptions.EitherNativeUIOrCECPOptions -> Transformation
setEitherNativeUIOrCECPOptions eitherNativeUIOrCECPOptions ioOptions@MkIOOptions { getUIOptions = uiOptions }	= ioOptions {
	getUIOptions	= uiOptions {
		Input.UIOptions.getEitherNativeUIOrCECPOptions	= eitherNativeUIOrCECPOptions
	}
}

-- | Mutator.
setMaybePrintMoveTree :: Maybe Property.Arboreal.Depth -> Transformation
setMaybePrintMoveTree maybePrintMoveTree ioOptions@MkIOOptions { getUIOptions = uiOptions }	= ioOptions {
	getUIOptions	= uiOptions {
		Input.UIOptions.getMaybePrintMoveTree	= maybePrintMoveTree
	}
}

-- | Mutator.
updateCECPFeature :: Input.CECPFeatures.Feature -> Transformation
updateCECPFeature feature ioOptions@MkIOOptions { getUIOptions = uiOptions }	= ioOptions {
	getUIOptions	= Input.UIOptions.updateCECPFeature feature uiOptions
}

-- | Mutator.
deleteCECPFeature :: Input.CECPFeatures.Feature -> Transformation
deleteCECPFeature feature ioOptions@MkIOOptions { getUIOptions = uiOptions }	= ioOptions {
	getUIOptions	= Input.UIOptions.deleteCECPFeature feature uiOptions
}

-- | Mutator.
setVerbosity :: Input.Verbosity.Verbosity -> Transformation
setVerbosity verbosity ioOptions@MkIOOptions { getUIOptions = uiOptions }	= ioOptions {
	getUIOptions	= uiOptions {
		Input.UIOptions.getVerbosity	= verbosity
	}
}

