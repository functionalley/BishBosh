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

 [@DESCRIPTION@]	Defines configurable options for PGN-processing.
-}

module BishBosh.Input.PGNOptions(
-- * Types
-- ** Type-synonyms
-- ** Data-types
	PGNOptions(
--		MkPGNOptions,
		getDatabaseFilePath,
		getIsStrictlySequential,
		getValidateMoves,
		getIdentificationTags,
		getMinimumPlies
	),
-- * Constants
	tag,
	databaseFilePathTag,
--	identificationTagTag,
--	minimumPliesTag,
-- * Functions
-- ** Constructor
	mkPGNOptions
) where

import qualified	BishBosh.Component.Move				as Component.Move
import qualified	BishBosh.ContextualNotation.PGN			as ContextualNotation.PGN
import qualified	BishBosh.ContextualNotation.StandardAlgebraic	as ContextualNotation.StandardAlgebraic
import qualified	BishBosh.Data.Exception				as Data.Exception
import qualified	BishBosh.Text.ShowList				as Text.ShowList
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Default
import qualified	System.FilePath
import qualified	Text.XML.HXT.Arrow.Pickle			as HXT
import qualified	ToolShed.Data.Foldable

-- | Used to qualify XML.
tag :: String
tag			= "pgnOptions"

-- | Used to qualify XML.
databaseFilePathTag :: String
databaseFilePathTag	= "databaseFilePath"

-- | Defines the command-line options.
validateMovesTag :: String
validateMovesTag	= "validateMoves"

-- | Defines the command-line options.
isStrictlySequentialTag :: String
isStrictlySequentialTag	= "isStrictlySequential"

-- | Defines the command-line options.
identificationTagTag :: String
identificationTagTag	= "identificationTag"

-- | Defines the command-line options.
minimumPliesTag :: String
minimumPliesTag		= "minimumPlies"

-- | Defines the options related to PGN.
data PGNOptions	= MkPGNOptions {
	getDatabaseFilePath	:: System.FilePath.FilePath,				-- ^ Path to a PGN-database file.
	getIsStrictlySequential	:: ContextualNotation.PGN.IsStrictlySequential,		-- ^ Whether moves with an unexpected number should be considered to be an error.
	getValidateMoves	:: ContextualNotation.StandardAlgebraic.ValidateMoves,	-- ^ Whether moves should be validated, which can become tedious if they're already known to be valid.
	getIdentificationTags	:: [ContextualNotation.PGN.Tag],			-- ^ The tags to extract from this PGN-database to form a unique composite game-identifier.
	getMinimumPlies		:: Component.Move.NMoves				-- ^ The minimum number of half moves, for the game to be considered useful; most short games result from "forfeit by disconnection".
} deriving Eq

instance Control.DeepSeq.NFData PGNOptions where
	rnf MkPGNOptions {
		getDatabaseFilePath	= databaseFilePath,
		getIsStrictlySequential	= isStrictlySequential,
		getValidateMoves	= validateMoves,
		getIdentificationTags	= identificationTags,
		getMinimumPlies		= minimumPlies
	} = Control.DeepSeq.rnf (databaseFilePath, isStrictlySequential, validateMoves, identificationTags, minimumPlies)

instance Show PGNOptions where
	showsPrec _ MkPGNOptions {
		getDatabaseFilePath	= databaseFilePath,
		getIsStrictlySequential	= isStrictlySequential,
		getValidateMoves	= validateMoves,
		getIdentificationTags	= identificationTags,
		getMinimumPlies		= minimumPlies
	} = Text.ShowList.showsAssociationList' [
		(
			databaseFilePathTag,
			shows databaseFilePath
		), (
			isStrictlySequentialTag,
			shows isStrictlySequential
		), (
			validateMovesTag,
			shows validateMoves
		), (
			showString identificationTagTag "s",
			shows identificationTags
		), (
			minimumPliesTag,
			shows minimumPlies
		)
	 ]

instance Data.Default.Default PGNOptions where
	def = MkPGNOptions {
		getDatabaseFilePath	= "pgn/bishbosh.pgn",	-- CAVEAT: rather arbitrary.
		getIsStrictlySequential	= True,
		getValidateMoves	= True,
		getIdentificationTags	= ["ECO", "Variation"],	-- CAVEAT: rather arbitrary.
		getMinimumPlies		= 1
	}

instance HXT.XmlPickler PGNOptions where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d, e) -> mkPGNOptions a b c d e,	-- Construct.
		\MkPGNOptions {
			getDatabaseFilePath	= databaseFilePath,
			getIsStrictlySequential	= isStrictlySequential,
			getValidateMoves	= validateMoves,
			getIdentificationTags	= identificationTags,
			getMinimumPlies		= minimumPlies
		} -> (databaseFilePath, isStrictlySequential, validateMoves, identificationTags, minimumPlies) -- Deconstruct.
	 ) $ HXT.xp5Tuple (
		HXT.xpTextAttr databaseFilePathTag
	 ) (
		getIsStrictlySequential def `HXT.xpDefault` HXT.xpAttr isStrictlySequentialTag HXT.xpickle {-Bool-}
	 ) (
		getValidateMoves def `HXT.xpDefault` HXT.xpAttr validateMovesTag HXT.xpickle {-Bool-}
	 ) (
		HXT.xpList . HXT.xpElem identificationTagTag $ HXT.xpTextAttr "tag"
	 ) (
		getMinimumPlies def `HXT.xpDefault` HXT.xpAttr minimumPliesTag HXT.xpickle {-NMoves-}
	 ) where
		def	= Data.Default.def

-- | Smart constructor.
mkPGNOptions
	:: System.FilePath.FilePath	-- ^ Database file-path.
	-> ContextualNotation.PGN.IsStrictlySequential
	-> ContextualNotation.StandardAlgebraic.ValidateMoves
	-> [ContextualNotation.PGN.Tag]	-- ^ Optional identification tags.
	-> Component.Move.NMoves	-- ^ The minimum plies.
	-> PGNOptions
mkPGNOptions databaseFilePath isStrictlySequential validateMoves identificationTags minimumPlies
	| not $ System.FilePath.isValid databaseFilePath	= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Input.PGNOptions.mkPGNOptions:\tinvalid " . showString databaseFilePathTag . Text.ShowList.showsAssociation $ shows databaseFilePath "."
	| any null identificationTags				= Control.Exception.throw . Data.Exception.mkNullDatum . showString "BishBosh.Input.PGNOptions.mkPGNOptions:\tno " $ shows identificationTagTag " can be null."
	| duplicateTags@(_ : _)	<- map head . filter ((/= 1) . length) $ ToolShed.Data.Foldable.gather identificationTags
	= Control.Exception.throw . Data.Exception.mkDuplicateData . showString "BishBosh.Input.PGNOptions.mkPGNOptions:\tduplicate " . showString identificationTagTag . showChar 's' . Text.ShowList.showsAssociation $ shows duplicateTags "."
	| minimumPlies < 0					= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.PGNOptions.mkPGNOptions:\t" $ shows minimumPliesTag " can't be negative."
	| otherwise						= MkPGNOptions {
		getDatabaseFilePath	= System.FilePath.normalise databaseFilePath,
		getIsStrictlySequential	= isStrictlySequential,
		getValidateMoves	= validateMoves,
		getIdentificationTags	= identificationTags,
		getMinimumPlies		= minimumPlies
	}

