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
		getMaybeDecompressor,
		getIsStrictlySequential,
		getValidateMoves,
		getTextEncoding,
		getIdentificationTags,
		getMinimumPlies,
		getMaybeMaximumGames
	),
-- * Constants
	tag,
	databaseFilePathTag,
--	decompressorTag,
--	validateMovesTag,
--	isStrictlySequentialTag,
--	identificationTagTag,
--	minimumPliesTag,
--	maximumGamesTag,
-- * Functions
-- ** Constructor
	mkPGNOptions
) where

import			BishBosh.Data.Bool()
import qualified	BishBosh.ContextualNotation.PGN			as ContextualNotation.PGN
import qualified	BishBosh.ContextualNotation.PGNDatabase		as ContextualNotation.PGNDatabase
import qualified	BishBosh.ContextualNotation.StandardAlgebraic	as ContextualNotation.StandardAlgebraic
import qualified	BishBosh.Data.Exception				as Data.Exception
import qualified	BishBosh.Data.Foldable				as Data.Foldable
import qualified	BishBosh.Text.Encoding				as Text.Encoding
import qualified	BishBosh.Text.ShowList				as Text.ShowList
import qualified	BishBosh.Type.Count				as Type.Count
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Char
import qualified	Data.Default
import qualified	Data.Maybe
import qualified	System.FilePath
import qualified	System.IO
import qualified	Text.XML.HXT.Arrow.Pickle			as HXT

-- | Used to qualify XML.
tag :: String
tag			= "pgnOptions"

-- | Used to qualify XML.
databaseFilePathTag :: String
databaseFilePathTag	= "databaseFilePath"

-- | Used to qualify XML.
decompressorTag :: String
decompressorTag		= "decompressor"

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

-- | Defines the command-line options.
maximumGamesTag :: String
maximumGamesTag		= "maximumGames"

-- | Defines the options related to PGN.
data PGNOptions	= MkPGNOptions {
	getDatabaseFilePath	:: System.FilePath.FilePath,				-- ^ Path to a PGN-database file.
	getMaybeDecompressor	:: Maybe ContextualNotation.PGNDatabase.Decompressor,	-- ^ Optional executable by which to decompress the specified file.
	getIsStrictlySequential	:: ContextualNotation.PGN.IsStrictlySequential,		-- ^ Whether moves with an unexpected number should be considered to be an error.
	getValidateMoves	:: ContextualNotation.StandardAlgebraic.ValidateMoves,	-- ^ Whether moves should be validated, which can become tedious if they're already known to be valid.
	getTextEncoding		:: System.IO.TextEncoding,				-- ^ The conversion-scheme between byte-sequences & Unicode characters.
	getIdentificationTags	:: [ContextualNotation.PGN.Tag],			-- ^ The tags to extract from this PGN-database to form a unique composite game-identifier.
	getMinimumPlies		:: Type.Count.NPlies,					-- ^ The minimum number of plies required before a recorded game is considered useful.
	getMaybeMaximumGames	:: ContextualNotation.PGNDatabase.MaybeMaximumGames	-- ^ The optional maximum number of games to read from the PGN-database.
} deriving Eq

instance Control.DeepSeq.NFData PGNOptions where
	rnf MkPGNOptions {
		getDatabaseFilePath	= databaseFilePath,
		getMaybeDecompressor	= maybeDecompressor,
		getIsStrictlySequential	= isStrictlySequential,
		getValidateMoves	= validateMoves,
		getIdentificationTags	= identificationTags,
		getMinimumPlies		= minimumPlies,
		getMaybeMaximumGames	= maybeMaximumGames
	} = Control.DeepSeq.rnf (databaseFilePath, maybeDecompressor, isStrictlySequential, validateMoves, identificationTags, minimumPlies, maybeMaximumGames)

instance Show PGNOptions where
	showsPrec _ MkPGNOptions {
		getDatabaseFilePath	= databaseFilePath,
		getMaybeDecompressor	= maybeDecompressor,
		getIsStrictlySequential	= isStrictlySequential,
		getValidateMoves	= validateMoves,
		getTextEncoding		= textEncoding,
		getIdentificationTags	= identificationTags,
		getMinimumPlies		= minimumPlies,
		getMaybeMaximumGames	= maybeMaximumGames
	} = Text.ShowList.showsAssociationList' $ Data.Maybe.maybe id (
		(:) . (,) decompressorTag . shows
	 ) maybeDecompressor [
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
			Text.Encoding.tag,
			shows textEncoding
		), (
			showString identificationTagTag "s",
			shows identificationTags
		), (
			minimumPliesTag,
			shows minimumPlies
		), (
			maximumGamesTag,
			shows maybeMaximumGames
		)
	 ]

instance Data.Default.Default PGNOptions where
	def = MkPGNOptions {
		getDatabaseFilePath	= "pgn/bishbosh.pgn",	-- CAVEAT: rather arbitrary.
		getMaybeDecompressor	= Nothing,
		getIsStrictlySequential	= True,
		getValidateMoves	= True,
		getTextEncoding		= Data.Default.def,
		getIdentificationTags	= ["ECO", "Variation"],	-- CAVEAT: rather arbitrary.
		getMinimumPlies		= 1,
		getMaybeMaximumGames	= Nothing
	}

instance HXT.XmlPickler PGNOptions where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d, e, f, g, h) -> mkPGNOptions a b c d e f g h,	-- Construct.
		\MkPGNOptions {
			getDatabaseFilePath	= databaseFilePath,
			getMaybeDecompressor	= maybeDecompressor,
			getIsStrictlySequential	= isStrictlySequential,
			getValidateMoves	= validateMoves,
			getTextEncoding		= textEncoding,
			getIdentificationTags	= identificationTags,
			getMinimumPlies		= minimumPlies,
			getMaybeMaximumGames	= maybeMaximumGames
		} -> (databaseFilePath, maybeDecompressor, isStrictlySequential, validateMoves, textEncoding, identificationTags, minimumPlies, maybeMaximumGames) -- Deconstruct.
	 ) $ HXT.xp8Tuple (
		HXT.xpTextAttr databaseFilePathTag
	 ) (
		HXT.xpOption $ HXT.xpTextAttr decompressorTag
	 ) (
		getIsStrictlySequential def `HXT.xpDefault` HXT.xpAttr isStrictlySequentialTag HXT.xpickle {-Bool-}
	 ) (
		getValidateMoves def `HXT.xpDefault` HXT.xpAttr validateMovesTag HXT.xpickle {-Bool-}
	 ) HXT.xpickle {-TextEncoding-} (
		HXT.xpList . HXT.xpElem identificationTagTag $ HXT.xpTextAttr "tag"
	 ) (
		getMinimumPlies def `HXT.xpDefault` HXT.xpAttr minimumPliesTag HXT.xpickle {-NPlies-}
	 ) (
		HXT.xpOption $ HXT.xpAttr maximumGamesTag HXT.xpickle {-NGames-}
	 ) where
		def	= Data.Default.def

-- | Smart constructor.
mkPGNOptions
	:: System.FilePath.FilePath				-- ^ Database file-path.
	-> Maybe ContextualNotation.PGNDatabase.Decompressor	-- ^ Optional name of an executable by which to decompress the specified file.
	-> ContextualNotation.PGN.IsStrictlySequential
	-> ContextualNotation.StandardAlgebraic.ValidateMoves
	-> System.IO.TextEncoding
	-> [ContextualNotation.PGN.Tag]				-- ^ Optional identification tags.
	-> Type.Count.NPlies					-- ^ The minimum plies.
	-> ContextualNotation.PGNDatabase.MaybeMaximumGames	-- ^ The optional maximum number of games to read from the database.
	-> PGNOptions
mkPGNOptions databaseFilePath maybeDecompressor isStrictlySequential validateMoves textEncoding identificationTags minimumPlies maybeMaximumGames
	| not $ System.FilePath.isValid databaseFilePath	= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Input.PGNOptions.mkPGNOptions:\tinvalid " . showString databaseFilePathTag . Text.ShowList.showsAssociation $ shows databaseFilePath "."
	| Data.Maybe.maybe False null maybeDecompressor		= Control.Exception.throw . Data.Exception.mkNullDatum . showString "BishBosh.Input.PGNOptions.mkPGNOptions:\t" $ shows decompressorTag " can't be null."
	| Data.Maybe.maybe False (
		not . all Data.Char.isAlphaNum
	) maybeDecompressor					= Control.Exception.throw . Data.Exception.mkNullDatum . showString "BishBosh.Input.PGNOptions.mkPGNOptions:\t" $ shows decompressorTag " should be alpha-numeric."
	| any null identificationTags				= Control.Exception.throw . Data.Exception.mkNullDatum . showString "BishBosh.Input.PGNOptions.mkPGNOptions:\tno " $ shows identificationTagTag " can be null."
	| not $ null duplicateIdentificationTags		= Control.Exception.throw . Data.Exception.mkDuplicateData . showString "BishBosh.Input.PGNOptions.mkPGNOptions:\tduplicate " . showString identificationTagTag . Text.ShowList.showsAssociation $ shows duplicateIdentificationTags "."
	| minimumPlies < 0					= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.PGNOptions.mkPGNOptions:\t" $ shows minimumPliesTag " can't be negative."
	| Data.Maybe.maybe False (<= 0) maybeMaximumGames	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.PGNOptions.mkPGNOptions:\t" $ shows maximumGamesTag " must be positive."
	| otherwise						= MkPGNOptions {
		getDatabaseFilePath	= System.FilePath.normalise databaseFilePath,
		getMaybeDecompressor	= maybeDecompressor,
		getIsStrictlySequential	= isStrictlySequential,
		getValidateMoves	= validateMoves,
		getTextEncoding		= textEncoding,
		getIdentificationTags	= identificationTags,
		getMinimumPlies		= minimumPlies,
		getMaybeMaximumGames	= maybeMaximumGames
	}
	where
		duplicateIdentificationTags	= Data.Foldable.findDuplicates identificationTags

