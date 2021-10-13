{-# LANGUAGE CPP #-}
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

	* Parses an optionally compressed file containing games encoded in PGN.

	* The parsed games can optionally be validated.

	* Permits the games to be filtered & their number capped.
-}

module BishBosh.ContextualNotation.PGNDatabase(
-- * Types
-- ** Type-synonyms
	PGNDatabase,
--	PGNPredicate,
	Decompressor,
	MaybeMaximumGames,
-- * Functions
--	parser,
--	parse,
	parseIO
 ) where

import			Control.DeepSeq(($!!))
import qualified	BishBosh.ContextualNotation.PGN			as ContextualNotation.PGN
import qualified	BishBosh.ContextualNotation.StandardAlgebraic	as ContextualNotation.StandardAlgebraic
import qualified	BishBosh.Data.Exception				as Data.Exception
import qualified	BishBosh.Type.Count				as Type.Count
import qualified	Control.Exception
import qualified	Control.Monad
import qualified	Data.Maybe
import qualified	System.Exit
import qualified	System.FilePath
import qualified	System.IO
import qualified	System.Process

#ifdef USE_POLYPARSE
import qualified	BishBosh.Text.Poly				as Text.Poly
#	if USE_POLYPARSE == 1
import qualified	Text.ParserCombinators.Poly.Lazy		as Poly
#	else /* Plain */
import qualified	Control.Arrow
import qualified	Text.ParserCombinators.Poly.Plain		as Poly
#	endif
#else /* Parsec */
import qualified	Control.Arrow
import qualified	Text.ParserCombinators.Parsec			as Parsec
import			Text.ParserCombinators.Parsec((<?>))
#endif

-- | Self-documentation.
type PGNDatabase	= [ContextualNotation.PGN.PGN]

-- | Parse a PGN-database.
parser
	:: ContextualNotation.PGN.IsStrictlySequential
	-> ContextualNotation.StandardAlgebraic.ValidateMoves
	-> [ContextualNotation.PGN.Tag]
#ifdef USE_POLYPARSE
	-> Text.Poly.TextParser PGNDatabase
parser isStrictlySequential validateMoves identificationTags	= Poly.manyFinally' parser' $ Text.Poly.spaces >> Poly.eof
#else /* Parsec */
	-> Parsec.Parser PGNDatabase
parser isStrictlySequential validateMoves identificationTags	= Parsec.manyTill parser' (Parsec.try $ Parsec.spaces >> Parsec.try Parsec.eof)	<?> "PGN-database"
#endif
	where
		parser'	= ContextualNotation.PGN.parser isStrictlySequential validateMoves identificationTags

-- | PGNPredicate used to filter the database.
type PGNPredicate	= ContextualNotation.PGN.PGN -> Bool

-- | The optional maximum number of games to read.
type MaybeMaximumGames	= Maybe Type.Count.NGames

-- | Parses a PGN-database from the specified string.
parse
	:: String		-- ^ The name of the specified database.
	-> ContextualNotation.PGN.IsStrictlySequential
	-> ContextualNotation.StandardAlgebraic.ValidateMoves
	-> [ContextualNotation.PGN.Tag]
	-> PGNPredicate		-- ^ Used to filter entries from the database.
	-> MaybeMaximumGames	-- ^ Optional maximum number of games to read from the database (after they've been filtered).
	-> String		-- ^ The database-contents.
	-> Either String PGNDatabase
#ifdef USE_POLYPARSE
#	if USE_POLYPARSE == 1
parse _ isStrictlySequential validateMoves identificationTags pgnPredicate maybeMaximumGames	= Right	-- N.B.: the lazy parser throws an exception rather than returning 'Either', because otherwise it can't choose whether to construct with 'Left' or 'Right' until the input has been fully parsed.
#	else /* Plain */
parse name isStrictlySequential validateMoves identificationTags pgnPredicate maybeMaximumGames	= Control.Arrow.left (showString "regarding " . shows name . showString ", ")
#	endif
	. fst {-discard unparsed data-} . Poly.runParser parser'
#else /* Parsec */
parse name isStrictlySequential validateMoves identificationTags pgnPredicate maybeMaximumGames	= Control.Arrow.left (showString "failed to parse; " . show) . Parsec.parse parser' name
#endif
	where
		parser'	= (
			Data.Maybe.maybe id (take . fromIntegral) maybeMaximumGames . filter pgnPredicate	-- CAVEAT: apply the filter before extracting the required number of games.
		 ) `fmap` parser isStrictlySequential validateMoves identificationTags

-- | The name of an executable used to decompress (to stdout) the PGN-file.
type Decompressor	= String

-- | Reads a PGN-database from the (optionally compressed) file-path & passes it to the parser.
parseIO
	:: System.FilePath.FilePath	-- ^ The PGN-file's location.
	-> Maybe Decompressor		-- ^ An Optional executable by which to decompress the PGN-file.
	-> ContextualNotation.PGN.IsStrictlySequential
	-> ContextualNotation.StandardAlgebraic.ValidateMoves
	-> System.IO.TextEncoding	-- ^ The conversion-scheme between byte-sequences & Unicode characters.
	-> [ContextualNotation.PGN.Tag]
	-> PGNPredicate			-- ^ Used to filter entries from the database.
	-> MaybeMaximumGames		-- ^ Optional maximum number of games to read from the database (after they've been filtered).
	-> IO (Either String PGNDatabase)
parseIO filePath maybeDecompressionCommand isStrictlySequential validateMoves textEncoding identificationTags pgnPredicate maybeMaximumGames	= parse filePath isStrictlySequential validateMoves identificationTags pgnPredicate maybeMaximumGames `fmap` Data.Maybe.maybe (
	System.IO.withFile filePath System.IO.ReadMode $ \fileHandle -> do
		System.IO.hSetEncoding fileHandle textEncoding

		contents	<- System.IO.hGetContents fileHandle

		return {-to IO-monad-} $!! contents	-- CAVEAT: evaluate the contents before the file is closed.
 ) (
	\decompressor	-> do
		(exitCode, stdOut, stdErr)	<- System.Process.readProcessWithExitCode decompressor [filePath] [{-stdIn-}]

		Control.Monad.unless (exitCode == System.Exit.ExitSuccess) . Control.Exception.throwIO . Data.Exception.mkRequestFailure $ showString "BishBosh.ContextualNotation.PGNDatabase.decompress:\t" stdErr

		return {-to IO-monad-} stdOut
 ) maybeDecompressionCommand

