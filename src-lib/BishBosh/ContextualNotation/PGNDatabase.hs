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

 [@DESCRIPTION@]	Parses a PGN-database.
-}

module BishBosh.ContextualNotation.PGNDatabase(
-- * Types
-- ** Type-synonyms
	PGNDatabase,
-- * Functions
--	parser,
--	parse,
	parseIO
 ) where

import qualified	BishBosh.ContextualNotation.PGN			as ContextualNotation.PGN
import qualified	BishBosh.ContextualNotation.StandardAlgebraic	as ContextualNotation.StandardAlgebraic
import qualified	BishBosh.Types					as T
import qualified	Control.DeepSeq
import qualified	System.FilePath
import qualified	System.IO

#ifdef USE_POLYPARSE
import qualified	BishBosh.Text.Poly				as Text.Poly
#if USE_POLYPARSE == 1
import qualified	Text.ParserCombinators.Poly.Lazy		as Poly
#else /* Plain */
import qualified	Text.ParserCombinators.Poly.Plain		as Poly
#endif
#else /* Parsec */
import qualified	Text.ParserCombinators.Parsec			as Parsec
import			Text.ParserCombinators.Parsec((<?>))
#endif

-- | Self-documentation.
type PGNDatabase x y	= [ContextualNotation.PGN.PGN x y]

-- | Parse a PGN-database.
parser :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Show	x,
	Show	y
 )
	=> ContextualNotation.PGN.IsStrictlySequential
	-> ContextualNotation.StandardAlgebraic.ValidateMoves
	-> [ContextualNotation.PGN.Tag]
#ifdef USE_POLYPARSE
	-> Text.Poly.TextParser (PGNDatabase x y)
{-# SPECIALISE parser :: ContextualNotation.PGN.IsStrictlySequential -> ContextualNotation.StandardAlgebraic.ValidateMoves -> [ContextualNotation.PGN.Tag] -> Text.Poly.TextParser (PGNDatabase T.X T.Y) #-}
parser isStrictlySequential validateMoves identificationTags	= Poly.manyFinally' parser' $ Text.Poly.spaces >> Poly.eof
#else /* Parsec */
	-> Parsec.Parser (PGNDatabase x y)
{-# SPECIALISE parser :: ContextualNotation.PGN.IsStrictlySequential -> ContextualNotation.StandardAlgebraic.ValidateMoves -> [ContextualNotation.PGN.Tag] -> Parsec.Parser (PGNDatabase T.X T.Y) #-}
parser isStrictlySequential validateMoves identificationTags	= Parsec.manyTill parser' (Parsec.try $ Parsec.spaces >> Parsec.try Parsec.eof)	<?> "PGN-database"
#endif
	where
		parser'	= ContextualNotation.PGN.parser isStrictlySequential validateMoves identificationTags

-- | Reads a PGN-database from the specified string.
parse :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Show	x,
	Show	y
 )
	=> String	-- ^ The name of the specified database.
	-> ContextualNotation.PGN.IsStrictlySequential
	-> ContextualNotation.StandardAlgebraic.ValidateMoves
	-> [ContextualNotation.PGN.Tag]
	-> String	-- ^ The database-contents.
	-> Either String (PGNDatabase x y)
{-# SPECIALISE parse :: String -> ContextualNotation.PGN.IsStrictlySequential -> ContextualNotation.StandardAlgebraic.ValidateMoves -> [ContextualNotation.PGN.Tag] -> String -> Either String (PGNDatabase T.X T.Y) #-}
#ifdef USE_POLYPARSE
#if USE_POLYPARSE == 1
parse _ isStrictlySequential validateMoves identificationTags		= Right	-- N.B.: the lazy parser throws an exception rather than returning 'Either', because otherwise it can't choose whether to construct with 'Left' or 'Right' until the input has been fully parsed.
#else /* Plain */
parse name isStrictlySequential validateMoves identificationTags	= either (Left . showString "regarding " . shows name . showString ", ") Right
#endif
	. fst {-discard unparsed data-} . Poly.runParser parser'
#else /* Parsec */
parse name isStrictlySequential validateMoves identificationTags	= either (Left . showString "failed to parse; " . show) Right . Parsec.parse parser' name
#endif
	where
		parser'	= parser isStrictlySequential validateMoves identificationTags

-- | Reads a PGN-database from the specified file-path.
parseIO :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Show	x,
	Show	y
 )
	=> System.FilePath.FilePath
	-> ContextualNotation.PGN.IsStrictlySequential
	-> ContextualNotation.StandardAlgebraic.ValidateMoves
	-> System.IO.TextEncoding	-- ^ The conversion-scheme between byte-sequences & Unicode characters.
	-> [ContextualNotation.PGN.Tag]
	-> IO (Either String (PGNDatabase x y))
{-# SPECIALISE parseIO :: System.FilePath.FilePath -> ContextualNotation.PGN.IsStrictlySequential -> ContextualNotation.StandardAlgebraic.ValidateMoves -> System.IO.TextEncoding -> [ContextualNotation.PGN.Tag] -> IO (Either String (PGNDatabase T.X T.Y)) #-}
parseIO filePath isStrictlySequential validateMoves textEncoding identificationTags	= System.IO.withFile filePath System.IO.ReadMode $ \fileHandle -> do
	System.IO.hSetEncoding fileHandle textEncoding

	contents	<- System.IO.hGetContents fileHandle

	Control.DeepSeq.deepseq contents {-CAVEAT: evaluate before the file is closed-} . return $ parse filePath isStrictlySequential validateMoves identificationTags contents

