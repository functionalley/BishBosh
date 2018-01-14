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

 [@DESCRIPTION@]	Implements a parser for PGN-comments.
-}

module BishBosh.ContextualNotation.PGNComment(
-- * Types
-- ** Data-types
	PGNComment(..),
-- * Constants
--	blockCommentStart,
	blockCommentEnd,
--	lineCommentStart,
	lineCommentEnd,
-- * Functions
	blockCommentParser,
--	lineCommentParser,
	parser,
-- ** Accessors
	getString
) where

import			Control.Applicative((<|>))
import qualified	Control.Applicative

#ifdef USE_POLYPARSE
import qualified	BishBosh.Text.Poly			as Text.Poly
#if USE_POLYPARSE == 1
import qualified	Text.ParserCombinators.Poly.Lazy	as Poly
#else /* Plain */
import qualified	Text.ParserCombinators.Poly.Plain	as Poly
#endif
#else /* Parsec */
import qualified	Control.Monad
import qualified	Text.ParserCombinators.Parsec		as Parsec
import			Text.ParserCombinators.Parsec((<?>))
#endif

-- | Constant comment start-delimiter.
blockCommentStart :: Char
blockCommentStart	= '{'

-- | Constant comment end-delimiter.
blockCommentEnd :: Char
blockCommentEnd		= '}'

-- | Constant comment end-delimiter.
lineCommentStart :: Char
lineCommentStart	= ';'

-- | Constant comment end-delimiter.
lineCommentEnd :: Char
lineCommentEnd		= '\n'

-- | Represents a comment in PGN.
data PGNComment	= BlockComment String | LineComment String

instance Show PGNComment where
	showsPrec _ (BlockComment s)	= showChar blockCommentStart . showString s . showChar blockCommentEnd
	showsPrec _ (LineComment s)	= showChar lineCommentStart . showString s . showChar lineCommentEnd

-- | Accessor.
getString :: PGNComment -> String
getString (BlockComment s)	= s
getString (LineComment s)	= s

-- | Parses PGN block-comments.
blockCommentParser ::
#ifdef USE_POLYPARSE
	Text.Poly.TextParser PGNComment
blockCommentParser	= Text.Poly.spaces >> Poly.bracket (
	Text.Poly.char blockCommentStart
 ) (
	Text.Poly.char blockCommentEnd	-- CAVEAT: 'Poly.commit' here fails ?!
 ) (
	BlockComment `fmap` Control.Applicative.many (Poly.satisfyMsg (/= blockCommentEnd) "Block-comment")
 )
#else /* Parsec */
	Parsec.Parser PGNComment
blockCommentParser	= Parsec.try (
	Parsec.spaces >> Parsec.between (
		Parsec.char blockCommentStart	<?> "Block-comment start"
	) (
		Parsec.char blockCommentEnd	<?> "Block-comment end"
	) (
		BlockComment `fmap` Control.Applicative.many (Parsec.satisfy (/= blockCommentEnd)) <?> "Block-comment text"
	) <?> "Block-comment"
 )
#endif

-- | Parses PGN line-comments.
lineCommentParser ::
#ifdef USE_POLYPARSE
	Text.Poly.TextParser PGNComment
lineCommentParser	= Text.Poly.spaces >> Poly.bracket (
	Text.Poly.char lineCommentStart
 ) (
	Poly.commit $ Text.Poly.char lineCommentEnd <|> Poly.eof
 ) (
	LineComment `fmap` Control.Applicative.many (Poly.satisfyMsg (/= lineCommentEnd) "Line-comment text")
 )
#else /* Parsec */
	Parsec.Parser PGNComment
lineCommentParser	= Parsec.try (
	Parsec.spaces >> Parsec.between (
		Parsec.char lineCommentStart	<?> "Line-comment start"
	) (
		Control.Monad.void (Parsec.char lineCommentEnd <?> "EOLN") <|> (Parsec.eof <?> "EOF")
	) (
		LineComment `fmap` Control.Applicative.many (Parsec.satisfy (/= lineCommentEnd)) <?> "Line-comment text"
	) <?> "Line-comment"
 )
#endif

-- | Parses PGN-comments.
parser ::
#ifdef USE_POLYPARSE
	Text.Poly.TextParser String
#else /* Parsec */
	Parsec.Parser String
#endif
parser	= fmap getString $ blockCommentParser <|> lineCommentParser

