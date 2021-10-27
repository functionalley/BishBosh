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

 [@DESCRIPTION@]	Exports functions to enhance the package /polyparse/.
-}

module BishBosh.Text.Poly(
-- * Types
-- ** Type-synonyms
	TextParser,
-- * Functions
	char,
	string,
	spaces,
	unsignedDecimal,
) where

import qualified	BishBosh.Data.Integral			as Data.Integral
import qualified	Data.Char

#if USE_POLYPARSE == 'L'
import qualified	Text.ParserCombinators.Poly.Lazy	as Poly
#elif USE_POLYPARSE == 'P'
import qualified	Text.ParserCombinators.Poly.Plain	as Poly
#else
#	error "USE_POLYPARSE invalid"
#endif

-- | Self-documentation.
type TextParser	= Poly.Parser Char

-- | Matches the specified char.
char :: Char -> TextParser ()
char c	= do
	_	<- Poly.satisfyMsg (== c) [c]

	return {-to Parser-monad-} ()

{- |
	* Matches the specified string.

	* N.B. this differs from /Text.Parse.word/ in that there's no requirement for the string to be a single Haskell lexical token.
-}
string :: String -> TextParser ()
string	= mapM_ char

{- |
	* Matches any number (including zero) of consecutive spaces.

	* CAVEAT: performance-hotspot.
-}
spaces :: TextParser ()
-- spaces	= Control.Monad.void . Poly.many $ Poly.satisfy Data.Char.isSpace	-- CAVEAT: poor performance ?!
spaces	= do
	_	<- Poly.many $ Poly.satisfy Data.Char.isSpace

	return {-to Parser-monad-} ()

-- | Parses an unsigned base-10 integer.
unsignedDecimal :: Num i => TextParser i
unsignedDecimal	= spaces >> Data.Integral.stringToUnsignedDecimal `fmap` Poly.many1 (Poly.satisfyMsg Data.Char.isDigit "<digit>")

