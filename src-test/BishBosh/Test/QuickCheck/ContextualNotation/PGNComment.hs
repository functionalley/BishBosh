{-# OPTIONS_GHC -fno-warn-orphans #-}
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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties.
-}

module BishBosh.Test.QuickCheck.ContextualNotation.PGNComment(
-- * Constants
	results
) where

import qualified	BishBosh.ContextualNotation.PGNComment	as ContextualNotation.PGNComment
import qualified	Test.QuickCheck

#ifndef USE_POLYPARSE
import			Control.Arrow((|||))
import qualified	Text.ParserCombinators.Parsec
#elif USE_POLYPARSE == 1
import qualified	Text.ParserCombinators.Poly.Lazy	as Poly
#else /* Plain */
import			Control.Arrow((|||))
import qualified	Text.ParserCombinators.Poly.Plain	as Poly
#endif

instance Test.QuickCheck.Arbitrary ContextualNotation.PGNComment.PGNComment where
	arbitrary	= Test.QuickCheck.arbitrary >>= (
		\s -> Test.QuickCheck.elements [
			ContextualNotation.PGNComment.BlockComment $ filter (`notElem` ['\\', ContextualNotation.PGNComment.blockCommentEnd]) s,
			ContextualNotation.PGNComment.LineComment $ filter (`notElem` ['\\', ContextualNotation.PGNComment.lineCommentEnd]) s
		]
	 )
-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: ContextualNotation.PGNComment.PGNComment -> Test.QuickCheck.Property
		f pgnComment	= Test.QuickCheck.label "PGNComment.prop_io" .
#ifdef USE_POLYPARSE
#	if USE_POLYPARSE == 1
			(== s)
#	else /* Plain */
			(const False ||| (== s))
#	endif
			. fst {-discard unparsed text-} . Poly.runParser ContextualNotation.PGNComment.parser
#else /* Parsec */
			(const False ||| (== s)) . Text.ParserCombinators.Parsec.parse ContextualNotation.PGNComment.parser "PGN parser"
#endif
			$ show pgnComment where
				s	= ContextualNotation.PGNComment.getString pgnComment
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f

 ]

