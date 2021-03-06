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

module BishBosh.Test.QuickCheck.ContextualNotation.PGN(
-- * Types
-- ** Type-synonyms
	PGN,
-- * Constants
--	ficsGamesDBGameNoTag,
--	ecoCodeTag,
	results
-- * Functions
--	arbitraryECOCode
) where

import			BishBosh.Test.QuickCheck.ContextualNotation.PGNComment()
import qualified	BishBosh.ContextualNotation.PGN			as ContextualNotation.PGN
import qualified	BishBosh.ContextualNotation.PGNComment		as ContextualNotation.PGNComment
import qualified	BishBosh.ContextualNotation.StandardAlgebraic	as ContextualNotation.StandardAlgebraic
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.Test.QuickCheck.Model.Game		as Test.QuickCheck.Model.Game
import qualified	BishBosh.Types					as T
import qualified	Data.Word
import qualified	Test.QuickCheck

#ifndef USE_POLYPARSE
import qualified	Text.ParserCombinators.Parsec
#elif USE_POLYPARSE == 1
import qualified	Text.ParserCombinators.Poly.Lazy	as Poly
#else /* Plain */
import qualified	Text.ParserCombinators.Poly.Plain	as Poly
#endif

-- | Qualifies a value.
ficsGamesDBGameNoTag :: ContextualNotation.PGN.Tag
ficsGamesDBGameNoTag	= "FICSGamesDBGameNo"

-- | Qualifies a value.
ecoCodeTag :: ContextualNotation.PGN.Tag
ecoCodeTag		= "ECO"

-- | Returns an arbitrary ECO-code.
arbitraryECOCode :: Test.QuickCheck.Gen String
arbitraryECOCode	= do
	c	<- Test.QuickCheck.elements ['A' .. 'E']
	i	<- Test.QuickCheck.elements ['0' .. '9']
	j	<- Test.QuickCheck.elements ['0' .. '9']

	return {-to Gen-monad-} [c, i, j]

-- | Defines a concrete type for testing.
type PGN	= ContextualNotation.PGN.PGN T.X T.Y

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Show	x,
	Show	y
 ) => Test.QuickCheck.Arbitrary (ContextualNotation.PGN.PGN x y) where
	{-# SPECIALISE instance Test.QuickCheck.Arbitrary PGN #-}
	arbitrary	= let
		arbitraryString :: Test.QuickCheck.Gen String
		arbitraryString	= do
			s	<- filter (
				`notElem` ['\\', '\n', '\r', ContextualNotation.PGN.quoteDelimiter]
			 ) <$> Test.QuickCheck.arbitrary

			return {-to Gen-monad-} $ if s == [ContextualNotation.PGN.unknownTagValue]
				then '\\' : s
				else s

		arbitraryWord :: Test.QuickCheck.Gen String
		arbitraryWord	= (show :: Data.Word.Word -> String) <$> Test.QuickCheck.arbitrary

		arbitraryMaybeWord :: Test.QuickCheck.Gen (Maybe String)
		arbitraryMaybeWord	= Test.QuickCheck.oneof [
			return {-to Gen-monad-} Nothing,
			fmap Just arbitraryWord
		 ]
	 in ContextualNotation.PGN.mkPGN <$> fmap Just arbitraryString {-Event-} <*> fmap Just arbitraryString {-Site-} <*> fmap (
		toEnum . abs
	 ) Test.QuickCheck.arbitrary {-Day-} <*> arbitraryMaybeWord {-Round-} <*> fmap Just arbitraryString {-White-} <*> fmap Just arbitraryString {-Black-} <*> (
		do
			ficsGamesDBGameNo	<- arbitraryWord
			ecoCode			<- arbitraryECOCode

			return {-to Gen-monad-} [
				(
					ficsGamesDBGameNoTag,	ficsGamesDBGameNo
				), (
					ecoCodeTag,		ecoCode
				)
			 ] -- Optional tags.
	 ) <*> Test.QuickCheck.arbitrary {-Game-}

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Bool -> ContextualNotation.StandardAlgebraic.ValidateMoves -> Test.QuickCheck.Model.Game.Game -> Test.QuickCheck.Property
		f isStrictlySequential validateMoves game	= Test.QuickCheck.label "PGN.prop_moveTextParser" .
#ifdef USE_POLYPARSE
#if USE_POLYPARSE == 1
			(Model.Game.listTurns game ==) . Model.Game.listTurns
#else /* Plain */
			either (const False) ((Model.Game.listTurns game ==) . Model.Game.listTurns)
#endif
			. fst {-discard unparsed text-} . Poly.runParser moveTextParser
#else /* Parsec */
			either (const False) ((Model.Game.listTurns game ==) . Model.Game.listTurns) . Text.ParserCombinators.Parsec.parse moveTextParser "Move-text parser"
#endif
			$ ContextualNotation.PGN.showsMoveText game ""
			where
				moveTextParser	= ContextualNotation.PGN.moveTextParser isStrictlySequential validateMoves
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 128 } f,
	let
		f :: Bool -> ContextualNotation.StandardAlgebraic.ValidateMoves -> PGN -> [ContextualNotation.PGNComment.PGNComment] -> Test.QuickCheck.Property
		f isStrictlySequential validateMoves pgn pgnComments	= Test.QuickCheck.label "PGN.prop_pgnParser" .
#ifdef USE_POLYPARSE
#if USE_POLYPARSE == 1
			(== pgn)
#else /* Plain */
			either (const False) (== pgn)
#endif
			. fst {-discard unparsed text-} . Poly.runParser parser
#else /* Parsec */
			either (const False) (== pgn) . Text.ParserCombinators.Parsec.parse parser "PGN parser"
#endif
			. unlines . (
				\l -> zipWith (++) l $ map (showChar '\t' . show) pgnComments ++ repeat "" {-pad comment-list with null lines-}
			) . lines $ show pgn
			where
				parser	= ContextualNotation.PGN.parser isStrictlySequential validateMoves [ficsGamesDBGameNoTag, ecoCodeTag]
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 1024 } f
 ]
