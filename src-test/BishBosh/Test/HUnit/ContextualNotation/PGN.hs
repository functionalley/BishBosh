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

 [@DESCRIPTION@]	Static tests.
-}

module BishBosh.Test.HUnit.ContextualNotation.PGN(
-- * Constants
	testCases
) where

import qualified	BishBosh.ContextualNotation.PGN		as ContextualNotation.PGN
import qualified	Test.HUnit
import			Test.HUnit((~?))

#ifdef USE_POLYPARSE
import qualified	BishBosh.Text.Poly			as Text.Poly
#	if USE_POLYPARSE == 'L'
import qualified	Data.Char
import qualified	Text.ParserCombinators.Poly.Lazy	as Poly
#	elif USE_POLYPARSE == 'P'
import			Control.Arrow((|||))
import qualified	Text.ParserCombinators.Poly.Plain	as Poly
#	else
#		error "USE_POLYPARSE invalid"
#	endif
#else /* Parsec */
import			Control.Arrow((|||))
import qualified	Text.ParserCombinators.Parsec		as Parsec
#endif

#if !defined(USE_POLYPARSE) || USE_POLYPARSE != 'L'
import qualified	BishBosh.Data.Exception			as Data.Exception
import qualified	Control.Exception
#endif

-- | Check the sanity of the implementation, by validating a list of static test-cases.
testCases :: Test.HUnit.Test
testCases	= Test.HUnit.test $ map (
	\(isStrictlySequential, validateMoves, s) ->
#ifdef USE_POLYPARSE
#	if USE_POLYPARSE == 'L'
	(
		\s' -> all Data.Char.isSpace s' ~? showString "Unparsed input: " (show s')
	) . snd
#	elif USE_POLYPARSE == 'P'
	(
		Control.Exception.throw . Data.Exception.mkParseFailure . showString "BishBosh.Test.HUnit.ContextualNotation.PGN.testCases:\tfailed: " . show ||| const (
			True ~? ""
		)
	) . fst
#	else
#		error "USE_POLYPARSE invalid"
#	endif
	$ Poly.runParser (
		ContextualNotation.PGN.parser isStrictlySequential validateMoves []	:: Text.Poly.TextParser ContextualNotation.PGN.PGN
	)
#else /* Parsec */
	Control.Exception.throw . Data.Exception.mkParseFailure . showString "BishBosh.Test.HUnit.ContextualNotation.PGN.testCases:\tfailed: " . show ||| const (
		True ~? ""
	) $ Parsec.parse (
		ContextualNotation.PGN.parser isStrictlySequential validateMoves []	:: Parsec.Parser ContextualNotation.PGN.PGN
	) "PGN-parser"
#endif
	s
 ) [
	(
		True,	True,	showChar '[' . showString ContextualNotation.PGN.dateTag $ ContextualNotation.PGN.showsDate (toEnum 0) "]1.e4 c5 2.d3*"	-- Reduced white space.
	), (
		True,	True,	showChar '[' . showString ContextualNotation.PGN.dateTag $ ContextualNotation.PGN.showsDate (toEnum 0) "]\n1. e4 $0 {nag} c5 $255 {nag} *\n"	-- Numeric Annotation Glyphs.
	), (
		True,	True,	showChar '[' . showString ContextualNotation.PGN.dateTag $ ContextualNotation.PGN.showsDate (toEnum 0) "]\n1. Nf3 ( 1. d4 ) c5 ( 1... Nf6 ) 2. c4 *\n"	-- Recursive Annotation Variation.
	), (
		True,	True,	showString "[ {Tag-name} " $ showString ContextualNotation.PGN.dateTag " {Tag-value} \"2018.01.01\" {Move-sequence} ] {Game unfinished} * {Block comment} ; Line comment.\n"	-- Null move-sequence.
	), (
		True,	True,	showString "[ {Tag-name} " . showString ContextualNotation.PGN.dateTag . showString " {Tag-value} " $ ContextualNotation.PGN.showsDate (toEnum 1) " {Move-sequence} ] 1. {White move} e4 {Black won} 0-1 {Block comment} ; Line comment.\n"
	), (
		True,	True,	showString "[ {Tag-name} " $ showString ContextualNotation.PGN.dateTag " {Tag-value} \"0000.00.00\" {Move-sequence} ] 1. {White move} Nf3 {Black move} c5 {White won} 1-0 { Block comment } ; Line comment {{}}.\n"
	), (
		True,	True,	showString "{Initial comment} [ {Tag-name} " . showString ContextualNotation.PGN.dateTag . showString " {Tag-value} " $ ContextualNotation.PGN.showsDate (toEnum 2) " {Move-sequence} ] 1. {White move} Nc3 {Black move} e6 {Draw} 1/2-1/2 {Block ; comment} ; Line comment.\n\n"
	), (
		True,	True,	showString "{} [ {} " . showString ContextualNotation.PGN.dateTag . showString " {} " $ ContextualNotation.PGN.showsDate (toEnum 3) " {Move-sequence} ] 1. { } Nc3 {\t} e6 {} 1/2-1/2 {} ;"	-- Null comments.
	), (
		True,	True,	showString ";Line comment 1.\n;Line comment 2.\n[" . showString ContextualNotation.PGN.dateTag $ ContextualNotation.PGN.showsDate (toEnum 4) "];Line comment 3.\n;Line comment 4.\n1. Nc3 e6 *;Line comment 5.\n;Line comment 6.\n"	-- Multiple line-comments.
	), (
		True,	True,	showChar '[' . showString ContextualNotation.PGN.dateTag $ ContextualNotation.PGN.showsDate (toEnum 0) "]\n1. e4 1... c5 *\n"	-- Black move-number.
	), (
		True,	True,	showChar '[' . showString ContextualNotation.PGN.dateTag $ ContextualNotation.PGN.showsDate (toEnum 0) "]\n1. Nf3 c5 2. c4 b6 3. Nc3 Bb7 4. e4 Nf6 $6 5. e5 Ng8 6. d4 Bxf3 7. Qxf3 ( 7. gxf3 cxd4 8. Qxd4 Nc6 9. Qe4 e6 10. f4 $14 { Pelletier-Adler, Schweiz 2000 ( ? )} ) 7... Nc6 8. d5 $5 ( 8. dxc5 bxc5 9. Qe4 Qb8 $5 ) 8... Nxe5 9. Qe2 d6 10. f4 Nd7 11. g3 g6 12. Bh3 Ngf6 ( 12... Bg7 13. Bd2 Nh6 14. O-O ) 13. Bd2 Bg7 14. O-O O-O 15. g4 ( 15. Rae1 Re8 16. g4 e6 17. dxe6 Rxe6 18. Qd3 $44 ) 15... h6 16. Rae1 a6 ( 16... Re8 17. g5 $6 ( 17. Kh1 ) 17... hxg5 18. fxg5 Nh7 19. Rxf7 $2 Bd4+ ) 17. a4 ( 17. Qxe7 Qxe7 18. Rxe7 Rfe8 19. Rfe1 Rxe7 20. Rxe7 Kf8 21. Re1 Ra7 $14 ) 17... b5 ( 17... Re8 18. Kh1 $44 ) 18. axb5 axb5 19. cxb5 Nb6 20. Bg2 Ra7 21. g5 hxg5 22. f5 $1 Nbd7 23. fxg6 fxg6 24. Bxg5 ( 24. Qxe7 Qxe7 25. Rxe7 $16 ) ( 24. Qe6+ Kh7 25. Ne4 $6 Nxe4 26. Bxe4 Bd4+ 27. Kg2 Nf6 28. Bxg5 $14 ) 24... Ne5 25. Bh3 Kh7 26. Be6 Bh6 27. Bxh6 Kxh6 28. Rf4 $2 ( 28. b4 $1 $18 ) 28... Ra8 29. Ref1 Kg7 30. Ne4 Qb6 31. Ng3 Nh7 32. Qg2 Qxb5 33. h4 Kh8 $138 34. h5 Rxf4 35. Rxf4 Ra1+ 36. Kh2 g5 $19 37. Rf2 Nf6 38. Ne4 Qc4 39. Bf5 ( 39. Nxf6 Qh4+ 40. Bh3 exf6 $19 ) ( 39. Qxg5 Qxe4 40. Qh6+ Qh7 41. Rxf6 Qxh6 42. Rxh6+ Kg7 $19 ) ( 39. h6 Qxe4 40. Qxe4 Nxe4 41. Rf8+ Kh7 42. Bf5+ Kxh6 43. Bxe4 Ra4 $19 ) 39... Neg4+ $2 ( 39... Qd4 $19 ) 40. Bxg4 Qxe4 41. Qxe4 Nxe4 42. Rf7 Nf6 43. Be6 ( 43. Bf5 Rf1 ) 43... Nxh5 44. Rxe7 Rd1 $2 ( 44... Nf6 45. Rf7 Rf1 46. Kg2 Rf4 $17 ) 45. Rd7 Nf4 ( 45... Rd2+ 46. Kh3 Nf4+ 47. Kg4 Nxd5 48. Kxg5 $11 ) 46. Rxd6 Nxe6 47. Rxe6 Rxd5 48. Re4 Rd2+ ( 48... Kg7 49. b4 Rd4 50. Rxd4 cxd4 51. b5 $11 ) 49. Kg3 Rxb2 50. Re5 1/2-1/2"
	), (
		True,	False,	showChar '[' . showString ContextualNotation.PGN.dateTag $ ContextualNotation.PGN.showsDate (toEnum 0) "]\n1. d4 Nf6 2. c4 e6 3. Nf3 b6 4. g3 Ba6 5. Qa4 Bb7 6. Bg2 c5 7. dxc5 Bxc5 8. O-O O-O 9. Nc3 Be7 10. Bf4 Na6 11. Rfd1 Nc5 12. Qc2 Qc8 13. Rac1 Nce4 14. Nd4 Nxc3 15. Qxc3 a6 16. Qb3 Bxg2 17. Kxg2 Qb7+ 18. Qf3 Ra7 19. Qxb7 Rxb7 20. f3 Rc8 21. e4 d6 22. Ne2 Rc6 23. b3 h6 24. h4 Nd7 25. Rd2 Ne5 26. Be3 h5 27. f4 Ng4 28. Bg1 g6 29. Kf3 Bf8 30. Rdc2 Rc8 31. b4 Nf6 32. Nd4 e5 33. Nb3 Nd7 34. a3 Rbc7 35. Be3 Be7 36. Bf2 f5 37. exf5 gxf5 38. Ke2 Kf7 39. Nd2 b5 40. fxe5 Nxe5 41. c5 dxc5 42. Bxc5 Bxc5 43. Rxc5 Rxc5 44. Rxc5 Rxc5 45. bxc5 Ke6 46. Ke3 a5 47. Nb3 Nc6 48. Kf4 Kf6 49. Nc1 b4 50. axb4 Nxb4 51. Ke3 a4 52. Kd4 a3 53. Kc3 a2 54. Nb3 Na6 55. Kb2 Ke5 56. Kxa2 f4 57. gxf4+ Kxf4 58. Ka3 Nxc5 59. Nxc5 Kg4 60. Ne4 Kxh4 61. Nf6 Kg5 62. Nxh5 {insufficient material} Kxh5 1/2-1/2"
	)
 ]

