{-# LANGUAGE CPP#-}
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

 [@DESCRIPTION@]	Static tests based on <https://www.chessprogramming.org/Bratko-Kopec_Test>.
-}

module BishBosh.Test.HUnit.Search.Search (
-- * Constants
--	evaluationOptions,
--	searchOptions,
	testCases
) where

import qualified	BishBosh.Attribute.CaptureMoveSortAlgorithm		as Attribute.CaptureMoveSortAlgorithm
import qualified	BishBosh.Data.Exception					as Data.Exception
import qualified	BishBosh.Evaluation.PositionHashQuantifiedGameTree	as Evaluation.PositionHashQuantifiedGameTree
import qualified	BishBosh.Evaluation.QuantifiedGame			as Evaluation.QuantifiedGame
import qualified	BishBosh.Input.CriteriaWeights				as Input.CriteriaWeights
import qualified	BishBosh.Input.EvaluationOptions			as Input.EvaluationOptions
import qualified	BishBosh.Input.SearchOptions				as Input.SearchOptions
import qualified	BishBosh.Notation.MoveNotation				as Notation.MoveNotation
import qualified	BishBosh.Property.Empty					as Property.Empty
import qualified	BishBosh.Property.ExtendedPositionDescription		as Property.ExtendedPositionDescription
import qualified	BishBosh.Search.Search					as Search.Search
import qualified	BishBosh.Search.SearchState				as Search.SearchState
import qualified	BishBosh.Type.Crypto					as Type.Crypto
import qualified	BishBosh.Type.Mass					as Type.Mass
import qualified	Control.Exception
import qualified	Control.Monad.Reader
import qualified	Data.Default
import qualified	Data.List
import qualified	Test.HUnit
import			Test.HUnit((~?))

-- | Constant.
evaluationOptions :: Fractional pieceSquareValue => Input.EvaluationOptions.EvaluationOptions pieceSquareValue
evaluationOptions	= Input.EvaluationOptions.mkEvaluationOptions Data.Default.def {-rankValues-} Data.Default.def {
	Input.CriteriaWeights.getWeightOfMobility		= fromRational $ 24 / 1000,
	Input.CriteriaWeights.getWeightOfCastlingPotential	= fromRational $ 5 / 1000
} False {-IncrementalEvaluation-} Nothing {-Maybe PieceSquareTable-}

-- | Constant.
searchOptions :: Input.SearchOptions.SearchOptions
searchOptions	= Data.Default.def {
	Input.SearchOptions.getMaybeCaptureMoveSortAlgorithm	= Just Attribute.CaptureMoveSortAlgorithm.MVVLVA,
	Input.SearchOptions.getMaybeRetireKillerMovesAfter	= Just 3,
	Input.SearchOptions.getMaybeUseTranspositions		= Just (1, 2)
}

-- | Check the sanity of the implementation, by validating a list of static test-cases.
testCases :: Test.HUnit.Test
testCases	= Test.HUnit.test $ map (
	\(searchDepth, epd, match, moveOptions) -> case Property.ExtendedPositionDescription.readsEPD epd of
			[(game, "")]	-> let
				turnString	= Notation.MoveNotation.showNotation Data.Default.def {-Smith-} $ case Search.Search.getQuantifiedGames $ Control.Monad.Reader.runReader (
					Search.Search.search searchDepth $ Search.SearchState.initialise (
						Evaluation.PositionHashQuantifiedGameTree.mkPositionHashQuantifiedGameTree (
							evaluationOptions	:: Input.EvaluationOptions.EvaluationOptions Type.Mass.PieceSquareValue
						) searchOptions Data.Default.def {-Zobrist-} Property.Empty.empty {-MoveFrequency-} game :: Evaluation.PositionHashQuantifiedGameTree.PositionHashQuantifiedGameTree Type.Crypto.PositionHash
					 )
				 ) searchOptions of
					quantifiedGame : _	-> Evaluation.QuantifiedGame.getLastTurn quantifiedGame
					_			-> Control.Exception.throw $ Data.Exception.mkNullDatum "BishBosh.Test.HUnit.Search.Search.testCases:\tfailed."

				moveStrings	= words moveOptions
			 in (
				if match
					then elem
					else notElem
			 ) turnString moveStrings ~? (
				shows turnString . showChar ' ' . showString (
					if match
						then "!~"
						else "=~"
				) . showString " /" $ showString (Data.List.intercalate "|" moveStrings) "/."
			 )
			_		-> Control.Exception.throw $ Data.Exception.mkParseFailure . showString "BishBosh.Test.HUnit.Search.Search.testCases:\tfailed to parse " $ shows epd "."

 ) [
--		EPD									Match		Move-options		Observed move, at various search-depths.
--																4		5
#ifdef USE_BRATKO_KOPEC
	(4,	"1k1r4/pp1b1R2/3q2pp/4p3/2B5/4Q3/PPP2B2/2K5 w - -",			True,		"e3a7p"),
	(4,	"3r1k2/4npp1/1ppr3p/p6P/P2PPPP1/1NR5/5K2/2R5 w - -",			True,		"d4d5"),		-- f2g2		e4e5.
	(4,	"2q1rr1k/3bbnnp/p2p1pp1/2pPp3/PpP1P1P1/1P2BNNP/2BQ1PRK/7R b - -",	True,		"f6f5"),		-- c8b7		d7g4p.
	(4,	"rnbqkb1r/p3pppp/1p6/2ppP3/3N4/2P5/PPP1QPPP/R1B1KB1R w KQkq -",		True,		"e5e6"),		-- d4b3		c1g5.
	(4,	"r1b2rk1/2q1b1pp/p2ppn2/1p6/3QP3/1BN1B3/PPP3PP/R4RK1 w - -",		True,		"c3d5 a3a4"),		-- e3g5		d4b6.
	(4,	"2r3k1/pppR1pp1/4p3/4P1P1/5P2/1P4K1/P1P5/8 w - -",			True,		"g5g6"),		-- g3f2		g3f2.
	(4,	"1nk1r1r1/pp2n1pp/4p3/q2pPp1N/b1pP1P2/B1P2R2/2P1B1PP/R2Q2K1 w - -",	True,		"h5f6"),		-- a3d6		a3e7n.
	(4,	"4b3/p3kp2/6p1/3pP2p/2pP1P2/4K1P1/P3N2P/8 w - -",			True,		"f4f5"),		-- e2c3		e2c3.
	(4,	"2kr1bnr/pbpq4/2n1pp2/3p3p/3P1P1B/2N2N1Q/PPP3PP/2KR1B1R w - -",		True,		"f4f5"),		-- f1b5.
	(4,	"3rr1k1/pp3pp1/1qn2np1/8/3p4/PP1R1P2/2P1NQPP/R1B3K1 b - -",		True,		"c6e5"),
	(4,	"2r1nrk1/p2q1ppp/bp1p4/n1pPp3/P1P1P3/2PBB1N1/4QPPP/R4RK1 w - -",	True,		"f3f4"),		-- f1e1.
	(4,	"r3r1k1/ppqb1ppp/8/4p1NQ/8/2P5/PP3PPP/R3R1K1 b - -",			True,		"d7f5"),
	(4,	"r2q1rk1/4bppp/p2p4/2pP4/3pP3/3Q4/PP1B1PPP/R3R1K1 w - -",		True,		"b2b4"),
	(4,	"rnb2r1k/pp2p2p/2pp2p1/q2P1p2/8/1Pb2NP1/PB2PPBP/R2Q1RK1 w - -",		True,		"d1d2 d1e1"),
	(4,	"2r3k1/1p2q1pp/2b1pr2/p1pp4/6Q1/1P1PP1R1/P1PN2PP/5RK1 w - -",		True,		"g4g7p"),		-- g4h3.
	(4,	"r1bqkb1r/4npp1/p1p4p/1p1pP1B1/8/1B6/PPPN1PPP/R2Q1RK1 w kq -",		True,		"d2e4"),		-- g5h4.
	(4,	"r2q1rk1/1ppnbppp/p2p1nb1/3Pp3/2P1P1P1/2N2N1P/PPB1QP2/R1B2RK1 b - -",	True,		"h7h5"),		-- f6e8.
	(4,	"r1bq1rk1/pp2ppbp/2np2p1/2n5/P3PP2/N1P2N2/1PB3PP/R1B1QRK1 b - -",	True,		"c5b3"),		-- c8g4.
	(4,	"3rr3/2pq2pk/p2p1pnp/8/2QBPP2/1P6/P5PP/4RRK1 b - -",			True,		"e8e4p"),		-- d6d5.
	(4,	"r4k2/pb2bp1r/1p1qp2p/3pNp2/3P1P2/2N3P1/PPP1Q2P/2KRR3 w - -",		True,		"g3g4"),		-- c3b5.
	(4,	"3rn2k/ppb2rpp/2ppqp2/5N2/2P1P3/1P5Q/PB3PPP/3RR1K1 w - -",		True,		"f5h6"),		-- f5h4.
	(4,	"2r2rk1/1bqnbpp1/1p1ppn1p/pP6/N1P1P3/P2B1N1P/1B2QPP1/R2R2K1 b - -",	True,		"b7e4p"),		-- d7e5.
	(4,	"r1bqk2r/pp2bppp/2p5/3pP3/P2Q1P2/2N1B3/1PP3PP/R4RK1 b kq -",		True,		"f7f6"),		-- c8f5.
	(4,	"r2qnrnk/p2b2b1/1p1p2pp/2pPpp2/1PP1P3/PRNBB3/3QNPPP/5RK1 w - -",	True,		"f2f4"),		-- e4f5p.
#endif
	(4,	"8/k6P/7R/8/8/8/8/1R5K w - -",						False,		"h7h8Q h7h8R")	-- Experimental test of Stalemate-avoidance.
 ]

