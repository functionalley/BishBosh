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

module BishBosh.Test.HUnit.ContextualNotation.StandardAlgebraic(
-- * Constants
	testCases
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.LogicalColour		as Attribute.LogicalColour
import qualified	BishBosh.Component.EitherQualifiedMove		as Component.EitherQualifiedMove
import qualified	BishBosh.Component.Move				as Component.Move
import qualified	BishBosh.Component.QualifiedMove		as Component.QualifiedMove
import qualified	BishBosh.ContextualNotation.StandardAlgebraic	as ContextualNotation.StandardAlgebraic
import qualified	BishBosh.Data.Exception				as Data.Exception
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.Notation.MoveNotation			as Notation.MoveNotation
import qualified	BishBosh.Property.Empty				as Property.Empty
import qualified	BishBosh.Text.ShowList				as Text.ShowList
import qualified	Control.Exception
import qualified	Data.Default
import qualified	Data.Maybe
import qualified	Test.HUnit

#ifdef USE_POLYPARSE
import			Test.HUnit((~?=))
#	if USE_POLYPARSE == 1
import qualified	Text.ParserCombinators.Poly.Lazy		as Poly
#	else /* Plain */
import qualified	Text.ParserCombinators.Poly.Plain		as Poly
#	endif
#else /* Parsec */
import			Control.Arrow((|||))
import			Test.HUnit((~?=), (~?))
import qualified	Text.ParserCombinators.Parsec
#endif

-- | Check the sanity of the implementation, by validating a list of static test-cases.
testCases :: Test.HUnit.Test
testCases	= Test.HUnit.test $ map (
	\(game, san, standardAlgebraic) -> let
		parser	= ContextualNotation.StandardAlgebraic.parser explicitEnPassant True {-validateMoves-} game
	in
#ifdef USE_POLYPARSE
		fst (Poly.runParser parser san) ~?=
#	if USE_POLYPARSE == 1
			standardAlgebraic
#	else /* Plain */
			Right standardAlgebraic
#	endif
#else /* Parsec */
		(\parseError -> False ~? show parseError) ||| (~?= standardAlgebraic) $ Text.ParserCombinators.Parsec.parse parser "SAN-parser" san
#endif
 ) $ map (
	\s -> let
		game :: Model.Game.Game
		game	= Data.Default.def
	 in case Notation.MoveNotation.readsQualifiedMove Data.Default.def s of
		[(eitherQualifiedMove, "")]	-> let
			(move, Right moveType)	= Component.EitherQualifiedMove.getMove &&& Component.EitherQualifiedMove.getPromotionRankOrMoveType $ eitherQualifiedMove
		 in Data.Maybe.maybe (
			game,
			showsSAN eitherQualifiedMove game "",
			ContextualNotation.StandardAlgebraic.fromQualifiedMove $ Component.QualifiedMove.mkQualifiedMove move moveType
		 ) (
			Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Test.HUnit.ContextualNotation.StandardAlgebraic.testCases:\tinvalid " . showString Component.Move.tag . Text.ShowList.showsAssociation . shows move . showString "; " . show
		 ) $ Model.Game.validateEitherQualifiedMove eitherQualifiedMove game
		_		-> Control.Exception.throw . Data.Exception.mkParseFailure . showString "BishBosh.Test.HUnit.ContextualNotation.StandardAlgebraic.testCases:\tfailed to read " $ shows s "."
 ) ["g1h3", "e2e3", "e2e4"] ++ map (
	\(fen, nextLogicalColour, s, maybeMoveSuffixAnnotation) -> case Notation.MoveNotation.readsQualifiedMove Data.Default.def s of
		[(eitherQualifiedMove, "")]	-> let
			(move, Right moveType)	= Component.EitherQualifiedMove.getMove &&& Component.EitherQualifiedMove.getPromotionRankOrMoveType $ eitherQualifiedMove
		 in case reads fen of
			[(board, "")]	-> let
				game	= Model.Game.mkGame nextLogicalColour Data.Default.def {-CastleableRooksByLogicalColour-} board Property.Empty.empty {-TurnsByLogicalColour-}
			 in Data.Maybe.maybe (
				game,
				showsSAN eitherQualifiedMove game $ Data.Maybe.fromMaybe "" maybeMoveSuffixAnnotation,
				ContextualNotation.StandardAlgebraic.fromQualifiedMove $ Component.QualifiedMove.mkQualifiedMove move moveType
			 ) (
				Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Test.HUnit.ContextualNotation.StandardAlgebraic.testCases:\tinvalid " . showString Component.Move.tag . Text.ShowList.showsAssociation . shows move . showString "; " . show
			 ) $ Model.Game.validateEitherQualifiedMove eitherQualifiedMove game
			_		-> Control.Exception.throw . Data.Exception.mkParseFailure . showString "BishBosh.Test.HUnit.ContextualNotation.StandardAlgebraic.testCases:\tfailed to read FEN" . Text.ShowList.showsAssociation $ show fen
		_		-> Control.Exception.throw . Data.Exception.mkParseFailure . showString "BishBosh.Test.HUnit.ContextualNotation.StandardAlgebraic.testCases:\tfailed to read " $ shows s "."
 ) [
	(
		"r3k2r/8/8/8/8/8/PPPPPPPP/RNBQK2R",
		Attribute.LogicalColour.White,
		"e1g1c",	-- O-O.
		Just "!"
	), (
		"rnbqk2r/pppppppp/8/8/8/8/8/R3K2R",
		Attribute.LogicalColour.Black,
		"e8g8c",	-- O-O.
		Just " ?"
	), (
		"r3k2r/8/8/8/8/8/PPPPPPPP/R3KBNR",
		Attribute.LogicalColour.White,
		"e1c1C",	-- O-O-O.
		Just "!!"
	), (
		"r3kbnr/pppppppp/8/8/8/8/8/R3K2R",
		Attribute.LogicalColour.Black,
		"e8c8C",	-- O-O-O.
		Just " !?"
	), (
		"r3kbnr/8/2R5/1RnR4/2R5/8/8/R3K2R",
		Attribute.LogicalColour.White,
		"b5c5n",	-- Rbxc5.
		Just "?!"
	), (
		"r3kbnr/8/2R5/1RnR4/2R5/8/8/R3K2R",
		Attribute.LogicalColour.White,
		"c4c5n",	-- R4xc5.
		Just "\t??"
	), (
		"r3kbnr/3N1N2/2N3N1/4b3/2N3N1/3N1N2/8/R3K2R",
		Attribute.LogicalColour.White,
		"d3e5b",	-- Nd3xe5.
		Nothing
	), (
		"r3kbnr/8/8/8/3r4/2P1P3/8/R3K2R",
		Attribute.LogicalColour.White,
		"c3d4r",	-- cxd4.
		Nothing
	), (
		"r3kbnr/1P6/8/8/8/8/8/R3K2R",
		Attribute.LogicalColour.White,
		"b7b8N",	-- b8=N.
		Nothing
	), (
		"r3kbnr/1P6/8/8/8/8/8/R3K2R",
		Attribute.LogicalColour.White,
		"b7a8rQ",	-- bxa8=Q+.
		Nothing
	)
 ] where
	explicitEnPassant :: ContextualNotation.StandardAlgebraic.ExplicitEnPassant
	explicitEnPassant	= False

	showsSAN eitherQualifiedMove game	= ContextualNotation.StandardAlgebraic.showsTurn explicitEnPassant (
		Data.Maybe.fromJust . Model.Game.maybeLastTurn $ Model.Game.applyEitherQualifiedMove eitherQualifiedMove game	-- Turn.
	 ) game

