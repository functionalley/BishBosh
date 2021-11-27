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

module BishBosh.Test.HUnit.Model.Game(
-- * Constants
	testCases,
-- * Functions
	applyMoves
) where

import			BishBosh.Model.Game((/~))
import			Control.Arrow((&&&), (***), (|||))
import			Data.Map((!))
import qualified	BishBosh.Attribute.MoveType			as Attribute.MoveType
import qualified	BishBosh.Attribute.Rank				as Attribute.Rank
import qualified	BishBosh.Cartesian.Coordinates			as Cartesian.Coordinates
import qualified	BishBosh.Colour.LogicalColour			as Colour.LogicalColour
import qualified	BishBosh.Component.Move				as Component.Move
import qualified	BishBosh.Component.QualifiedMove		as Component.QualifiedMove
import qualified	BishBosh.Data.Exception				as Data.Exception
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.Notation.MoveNotation			as Notation.MoveNotation
import qualified	BishBosh.Notation.Smith				as Notation.Smith
import qualified	BishBosh.Property.ExtendedPositionDescription	as Property.ExtendedPositionDescription
import qualified	BishBosh.Property.ForsythEdwards		as Property.ForsythEdwards
import qualified	BishBosh.Rule.GameTerminationReason		as Rule.GameTerminationReason
import qualified	BishBosh.Text.ShowList				as Text.ShowList
import qualified	Control.Exception
import qualified	Data.Default
import qualified	Data.Maybe
import qualified	Test.HUnit
import			Test.HUnit((~?), (~:), (~?=))

-- | Apply moves specified in Smith-notation, to the default opening board.
applyMoves :: [String] -> Either (String, String) Model.Game.Game
applyMoves	= Model.Game.applyEitherQualifiedMoves (
	\s -> case Notation.MoveNotation.readsQualifiedMove Data.Default.def {-Smith-} s of
		[(eitherQualifiedMove, "")]	-> Right eitherQualifiedMove
		_				-> Left . shows s . showString " /~ " $ Notation.MoveNotation.showsMoveSyntax Data.Default.def ""
 ) Data.Default.def

-- | Check the sanity of the implementation, by validating a list of static test-cases.
testCases :: Test.HUnit.Test
testCases	= Test.HUnit.test [
	"'BishBosh.Model.Game.showFEN' failed" ~: Property.ForsythEdwards.showFEN (Data.Default.def :: Model.Game.Game) ~?= "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
	"'BishBosh.Model.Game.showFEN' failed" ~: either (
		\(moveString, s)	-> Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Test.HUnit.Model.Game.testCases:\t" . showString moveString . showString "; " $ showString s "."
	) Property.ForsythEdwards.showFEN (
		applyMoves ["e2e4"]
	) ~?= "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
	"'BishBosh.Model.Game.showFEN' failed" ~: either (
		\(moveString, s)	-> Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Test.HUnit.Model.Game.testCases:\t" . showString moveString . showString "; " $ showString s "."
	) Property.ForsythEdwards.showFEN (
		applyMoves ["e2e4", "c7c5"]
	) ~?= "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2",
	"'BishBosh.Model.Game.showFEN' failed" ~: either (
		\(moveString, s)	-> Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Test.HUnit.Model.Game.testCases:\t" . showString moveString . showString "; " $ showString s "."
	) Property.ForsythEdwards.showFEN (
		applyMoves $ words "e2e4 c7c5 g1f3"
	) ~?= "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2",
	all (
		(
			$ (Model.Game.fromBoard $ Property.ForsythEdwards.readFEN "r3k2r/8/8/8/8/8/8/R3K2R" :: Model.Game.Game)
		) . (
			\s -> case Notation.MoveNotation.readsQualifiedMove Data.Default.def s of
				[(eitherQualifiedMove, "")]	-> (`Model.Game.isValidEitherQualifiedMove` eitherQualifiedMove)
				_				-> Control.Exception.throw . Data.Exception.mkParseFailure . showString "BishBosh.Test.HUnit.Model.Game.testCases:\t" . shows s . showString " /~ " $ Notation.MoveNotation.showsMoveSyntax Data.Default.def ""
		)
	) [
		"e1g1c",
		"e1c1C"
	] ~? "'BishBosh.Model.Game.isValidEitherQualifiedMove' failed when castling.",
	"'BishBosh.Model.Game.findQualifiedMovesAvailableToNextPlayer' failed" ~: Model.Game.findQualifiedMovesAvailableToNextPlayer (
		Model.Game.fromBoard $ Property.ForsythEdwards.readFEN "8/3K4/4q3/3bb3/8/8/8/4k3"
	) ~?= [
		Component.QualifiedMove.mkQualifiedMove (
			Cartesian.Coordinates.mkRelativeCoordinates ((+ 3) *** (+ 6)) `Component.Move.mkMove` Cartesian.Coordinates.mkRelativeCoordinates ((+ 3) *** (+ 7))
		) Data.Default.def {-moveType-}
	],
	not (
		Model.Game.isValidQualifiedMove (
			Model.Game.fromBoard $ Property.ForsythEdwards.readFEN "4k3/8/8/8/1b6/3p4/3R4/4K3"
		) . Component.QualifiedMove.mkQualifiedMove (
			Cartesian.Coordinates.mkRelativeCoordinates ((+ 3) *** succ) `Component.Move.mkMove` Cartesian.Coordinates.mkRelativeCoordinates ((+ 3) *** (+ 2))
		) $ Attribute.MoveType.mkNormalMoveType (Just Attribute.Rank.Pawn) Nothing
	) ~? "'BishBosh.Model.Game.isValidQualifiedMove' failed",
	let
		pawnsCoordinates :: Cartesian.Coordinates.Coordinates
		pawnsCoordinates	= Cartesian.Coordinates.translateY pred maxBound

		game :: Model.Game.Game
		game	= Model.Game.fromBoard $ Property.ForsythEdwards.readFEN "4k3/7P/8/8/8/8/8/4K3"
	in Model.Game.getBoard (
		fst {-game-} . head . Model.Game.rollBack $ Model.Game.applyQualifiedMove (
			Component.QualifiedMove.mkQualifiedMove (
				Component.Move.mkMove pawnsCoordinates maxBound
			) . Attribute.MoveType.mkNormalMoveType Nothing $ Just Attribute.Rank.defaultPromotionRank
		) game
	) == Model.Game.getBoard game ~? "'BishBosh.Model.Game.rollback' failed to undo a Pawn-promotion",
	let
		game :: Model.Game.Game
		game	= Model.Game.fromBoard $ Property.ForsythEdwards.readFEN "4k3/8/8/8/8/8/8/R3K2R"
	in all (
		(
			== Model.Game.getBoard game
		) . (
			\s -> case Notation.MoveNotation.readsQualifiedMove Data.Default.def s of
				[(eitherQualifiedMove, "")]	-> Model.Game.getBoard . fst {-game-} . head . Model.Game.rollBack $ Model.Game.applyEitherQualifiedMove eitherQualifiedMove game
				_				-> Control.Exception.throw . Data.Exception.mkParseFailure . showString "BishBosh.Test.HUnit.Model.Game.testCases:\t" . shows s . showString " /~ " $ Notation.MoveNotation.showsMoveSyntax Data.Default.def ""
		)
	) [
		"e1g1c",
		"e1c1C"
	] ~? "'BishBosh.Model.Game.rollback' failed to undo castling",
	(
		\(moveString, errorMessage)	-> Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Test.HUnit.Model.Game.testCases:\tfailed for " . showString Component.Move.tag . Text.ShowList.showsAssociation . shows moveString . showString "; " $ showString errorMessage "."
	) ||| (
		\game -> case Notation.MoveNotation.readsQualifiedMove Data.Default.def "a5b6E" of
			[(eitherQualifiedMove, "")]	-> Model.Game.isValidEitherQualifiedMove game eitherQualifiedMove ~? "'BishBosh.Model.Game.isValidEitherQualifiedMove' failed for En-passant by White."
			_				-> Control.Exception.throw . Data.Exception.mkParseFailure . showString "BishBosh.Test.HUnit.Model.Game.testCases:\t /~ " $ Notation.MoveNotation.showsMoveSyntax Data.Default.def ""
	) $ applyMoves [
		"a2a4",	-- White: Queen's Rook's Pawn.
		"b8c6",	-- Black: Queen's Knight.
		"a4a5",	-- White: Queen's Rook's Pawn.
		"b7b5"	-- Black: Queen's Knight's Pawn.
	],
	(
		\(moveString, errorMessage)	-> Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Test.HUnit.Model.Game.testCases:\tfailed for " . showString Component.Move.tag . Text.ShowList.showsAssociation . shows moveString . showString "; " $ showString errorMessage "."
	) ||| (
		\game -> case Notation.MoveNotation.readsQualifiedMove Data.Default.def "a4b3E" of
			[(eitherQualifiedMove, "")]	-> Model.Game.isValidEitherQualifiedMove game eitherQualifiedMove ~? "'BishBosh.Model.Game.isValidEitherQualifiedMove' failed for En-passant by Black."
			_				-> Control.Exception.throw $ Data.Exception.mkParseFailure "BishBosh.Test.HUnit.Model.Game.testCases:\tfailed to parse move."
	) $ applyMoves [
		"b1c3",	-- White: Queen's Knight.
		"a7a5",	-- Black: Queen's Rook's Pawn.
		"c3b1",	-- White: Queen's Knight.
		"a5a4",	-- Black: Queen's Rook's Pawn.
		"b2b4"	-- White: Queen's Knight's Pawn.
	],
	Data.Maybe.maybe False Rule.GameTerminationReason.isStaleMate (
		Model.Game.getMaybeTerminationReason (
			Model.Game.fromBoard $ Property.ForsythEdwards.readFEN "rnbqk2r/pppppp2/8/8/8/7p/6np/6bK"
		)
	) ~? "'BishBosh.Model.Game.getMaybeTerminationReason' failed to detect \"Stale-mate\".",
	all (
		\s -> Data.Maybe.maybe False Rule.GameTerminationReason.isDrawByInsufficientMaterial $ Model.Game.getMaybeTerminationReason (
			Model.Game.fromBoard $ Property.ForsythEdwards.readFEN s
		)
	) [
		"2k5/8/1KB5/3B4/8/8/8/8",
		"6k1/8/5KN1/8/8/8/8/8",
		"8/8/2KB4/8/7k/2B5/5b2/8"
	] ~? "'BishBosh.Model.Game.getMaybeTerminationReason' failed to detect \"Draw by Insufficient Material\".",
	all (
		\s -> Data.Maybe.maybe True (not . Rule.GameTerminationReason.isDrawByInsufficientMaterial) $ Model.Game.getMaybeTerminationReason (
			Model.Game.fromBoard $ Property.ForsythEdwards.readFEN s
		)
	) [
		"k7/8/K7/8/8/8/8/1Q6",
		"k7/8/K7/8/8/8/8/1R6",
		"k7/8/K7/8/8/8/P7/8",
		"k7/8/K7/8/8/8/8/B6n",
		"k7/8/K7/8/8/8/8/n6n",
		"k7/8/K7/8/8/8/8/N6n",
		"k7/8/K7/8/8/8/8/bB6",
		"k7/8/K7/8/8/8/8/BB6"
	] ~? "'BishBosh.Model.Game.getMaybeTerminationReason' false positive \"Draw by Insufficient Material\".",
	(
		either (
			\(moveString, s)	-> Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Test.HUnit.Model.Game.testCases:\t" . showString moveString . showString "; " $ showString s "."
		) (
			uncurry (==) . (
				Model.Game.sortAvailableQualifiedMoves . (! Colour.LogicalColour.White) . Model.Game.getAvailableQualifiedMovesByLogicalColour &&& Model.Game.sortAvailableQualifiedMoves . (`Model.Game.mkAvailableQualifiedMovesFor` Colour.LogicalColour.White)
			)
		) . applyMoves $ words "g2g4 f7f6 g1h3 g7g5 h1g1 c7c6 d2d3 h7h5 c1d2 h5g4p d3d4 b8a6 f2f4 g4f3E"
	 ) ~? "'BishBosh.Model.Game.getAvailableQualifiedMovesByLogicalColour' failed after En-passant.",
	either (
		\(moveString, s)	-> Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Test.HUnit.Model.Game.testCases:\t" . showString moveString . showString "; " $ showString s "."
	) (
		Data.Maybe.maybe False Rule.GameTerminationReason.isDraw . Model.Game.getMaybeTerminationReason
	) (
		applyMoves . concat . replicate 4 $ words "g1f3 g8f6 f3g1 f6g8"
	) ~? "'BishBosh.Model.Game./~' failed to account for Draw by Five-fold Repetition.",
	let
		initialMoves	= [
			"e2e4",
			"g8f6",
			"e4e5",
			"d7d5"	-- Construct an en-passant opportunity.
		 ]
	in either (
		\(moveString, s)	-> Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Test.HUnit.Model.Game.testCases:\t" . showString moveString . showString "; " $ showString s "."
	) id (
		applyMoves initialMoves
	) /~ either (
		\(moveString, s)	-> Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Test.HUnit.Model.Game.testCases:\t" . showString moveString . showString "; " $ showString s "."
	) id (
		applyMoves $ initialMoves ++ [
			"d1e2",
			"d8d7",
			"e2d1",
			"d7d8"	-- Dither to waste the en-passant opportunity, while retaining an identical board.
		]
	) ~? "'BishBosh.Model.Game./~' failed to account for En-passant.",
	either (
		\(moveString, s)	-> Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Test.HUnit.Model.Game.testCases:\t" . showString moveString . showString "; " $ showString s "."
	) id (
		applyMoves [
			"e2e4",
			"g8f6",
			"e4e5",
			"d7d5"	-- Construct an en-passant opportunity.
		]
	) /~ either (
		\(moveString, s)	-> Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Test.HUnit.Model.Game.testCases:\t" . showString moveString . showString "; " $ showString s "."
	) id (
		applyMoves [
			"e2e3",
			"g8f6",
			"e3e4",
			"d7d6",
			"e4e5",
			"d6d5"	-- Create an identical board without the en-passant opportunity.
		]
	) ~? "'BishBosh.Model.Game./~' failed to account for En-passant.",
	either (
		\(moveString, s)	-> Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Test.HUnit.Model.Game.testCases:\t" . showString moveString . showString "; " $ showString s "."
	) (
		/~ Data.Default.def
	) (
		applyMoves $ words "g1f3 g8f6 h1g1 f6g4 g1h1 g4h6 f3g1 h6g8"
	) ~? "'BishBosh.Model.Game./~' failed to account for lost Castling-potential.",
	let
		longCastle	= "e1c1C"
	in case Notation.MoveNotation.readsQualifiedMove Data.Default.def longCastle of
		[(eitherQualifiedMove, "")]	-> Model.Game.isValidEitherQualifiedMove (
			Property.ForsythEdwards.readFEN "r2qkbnr/ppp1pppp/2np4/5b2/3P4/NQP5/PP1BPPPP/R3KBNR w KQkq - 5 6"
		 ) eitherQualifiedMove ~? "long Castle through legal check failed."
		_				-> Control.Exception.throw . Data.Exception.mkParseFailure . showString "BishBosh.Test.HUnit.Model.Game.testCases:\tfailed to parse " $ shows longCastle ".",
	let
		epds :: [String]
		epds	= [
			"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -",			-- Initial position.
			"rnbqkbnr/pppp1ppp/8/3Pp3/8/8/PPP1PPPP/RNBQKBNR w KQkq e6",		-- Double Pawn-advance leading to en-passant option.
			"rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3",		-- Double Pawn-advance, but no actual en-passant option.
			"rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6",		-- Double Pawn-advance, but no actual en-passant option.
			" rnbqkbnr/pppppppp/8/8/1P6/8/P1PPPPPP/RNBQKBNR  w\t Kq \r  b3",	-- Extra white space & potential parser confusion between Casteable Rooks & En-passant destination fields.
			"rnbqkr2/p2pbp1p/1pp4n/6N1/PP2pRpP/2N5/R1PPPPP1/2BQKB2 b - -"		-- Neither Casteable Rooks nor En-passant destination are defined.
		 ]

		parseFailures :: [(String, String)]
		parseFailures	= filter (
			uncurry (/=)
		 ) $ map (
			\epd -> (
				epd,
				case Property.ExtendedPositionDescription.readsEPD epd of
					[(game, "")]		-> Property.ExtendedPositionDescription.showEPD (game :: Model.Game.Game)
					[(_, remainder)]	-> Control.Exception.throw . Data.Exception.mkRedundantData . showString "BishBosh.Test.HUnit.Model.Game.testCases:\tparsed EPD=" . shows epd . showString ", but leaving " $ shows remainder " unparsed."
					_			-> Control.Exception.throw . Data.Exception.mkParseFailure . showString "BishBosh.Test.HUnit.Model.Game.testCases:\tfailed to parse EPD=" $ shows epd "."
			) -- Pair.
		 ) epds
	in not (null parseFailures) ~? showString "BishBosh.Test.HUnit.Model.Game.testCases:\tfailed to correctly parse EPDs=" (shows parseFailures "."),
	let
		terminalMoves	= map (Notation.Smith.getQualifiedMove . read) ["d6d8", "e7c7", "d6b6"]
	in all (
		`elem` Model.Game.findQualifiedMovesAvailableTo (Model.Game.fromBoard $ Property.ForsythEdwards.readFEN "K7/4q1p1/3r4/8/8/5k2/8/8 b - - 13 49" :: Model.Game.Game) minBound
	) terminalMoves ~? "BishBosh.Test.HUnit.Model.Game.testCases:\tterminal move unavailable."
 ]

