{-# OPTIONS_GHC -fno-warn-orphans #-}
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

module BishBosh.Test.QuickCheck.Model.Game(
-- * Types
-- ** Type-synonyms
	Game,
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.State.Board()
import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.MoveType			as Attribute.MoveType
import qualified	BishBosh.Attribute.Rank				as Attribute.Rank
import qualified	BishBosh.Cartesian.Coordinates			as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate			as Cartesian.Ordinate
import qualified	BishBosh.Component.Move				as Component.Move
import qualified	BishBosh.Component.Piece			as Component.Piece
import qualified	BishBosh.Component.QualifiedMove		as Component.QualifiedMove
import qualified	BishBosh.Component.Turn				as Component.Turn
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.Property.FixedMembership		as Property.FixedMembership
import qualified	BishBosh.Property.ForsythEdwards		as Property.ForsythEdwards
import qualified	BishBosh.Property.Null				as Property.Null
import qualified	BishBosh.Property.Opposable			as Property.Opposable
import qualified	BishBosh.Property.Reflectable			as Property.Reflectable
import qualified	BishBosh.Rule.DrawReason			as Rule.DrawReason
import qualified	BishBosh.State.Board				as State.Board
import qualified	BishBosh.State.CastleableRooksByLogicalColour	as State.CastleableRooksByLogicalColour
import qualified	BishBosh.State.CoordinatesByRankByLogicalColour	as State.CoordinatesByRankByLogicalColour
import qualified	BishBosh.State.MaybePieceByCoordinates		as State.MaybePieceByCoordinates
import qualified	BishBosh.StateProperty.Seeker			as StateProperty.Seeker
import qualified	BishBosh.State.TurnsByLogicalColour		as State.TurnsByLogicalColour
import qualified	BishBosh.Type.Count				as Type.Count
import qualified	BishBosh.Type.Length				as Type.Length
import qualified	Data.Array.IArray
import qualified	Data.Default
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.Map					as Map
import qualified	Data.Maybe
import qualified	Data.Ord
import qualified	System.Random
import qualified	Test.QuickCheck
import qualified	ToolShed.Data.Foldable
import qualified	ToolShed.System.Random
import qualified	ToolShed.Test.ReversibleIO
import			Test.QuickCheck((==>))

-- | Defines a concrete type for testing.
type Game	= Model.Game.Game Type.Length.X Type.Length.Y

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Show	x,
	Show	y
 ) => Test.QuickCheck.Arbitrary (Model.Game.Game x y) where
	{-# SPECIALISE instance Test.QuickCheck.Arbitrary Game #-}
	arbitrary	= let
		play game (randomGen : randomGens)
			| Model.Game.isTerminated game	= game
			| otherwise			= (
				\qualifiedMove -> play (Model.Game.applyQualifiedMove qualifiedMove game) randomGens	-- Recurse.
			) . Data.Maybe.fromJust . ToolShed.System.Random.select randomGen $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
		play game _				= game	-- Terminate recursion.
	 in Test.QuickCheck.arbitrary >>= (
		\randomGens -> (
			play Data.Default.def . (`take` ToolShed.System.Random.randomGens (System.Random.mkStdGen randomGens))
		) `fmap` Test.QuickCheck.choose (1 :: Int, 64)
	 )

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Game -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Game.prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: String -> Test.QuickCheck.Property
		f garbage	= Test.QuickCheck.label "Game.prop_read" $ case (reads garbage :: [(Game, String)]) of
			[_]	-> True
			_	-> True	-- Unless the read-implementation throws an exception.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Game -> String -> Test.QuickCheck.Property
		f game	= Test.QuickCheck.label "Game.prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (const False) game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f game	= Test.QuickCheck.label "Game.prop_fen/nFields" . (== 6) . length . words $ Property.ForsythEdwards.showFEN game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f game	= Test.QuickCheck.label "Game.prop_fen/Half move clock" $ uncurry (&&) . (
			(>= 0) &&& (<= Rule.DrawReason.maximumConsecutiveRepeatablePlies)
		 ) . fromInteger . read . (
			!! 4	-- Half-move Clock.
		 ) . words $ Property.ForsythEdwards.showFEN game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f game	= Test.QuickCheck.label "Game.prop_fen/Full move counter" $ (
			> (0 :: Type.Count.NMoves)
		 ) . fromInteger . read . (
			!! 5	-- Full Move Counter.
		 ) . words $ Property.ForsythEdwards.showFEN game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f game	= Test.QuickCheck.label "Game.prop_fen" . (
			\game'	-> and [
				uncurry (==) $ (($ game) &&& ($ game')) Model.Game.getNextLogicalColour,
				uncurry (==) . (($ game) &&& ($ game')) $ State.CastleableRooksByLogicalColour.unify . Model.Game.getCastleableRooksByLogicalColour,
				uncurry (==) $ (($ game) &&& ($ game')) Model.Game.getBoard,
				uncurry (==) $ (($ game) &&& ($ game')) Model.Game.getMaybeChecked
			]
		 ) . Property.ForsythEdwards.readFEN $ Property.ForsythEdwards.showFEN game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f game	= not (Model.Game.isTerminated game) ==> Test.QuickCheck.label "Game.prop_isValidQualifiedMove" . all (`Model.Game.isValidQualifiedMove` game) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 4096 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f game	= Test.QuickCheck.label "Game.prop_findOrderedQualifiedMovesAvailableToNextPlayer" . (
			== if Model.Game.isTerminated game
				then []
				else sort $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
		 ) $ sort [
			qualifiedMove |
				source			<- State.CoordinatesByRankByLogicalColour.listCoordinates $ State.Board.getCoordinatesByRankByLogicalColour board,
				destination		<- Property.FixedMembership.members,
				source /= destination,
				let move	= Component.Move.mkMove source destination,
				maybePromotionRank	<- if Data.Maybe.maybe False (Component.Piece.isPawnPromotion destination) $ State.MaybePieceByCoordinates.dereference source maybePieceByCoordinates
					then map Just Attribute.Rank.promotionProspects
					else [Nothing],
				let qualifiedMove	= Component.QualifiedMove.mkQualifiedMove move $ State.MaybePieceByCoordinates.inferMoveType move maybePromotionRank maybePieceByCoordinates,
				Model.Game.isValidQualifiedMove qualifiedMove game
		 ] {-list-comprehension-} where
			sort			= Data.List.sortBy $ Data.Ord.comparing Component.QualifiedMove.getMove
			board			= Model.Game.getBoard game
			maybePieceByCoordinates	= State.Board.getMaybePieceByCoordinates board
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 4096 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f game	= Test.QuickCheck.label "Game.prop_inferMoveType" . all (
			\qualifiedMove -> let
				moveType		= Component.QualifiedMove.getMoveType qualifiedMove
				inferredMoveType	= State.MaybePieceByCoordinates.inferMoveType (Component.QualifiedMove.getMove qualifiedMove) Nothing {-promotion-rank-} . State.Board.getMaybePieceByCoordinates $ Model.Game.getBoard game
			in if Attribute.MoveType.isPromotion moveType
				then Attribute.MoveType.isPromotion inferredMoveType && Attribute.MoveType.getMaybeExplicitlyTakenRank moveType == Attribute.MoveType.getMaybeExplicitlyTakenRank inferredMoveType
				else moveType == inferredMoveType
		 ) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f game	= Test.QuickCheck.label "Game.prop_findQualifiedMovesAvailableToNextPlayer/unique" . uncurry (==) . (id &&& Data.List.nub) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 512 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Game.prop_getNextLogicalColour" . uncurry (==) . (Model.Game.getNextLogicalColour &&& State.TurnsByLogicalColour.inferNextLogicalColour . Model.Game.getTurnsByLogicalColour)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Game.prop_mkCoordinatesByRankByLogicalColour" . uncurry (==) . (
			State.CoordinatesByRankByLogicalColour.deconstruct . State.CoordinatesByRankByLogicalColour.sortCoordinates . State.Board.getCoordinatesByRankByLogicalColour &&& State.CoordinatesByRankByLogicalColour.deconstruct . State.CoordinatesByRankByLogicalColour.sortCoordinates . State.CoordinatesByRankByLogicalColour.fromMaybePieceByCoordinates . State.Board.getMaybePieceByCoordinates
		 ) . Model.Game.getBoard
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Game.prop_getCoordinatesByRankByLogicalColour/unique" . all (
			(== 1) . length
		 ) . ToolShed.Data.Foldable.gather . State.CoordinatesByRankByLogicalColour.listCoordinates . State.Board.getCoordinatesByRankByLogicalColour . Model.Game.getBoard
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f game	= Test.QuickCheck.label "Game.prop_(getAvailableQualifiedMovesByLogicalColour == mkAvailableQualifiedMovesFor)" . Data.Maybe.maybe True (
			== Model.Game.mkAvailableQualifiedMovesFor nextLogicalColour game
		 ) . Map.lookup nextLogicalColour $ Model.Game.getAvailableQualifiedMovesByLogicalColour game where
			nextLogicalColour	= Model.Game.getNextLogicalColour game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 4096 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f game	= Test.QuickCheck.label "Game.prop_(findQualifiedMovesAvailableTo => countPliesAvailableTo)" $ all (
			\logicalColour -> Model.Game.countPliesAvailableTo logicalColour game == (
				if Model.Game.isTerminated game
					then 0
					else fromIntegral . length $ Model.Game.findQualifiedMovesAvailableTo logicalColour game
			)
		 ) Property.FixedMembership.members
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f = Test.QuickCheck.label "Game.prop_(getNDefendersByCoordinatesByLogicalColour == countDefendersByCoordinatesByLogicalColour)" . uncurry (==) . (
			State.Board.getNDefendersByCoordinatesByLogicalColour &&& State.Board.countDefendersByCoordinatesByLogicalColour
		 ) . Model.Game.getBoard
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 2048 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f = Test.QuickCheck.label "Game.prop_(getNPawnsByFileByLogicalColour => countPawnsByFileByLogicalColour)" . uncurry (==) . (
			State.Board.getNPawnsByFileByLogicalColour &&& State.CoordinatesByRankByLogicalColour.countPawnsByFileByLogicalColour . State.Board.getCoordinatesByRankByLogicalColour
		 ) . Model.Game.getBoard
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f = Test.QuickCheck.label "Game.prop_getNPawnsByFileByLogicalColour/non-zero" . Data.Foldable.all (
			Data.Foldable.all (> 0)
		 ) . State.Board.getNPawnsByFileByLogicalColour . Model.Game.getBoard
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f game	= Test.QuickCheck.label "Game.prop_(getCoordinatesByRankByLogicalColour => getNPawnsByFileByLogicalColour)" . all (
			\(logicalColour, nPawnsByFile) -> Data.Foldable.sum nPawnsByFile == fromIntegral (
				length . State.CoordinatesByRankByLogicalColour.dereference logicalColour Attribute.Rank.Pawn $ State.Board.getCoordinatesByRankByLogicalColour board
			)
		 ) . Data.Array.IArray.assocs $ State.Board.getNPawnsByFileByLogicalColour board where
			board	= Model.Game.getBoard game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f = Test.QuickCheck.label "Game.prop_(getPassedPawnCoordinatesByLogicalColour == findPassedPawnCoordinatesByLogicalColour)" . uncurry (==) . (
			 State.Board.getPassedPawnCoordinatesByLogicalColour &&& State.CoordinatesByRankByLogicalColour.findPassedPawnCoordinatesByLogicalColour . State.Board.getCoordinatesByRankByLogicalColour
		 ) . Model.Game.getBoard
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 512 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f = Test.QuickCheck.label "Game.prop_(getMaybeChecked == isKingChecked)" . uncurry (==) . (
			Data.Maybe.isJust . Model.Game.getMaybeChecked &&& uncurry State.Board.isKingChecked . (Model.Game.getNextLogicalColour &&& Model.Game.getBoard)
		 )
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 512 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f = Test.QuickCheck.label "Game.(prop_getCastleableRooksByLogicalColour == fromTurnsByLogicalColour)" . uncurry (==) . (
			Model.Game.getCastleableRooksByLogicalColour &&& State.CastleableRooksByLogicalColour.fromTurnsByLogicalColour . Model.Game.getTurnsByLogicalColour
		 )
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 1024 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Game.prop_reflectOnX" . uncurry (==) . (id &&& Property.Reflectable.reflectOnX . Property.Reflectable.reflectOnX)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 32 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Game.prop_reflectOnX/isValidQualifiedMove" . all (
			\(game, turn) -> Model.Game.isValidQualifiedMove (Component.Turn.getQualifiedMove turn) game
		 ) . Model.Game.rollBack . Property.Reflectable.reflectOnX
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Game.prop_cantConverge" . not . any (
			\(game, turn) -> Model.Game.cantConverge game $ Model.Game.takeTurn turn game
		 ) . Model.Game.rollBack
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f game	= not (Property.Null.isNull game) ==> Test.QuickCheck.label "Game.prop_rollBack/restart" . (== Data.Default.def) . fst {-game-} . last {-original-} $ Model.Game.rollBack game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 1024 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f game	= Test.QuickCheck.label "Game.prop_rollBack/takeTurn" . (== game) . foldr (
			Model.Game.takeTurn . snd {-turn-}
		 ) Data.Default.def $ Model.Game.rollBack game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 1024 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f game	= Test.QuickCheck.label "Game.prop_(findAttackersOf => listDestinationsFor)" $ all (
			\(destination, destinationLogicalColour, destinationRank, source, sourceRank) -> (destination, Just destinationRank) `elem` State.MaybePieceByCoordinates.listDestinationsFor source (
				Component.Piece.mkPiece (Property.Opposable.getOpposite destinationLogicalColour) sourceRank
			) maybePieceByCoordinates
		 ) [
			(destination, destinationLogicalColour, destinationRank, source, sourceRank) |
				(destination, piece)	<- StateProperty.Seeker.findAllPieces maybePieceByCoordinates,
				let (destinationLogicalColour, destinationRank)	= Component.Piece.getLogicalColour &&& Component.Piece.getRank $ piece,	-- Deconstruct.
				(source, sourceRank)	<- State.Board.findAttackersOf destinationLogicalColour destination board
		 ] {-list-comprehension-} where
			board			= Model.Game.getBoard game
			maybePieceByCoordinates	= State.Board.getMaybePieceByCoordinates board
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 1024 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f game	= Test.QuickCheck.label "Game.prop_(listDestinationsFor => findAttackersOf)" $ all (
			\(source, piece, destination) -> (source, Component.Piece.getRank piece) `elem` State.Board.findAttackersOf (
				Property.Opposable.getOpposite $ Component.Piece.getLogicalColour piece
			) destination board
		 ) [
			(source, piece, destination) |
				(source, piece)		<- StateProperty.Seeker.findAllPieces maybePieceByCoordinates,
				(destination, Just _)	<- State.MaybePieceByCoordinates.listDestinationsFor source piece maybePieceByCoordinates	-- Identify attacks.
		 ] {-list-comprehension-} where
			board			= Model.Game.getBoard game
			maybePieceByCoordinates	= State.Board.getMaybePieceByCoordinates board
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 1024 } f,
	let
		f :: Game -> Test.QuickCheck.Property
		f game	= Test.QuickCheck.label "Game.prop_pawnOrdinates" . all (
			uncurry (&&) . (
				(/= Cartesian.Ordinate.yMin) &&& (/= Cartesian.Ordinate.yMax)
			) . Cartesian.Coordinates.getY . fst {-coordinates-}
		 ) . StateProperty.Seeker.findPieces Component.Piece.isPawn . State.Board.getCoordinatesByRankByLogicalColour $ Model.Game.getBoard game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f
 ]

