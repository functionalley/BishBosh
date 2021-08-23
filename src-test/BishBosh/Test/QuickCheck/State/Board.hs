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

module BishBosh.Test.QuickCheck.State.Board(
-- * Types
-- ** Type-synonyms
	Board,
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Attribute.LogicalColour()
import			BishBosh.Test.QuickCheck.Component.Piece()
import			Control.Arrow((&&&))
import			Data.Array.IArray((!))
import qualified	BishBosh.Attribute.LogicalColour			as Attribute.LogicalColour
import qualified	BishBosh.Attribute.Rank					as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa				as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates				as Cartesian.Coordinates
import qualified	BishBosh.Component.Move					as Component.Move
import qualified	BishBosh.Component.Piece				as Component.Piece
import qualified	BishBosh.Property.Empty					as Property.Empty
import qualified	BishBosh.Property.FixedMembership			as Property.FixedMembership
import qualified	BishBosh.Property.ForsythEdwards			as Property.ForsythEdwards
import qualified	BishBosh.Property.Opposable				as Property.Opposable
import qualified	BishBosh.Property.Reflectable				as Property.Reflectable
import qualified	BishBosh.State.Board					as State.Board
import qualified	BishBosh.State.CoordinatesByRankByLogicalColour		as State.CoordinatesByRankByLogicalColour
import qualified	BishBosh.State.MaybePieceByCoordinates			as State.MaybePieceByCoordinates
import qualified	BishBosh.StateProperty.Mutator				as StateProperty.Mutator
import qualified	BishBosh.StateProperty.Seeker				as StateProperty.Seeker
import qualified	BishBosh.Test.QuickCheck.Cartesian.Coordinates		as Test.QuickCheck.Cartesian.Coordinates
import qualified	BishBosh.Types						as T
import qualified	Control.Monad
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Data.Ord
import qualified	Data.Set
import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO

-- | Defines a concrete type for testing.
type Board	= State.Board.Board T.X T.Y

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => Test.QuickCheck.Arbitrary (State.Board.Board x y) where
	{-# SPECIALISE instance Test.QuickCheck.Arbitrary Board #-}
	arbitrary	= let
		isKingChecked :: (Enum x, Enum y, Ord x, Ord y) => Attribute.LogicalColour.LogicalColour -> State.Board.Board x y -> Bool
		isKingChecked logicalColour board = not . all (
			null . ($ board) . State.Board.findAttackersOf logicalColour
		 ) . State.CoordinatesByRankByLogicalColour.dereference logicalColour Attribute.Rank.King $ State.Board.getCoordinatesByRankByLogicalColour board
	 in Control.Monad.foldM (
		\board piece -> Test.QuickCheck.suchThat (
			fmap (
				($ board) . StateProperty.Mutator.placePiece piece	-- Mutate the board.
			) . Test.QuickCheck.suchThat Test.QuickCheck.arbitrary {-destination-} $ uncurry (&&) . (
				Data.Maybe.maybe True {-unoccupied-} (
					not . Component.Piece.isKing	-- Avoid taking a King.
				) . (`State.MaybePieceByCoordinates.dereference` State.Board.getMaybePieceByCoordinates board) &&& not . (`Component.Piece.isPawnPromotion` piece)	-- Avoid impossible scenarios.
			) -- Predicate.
		) $ not . uncurry (||) . (
			isKingChecked minBound &&& isKingChecked maxBound
		) -- Predicate.
	 ) Property.Empty.empty {-Board-} $ Data.List.sortBy (
		Data.Ord.comparing Component.Piece.getRank	-- Minimise the chance that either 'selectDestination' or 'mutateBoard' must recurse, by moving both Kings to the end of the list.
	 ) Property.FixedMembership.members

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Board -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Board.prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: String -> Test.QuickCheck.Property
		f garbage	= Test.QuickCheck.label "Board.prop_read" $ case (reads garbage :: [(Board, String)]) of
			[_]	-> True
			_	-> True	-- Unless the read-implementation throws an exception.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Board -> String -> Test.QuickCheck.Property
		f board	= Test.QuickCheck.label "Board.prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (`elem` ('/' : Component.Piece.showPieces ++ concatMap show [1 .. Cartesian.Abscissa.xLength])) board
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Board -> Test.QuickCheck.Property
		f board	= Test.QuickCheck.label "Board.prop_fen" $ case Property.ForsythEdwards.readsFEN $ Property.ForsythEdwards.showFEN board of
			[(board', "")]	-> board' == board
			_		-> False
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 16 } f,
	let
		f :: Test.QuickCheck.Cartesian.Coordinates.Coordinates -> Attribute.LogicalColour.LogicalColour -> Test.QuickCheck.Property
		f source logicalColour	= Test.QuickCheck.label "Board.prop_bishopsMove/logicalColour" . all (
			(== Cartesian.Coordinates.getLogicalColourOfSquare source) . Cartesian.Coordinates.getLogicalColourOfSquare . fst {-coordinates-}
		 ) . State.MaybePieceByCoordinates.listDestinationsFor source piece . State.Board.getMaybePieceByCoordinates $ StateProperty.Mutator.placeFirstPiece piece source where
			piece	= Component.Piece.mkBishop logicalColour
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Test.QuickCheck.Cartesian.Coordinates.Coordinates -> Attribute.LogicalColour.LogicalColour -> Test.QuickCheck.Property
		f source logicalColour	= Test.QuickCheck.label "Board.prop_knightsMove/logicalColour" . all (
			(/= Cartesian.Coordinates.getLogicalColourOfSquare source) . Cartesian.Coordinates.getLogicalColourOfSquare . fst {-coordinates-}
		 ) . State.MaybePieceByCoordinates.listDestinationsFor source piece . State.Board.getMaybePieceByCoordinates $ StateProperty.Mutator.placeFirstPiece piece source where
			piece	= Component.Piece.mkKnight logicalColour
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Board -> Test.QuickCheck.Property
		f board	= Test.QuickCheck.label "Board.prop_listDestinationsFor/unique" $ all (
			\(coordinates, piece) -> uncurry (==) . (
				length &&& length . Data.List.nub
			) $ State.MaybePieceByCoordinates.listDestinationsFor coordinates piece maybePieceByCoordinates
		 ) $ StateProperty.Seeker.findAllPieces maybePieceByCoordinates where
			maybePieceByCoordinates	= State.Board.getMaybePieceByCoordinates board
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Board -> Test.QuickCheck.Property
		f board	= Test.QuickCheck.label "Board.prop_(findAttacksBy <=> findAttackersOf)" $ all (
			\(piece, coordinates) -> let
				logicalColour	= Component.Piece.getLogicalColour piece
			in all (
				\destination -> foldr (
					\rank m -> foldr (
						\source -> Data.Map.insertWith Data.Set.union source $ Data.Set.singleton rank
					) m $ State.Board.findAttacksBy (
						Component.Piece.mkPiece (Property.Opposable.getOpposite logicalColour) rank
					) destination board
				) Data.Map.empty Property.FixedMembership.members == foldr (
					\(source, rank)	-> Data.Map.insertWith Data.Set.union source $ Data.Set.singleton rank
				) Data.Map.empty (
					State.Board.findAttackersOf logicalColour destination board
				)
			) coordinates
		 ) . State.CoordinatesByRankByLogicalColour.assocs $ State.Board.getCoordinatesByRankByLogicalColour board
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Board -> Test.QuickCheck.Property
		f board	= Test.QuickCheck.label "Board.prop_(findBlockingPiece => isObstructed)" $ all (
			\(source, _) -> let
				isClear	= ($ maybePieceByCoordinates) . State.MaybePieceByCoordinates.isClear source
			in all (
				\direction -> Data.Maybe.maybe (
					Data.Maybe.maybe True isClear . Data.Maybe.listToMaybe . reverse $ Cartesian.Coordinates.extrapolate direction source
				) (
					isClear . fst {-destination-}
				) $ State.MaybePieceByCoordinates.findBlockingPiece direction source maybePieceByCoordinates
			) Property.FixedMembership.members
		 ) $ StateProperty.Seeker.findAllPieces maybePieceByCoordinates where
			maybePieceByCoordinates	= State.Board.getMaybePieceByCoordinates board
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Board -> Test.QuickCheck.Property
		f = Test.QuickCheck.label "Board.prop_findPieces" . uncurry (==) . (
			Data.List.sort . StateProperty.Seeker.findAllPieces . State.Board.getCoordinatesByRankByLogicalColour &&& Data.List.sort . StateProperty.Seeker.findAllPieces . State.Board.getMaybePieceByCoordinates
		 )
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Board -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Board.prop_reflectOnX" . uncurry (==) . (id &&& Property.Reflectable.reflectOnX . Property.Reflectable.reflectOnX)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 32 } f,
	let
		f :: Board -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Board.prop_reflectOnY" . uncurry (==) . (id &&& Property.Reflectable.reflectOnY . Property.Reflectable.reflectOnY)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 32 } f,
	let
		f :: Board -> Test.QuickCheck.Property
		f = Test.QuickCheck.label "Board.prop_countPawnsByFileByLogicalColour" . (
			\coordinatesByRankByLogicalColour -> all (
				uncurry (==) . (
					Data.Foldable.sum . (
						State.CoordinatesByRankByLogicalColour.countPawnsByFileByLogicalColour coordinatesByRankByLogicalColour !
					) &&& length . (
						\logicalColour -> State.CoordinatesByRankByLogicalColour.dereference logicalColour Attribute.Rank.Pawn coordinatesByRankByLogicalColour
					)
				)
			) Property.FixedMembership.members
		 ) . State.Board.getCoordinatesByRankByLogicalColour
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Board -> Test.QuickCheck.Property
		f board	= Test.QuickCheck.label "Board.prop_(exposesKing => isKingChecked)" $ all (
			\(logicalColour, move)	-> State.Board.isKingChecked logicalColour $ State.Board.movePiece move Nothing board
		 ) [
			(logicalColour, move) |
				(source, piece)		<- StateProperty.Seeker.findAllPieces $ State.Board.getMaybePieceByCoordinates board,
				let logicalColour	= Component.Piece.getLogicalColour piece,
				(destination, _)	<- State.MaybePieceByCoordinates.listDestinationsFor source piece $ State.Board.getMaybePieceByCoordinates board,
				let move	= Component.Move.mkMove source destination,
				State.Board.exposesKing logicalColour move board
		 ]	-- List-comprehension.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 1024 } f,
	let
		f :: Board -> Test.QuickCheck.Property
		f board	= Test.QuickCheck.label "Board.prop_findProximateKnights" . all (
			\(coordinates, piece) -> let
				logicalColour	= Property.Opposable.getOpposite $ Component.Piece.getLogicalColour piece
			in Data.List.sort (
				StateProperty.Seeker.findProximateKnights logicalColour coordinates maybePieceByCoordinates
			) == Data.List.sort (
				StateProperty.Seeker.findProximateKnights logicalColour coordinates coordinatesByRankByLogicalColour
			)
		 ) $ StateProperty.Seeker.findAllPieces maybePieceByCoordinates where
			(maybePieceByCoordinates, coordinatesByRankByLogicalColour)	= State.Board.getMaybePieceByCoordinates &&& State.Board.getCoordinatesByRankByLogicalColour $ board
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f
 ]

