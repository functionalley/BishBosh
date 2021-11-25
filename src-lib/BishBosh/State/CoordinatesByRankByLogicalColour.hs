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

 [@DESCRIPTION@]

	* A view of the /board/ from the perspective of its /piece/s.

	* cf. the square-centric model of the board defined in "BishBosh.State.MaybePieceByCoordinates".
-}

module BishBosh.State.CoordinatesByRankByLogicalColour(
-- * Types
-- ** Type-synonyms
--	CoordinatesByRank,
	BareCoordinatesByRankByLogicalColour,
	CoordinatesByLogicalColour,
--	Transformation,
-- ** Data-types
	CoordinatesByRankByLogicalColour(
--		MkCoordinatesByRankByLogicalColour,
		deconstruct
	),
-- * Functions
--	advanceDirection,
	findPassedPawnCoordinatesByLogicalColour,
	findPiecesOfColour,
	assocs,
	listCoordinates,
-- ** Accessors
	getKingsCoordinates,
	dereference,
-- ** Mutators
--	deleteCoordinatesFromRank,
--	mapCoordinates,
--	purgeCoordinates,
	sortCoordinates
) where

import			Control.Arrow((&&&), (|||))
import			Data.Array.IArray((!), (//))
import qualified	BishBosh.Attribute.Rank					as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa				as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates				as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Vector				as Cartesian.Vector
import qualified	BishBosh.Colour.LogicalColour				as Colour.LogicalColour
import qualified	BishBosh.Component.Accountant				as Component.Accountant
import qualified	BishBosh.Component.Move					as Component.Move
import qualified	BishBosh.Component.Piece				as Component.Piece
import qualified	BishBosh.Component.PieceSquareByCoordinatesByRank	as Component.PieceSquareByCoordinatesByRank
import qualified	BishBosh.Component.Zobrist				as Component.Zobrist
import qualified	BishBosh.Property.Empty					as Property.Empty
import qualified	BishBosh.Property.FixedMembership			as Property.FixedMembership
import qualified	BishBosh.Property.Opposable				as Property.Opposable
import qualified	BishBosh.Property.SelfValidating			as Property.SelfValidating
import qualified	BishBosh.StateProperty.Censor				as StateProperty.Censor
import qualified	BishBosh.StateProperty.Hashable				as StateProperty.Hashable
import qualified	BishBosh.StateProperty.Mutator				as StateProperty.Mutator
import qualified	BishBosh.StateProperty.Seeker				as StateProperty.Seeker
import qualified	BishBosh.StateProperty.View				as StateProperty.View
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.List.Extra
import qualified	Data.Map.Strict						as Map
import qualified	Data.Maybe

-- | The /coordinate/s of all the pieces of one /rank/.
type CoordinatesByRank	= Attribute.Rank.ArrayByRank [Cartesian.Coordinates.Coordinates]

-- | The /coordinate/s of all the pieces of one /rank/, for both logical colours.
type BareCoordinatesByRankByLogicalColour	= Colour.LogicalColour.ArrayByLogicalColour CoordinatesByRank

{- |
	* This structure allows one to determine the set of /coordinates/ where a type of /piece/ is located.

	* CAVEAT: the list of /coordinates/ is unordered, so test for equality using @ deconstruct . sortCoordinates @.
-}
newtype CoordinatesByRankByLogicalColour	= MkCoordinatesByRankByLogicalColour {
	deconstruct	:: BareCoordinatesByRankByLogicalColour
}

instance Control.DeepSeq.NFData CoordinatesByRankByLogicalColour where
	rnf MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= Control.DeepSeq.rnf byLogicalColour

instance StateProperty.Censor.Censor CoordinatesByRankByLogicalColour where
	countPiecesByLogicalColour MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= ($ Colour.LogicalColour.Black) &&& ($ Colour.LogicalColour.White) $ Data.List.foldl' (\acc -> (+ acc) . fromIntegral . length) 0 . (byLogicalColour !)

	countPieces MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= Data.Foldable.foldl' (
		Data.List.foldl' $ \acc -> (+ acc) . fromIntegral . length
	 ) 0 byLogicalColour

	countPieceDifferenceByRank MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= Attribute.Rank.listArrayByRank . uncurry (
		zipWith (-)
	 ) . (
		($ Colour.LogicalColour.White) &&& ($ Colour.LogicalColour.Black)
	 ) $ map (fromIntegral . length) . Data.Foldable.toList . (byLogicalColour !)

	hasInsufficientMaterial MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= Data.Foldable.all (
		\byRank -> all (
			null . (byRank !)
		) Attribute.Rank.individuallySufficientMaterial
	 ) byLogicalColour && case blackKnights ++ whiteKnights of
		[]	-> Cartesian.Coordinates.areSquaresIsochromatic bishops
		[_]	-> null bishops
		_	-> False
		where
			[blackKnights, blackBishops, whiteKnights, whiteBishops]	= [
				byRank ! rank |
					byRank	<- Data.Foldable.toList byLogicalColour,
					rank	<- [Attribute.Rank.Knight, Attribute.Rank.Bishop]
			 ] -- List-comprehension.

			bishops	= blackBishops ++ whiteBishops

	hasBothKings MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= Data.Foldable.all ((== 1) . length . (! Attribute.Rank.King)) byLogicalColour	-- CAVEAT: false for more than one King per side also.

instance StateProperty.Hashable.Hashable CoordinatesByRankByLogicalColour where
	listRandoms MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour } zobrist	= [
		Component.Zobrist.dereferenceRandomByCoordinatesByRankByLogicalColour (logicalColour, rank, coordinates) zobrist |
			(logicalColour, byRank)	<- Data.Array.IArray.assocs byLogicalColour,
			(rank, coordinatesList)	<- Data.Array.IArray.assocs byRank,
			coordinates		<- coordinatesList
	 ] -- List-comprehension.

instance StateProperty.Mutator.Mutator CoordinatesByRankByLogicalColour where
	defineCoordinates maybePiece coordinates	= MkCoordinatesByRankByLogicalColour . (
		\byLogicalColour -> Data.Maybe.maybe byLogicalColour (
			\piece -> let
				logicalColour	= Component.Piece.getLogicalColour piece
				byRank		= byLogicalColour ! logicalColour
			in byLogicalColour // [
				(
					logicalColour,
					byRank // [
						id &&& (coordinates :) . (byRank !) $ Component.Piece.getRank piece	-- Prepend.
					] -- Singleton.
				) -- Pair.
			] -- Singleton.
		) maybePiece
	 ) . deconstruct . purgeCoordinates coordinates

	movePiece move sourcePiece maybePromotionRank eitherPassingPawnsDestinationOrMaybeTakenRank MkCoordinatesByRankByLogicalColour {
		deconstruct	= byLogicalColour
	} = MkCoordinatesByRankByLogicalColour $ byLogicalColour // (
		(:) . (`deleteOpponentsCoordinates` Attribute.Rank.Pawn) ||| Data.Maybe.maybe id {-quiet move-} (
			(:) . deleteOpponentsCoordinates destination
		) $ eitherPassingPawnsDestinationOrMaybeTakenRank
	 ) [
		let
			byRank	= byLogicalColour ! logicalColour
		in (
			logicalColour,
			byRank // Data.Maybe.maybe (
				return {-to List-monad-} . Control.Arrow.second (destination :)	-- Add the destination to the mover.
			) (
				\promotionRank -> (:) (
					promotionRank,
					destination : byRank ! promotionRank	-- Add the destination to the mover's promoted rank.
				) . return {-to List-monad-}
			) maybePromotionRank (
				id &&& Data.List.delete (Component.Move.getSource move) . (byRank !) $ Component.Piece.getRank sourcePiece
			)
		) -- Pair.
	 ] where
		destination					= Component.Move.getDestination move
		logicalColour					= Component.Piece.getLogicalColour sourcePiece
		deleteOpponentsCoordinates coordinates rank	= id &&& deleteCoordinatesFromRank coordinates rank . (byLogicalColour !) $ Property.Opposable.getOpposite logicalColour

{- |
	* Find any @Knight@s of the specified /logical colour/, in attack-range around the specified /coordinates/.

	* CAVEAT: nothing is said about whether any /piece/ at the specified /coordinates/ belongs to the opponent, as one might expect.
-}
instance StateProperty.Seeker.Seeker CoordinatesByRankByLogicalColour {-CAVEAT: MultiParamTypeClasses-} where
	findProximateKnights logicalColour destination MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= filter (
		\source -> source /= destination {-guard against attempting to constructing a null vector-} && Cartesian.Vector.isKnightsMove (
			Cartesian.Vector.measureDistance source destination
		)
	 ) $ byLogicalColour ! logicalColour ! Attribute.Rank.Knight

	findPieces predicate MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= [
		(coordinates, piece) |
			(logicalColour, byRank)	<- Data.Array.IArray.assocs byLogicalColour,
			(rank, coordinatesList)	<- Data.Array.IArray.assocs byRank,
			let piece	= Component.Piece.mkPiece logicalColour rank,
			predicate piece,
			coordinates		<- coordinatesList
	 ] -- List-comprehension.

	countPawnsByFileByLogicalColour MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= Data.Array.IArray.amap (
		Data.List.foldl' (
			\m coordinates -> StateProperty.Seeker.accumulatePawnsByFile (Cartesian.Coordinates.getX coordinates) m
		) Property.Empty.empty . (! Attribute.Rank.Pawn)
	 ) byLogicalColour

instance StateProperty.View.View CoordinatesByRankByLogicalColour where
	fromAssocs	= MkCoordinatesByRankByLogicalColour . Data.Array.IArray.accumArray (
		flip const	-- Replace the default.
	 ) (
		Attribute.Rank.listArrayByRank $ repeat []	-- Default.
	 ) (minBound, maxBound) . map (
		Control.Arrow.second $ Data.Array.IArray.accumArray (++) [] {-default-} (minBound, maxBound) . Data.List.Extra.groupSort {-by Rank-}	-- Construct the ArrayByRank.
	 ) . Data.List.Extra.groupSort {-by LogicalColour-} . map (
		\(coordinates, piece) -> (Component.Piece.getLogicalColour piece, (Component.Piece.getRank piece, coordinates))	-- Reorder the components.
	 )

instance Component.Accountant.Accountant CoordinatesByRankByLogicalColour where
	sumPieceSquareValueByLogicalColour pieceSquareByCoordinatesByRank nPieces MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= map (
		\(logicalColour, byRank) -> Data.List.foldl' (
			\acc (rank, coordinatesList) -> Data.List.foldl' (
				\acc' -> (+ acc') . realToFrac . Component.PieceSquareByCoordinatesByRank.findPieceSquareValue pieceSquareByCoordinatesByRank nPieces logicalColour rank
			) acc coordinatesList
		) 0 $ Data.Array.IArray.assocs byRank
	 ) $ Data.Array.IArray.assocs byLogicalColour

instance Property.SelfValidating.SelfValidating CoordinatesByRankByLogicalColour where
	findInvalidity selfValidator	= concatMap ($ selfValidator) [
		StateProperty.Censor.findInvalidity,
		StateProperty.Seeker.findInvalidity,
		Property.SelfValidating.findErrors [
			(
				not . all ((== 1) . length) . Data.List.group . Data.List.sort . listCoordinates,
				"there can't be any duplicate coordinates regardless of logical colour or ranks."
			)
		]
	 ]

-- | Dereference the array.
dereference
	:: Colour.LogicalColour.LogicalColour
	-> Attribute.Rank.Rank
	-> CoordinatesByRankByLogicalColour
	-> [Cartesian.Coordinates.Coordinates]
{-# INLINE dereference #-}
dereference logicalColour rank MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= byLogicalColour ! logicalColour ! rank

-- | Build an association-list.
assocs :: CoordinatesByRankByLogicalColour -> [(Component.Piece.Piece, [Cartesian.Coordinates.Coordinates])]
assocs MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= [
	(Component.Piece.mkPiece logicalColour rank, coordinatesList) |
		(logicalColour, byRank)	<- Data.Array.IArray.assocs byLogicalColour,
		(rank, coordinatesList)	<- Data.Array.IArray.assocs byRank
 ] -- List-comprehension.

-- | Access the coordinate-lists.
listCoordinates :: CoordinatesByRankByLogicalColour -> [Cartesian.Coordinates.Coordinates]
listCoordinates MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= [
	coordinates |
		byRank		<- Data.Foldable.toList byLogicalColour,
		coordinatesList	<- Data.Foldable.toList byRank,
		coordinates	<- coordinatesList
 ] -- List-comprehension.

-- | Get the /coordinates/ of the @King@ of the specified /logical colour/.
getKingsCoordinates
	:: Colour.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the @King@ to find.
	-> CoordinatesByRankByLogicalColour
	-> Cartesian.Coordinates.Coordinates
{-# INLINE getKingsCoordinates #-}
getKingsCoordinates logicalColour MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= coordinates where
	[coordinates]	= byLogicalColour ! logicalColour ! Attribute.Rank.King	-- CAVEAT: there should be exactly one.

-- | Locate all /piece/s of the specified /logical colour/.
findPiecesOfColour
	:: Colour.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the /piece/s to find.
	-> CoordinatesByRankByLogicalColour
	-> [Component.Piece.LocatedPiece]
findPiecesOfColour logicalColour MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= [
	(coordinates, Component.Piece.mkPiece logicalColour rank) |
		(rank, coordinatesList)	<- Data.Array.IArray.assocs $ byLogicalColour ! logicalColour,
		coordinates		<- coordinatesList
 ] -- List-comprehension.

-- | The /y/-direction in which a @Pawn@ of the specified /logical colour/ advances.
advanceDirection :: Colour.LogicalColour.LogicalColour -> Ordering
advanceDirection Colour.LogicalColour.Black	= LT	-- Black moves down.
advanceDirection _				= GT

-- | A list of /coordinates/ for each /logical colour/.
type CoordinatesByLogicalColour	= Colour.LogicalColour.ArrayByLogicalColour [Cartesian.Coordinates.Coordinates]

-- | For each /logical colour/, find the /coordinates/ of any passed @Pawn@s (<https://en.wikipedia.org/wiki/Passed_pawn>).
findPassedPawnCoordinatesByLogicalColour :: CoordinatesByRankByLogicalColour -> CoordinatesByLogicalColour
findPassedPawnCoordinatesByLogicalColour MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= Colour.LogicalColour.listArrayByLogicalColour $ map (
	\logicalColour	-> let
		opponentsLogicalColour	= Property.Opposable.getOpposite logicalColour
		opposingPawnYByX	= Data.List.foldl' (
			\m coordinates -> uncurry (
				Map.insertWith $ if Colour.LogicalColour.isBlack opponentsLogicalColour
					then max
					else min
			) {-only compare with the least advanced opposing Pawn in each file-} (
				Cartesian.Coordinates.getX &&& Cartesian.Coordinates.getY $ coordinates
			) m
		 ) Property.Empty.empty $ findPawns opponentsLogicalColour
	in filter (
		\coordinates -> all (
			Data.Maybe.maybe True {-absence of opposition doesn't impede advance-} (
				(
					/= advanceDirection logicalColour	-- Either equal or backwards is OK.
				) . (
					{-opponent-} `compare` Cartesian.Coordinates.getY coordinates
				) -- As a Pawn advances, it becomes "Passed" when the y-distance to the least advanced adjacent opposing Pawn, is either equal or backwards.
			 ) . (`Map.lookup` opposingPawnYByX)
		) . uncurry (:) . (
			id &&& Cartesian.Abscissa.getAdjacents
		) $ Cartesian.Coordinates.getX coordinates
	) $ findPawns logicalColour
 ) Property.FixedMembership.members where
	findPawns	= (! Attribute.Rank.Pawn) . (byLogicalColour !)

-- | Remove the specified /coordinates/ from those recorded for the specified /rank/.
deleteCoordinatesFromRank
	:: Cartesian.Coordinates.Coordinates
	-> Attribute.Rank.Rank
	-> CoordinatesByRank
	-> CoordinatesByRank
deleteCoordinatesFromRank coordinates rank byRank	= byRank // [id &&& Data.List.delete coordinates . (byRank !) $ rank]

-- | Self-documentation.
type Transformation	= CoordinatesByRankByLogicalColour -> CoordinatesByRankByLogicalColour

-- | Map the coordinate-lists.
mapCoordinates :: ([Cartesian.Coordinates.Coordinates] -> [Cartesian.Coordinates.Coordinates]) -> Transformation
mapCoordinates f MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= MkCoordinatesByRankByLogicalColour $ Data.Array.IArray.amap (Data.Array.IArray.amap f) byLogicalColour

-- | Purge the specified /coordinates/ regardless of the /rank/ or /logical colour/ of any incumbent piece.
purgeCoordinates :: Cartesian.Coordinates.Coordinates -> Transformation
purgeCoordinates coordinates	= mapCoordinates $ Data.List.delete coordinates

-- | Independently sort each list of /coordinates/.
sortCoordinates :: Transformation
sortCoordinates	= mapCoordinates Data.List.sort

