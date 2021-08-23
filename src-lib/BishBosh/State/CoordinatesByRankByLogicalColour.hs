{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
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
	NPiecesByFileByLogicalColour,
--	CoordinatesByRank,
	CoordinatesByLogicalColour,
--	Transformation,
-- ** Data-types
	CoordinatesByRankByLogicalColour(
--		MkCoordinatesByRankByLogicalColour,
		deconstruct
	),
-- * Functions
	countPawnsByFileByLogicalColour,
	findPassedPawnCoordinatesByLogicalColour,
	findPiecesOfColour,
	sumPieceSquareValueByLogicalColour,
--	deleteCoordinates,
	assocs,
	listCoordinates,
-- ** Accessors
	getKingsCoordinates,
	dereference,
-- ** Constructor
	fromMaybePieceByCoordinates,
-- ** Mutators
	movePiece,
	sortCoordinates
) where

import			Control.Arrow((&&&))
import			Data.Array.IArray((!), (//))
import qualified	BishBosh.Attribute.Direction				as Attribute.Direction
import qualified	BishBosh.Attribute.LogicalColour			as Attribute.LogicalColour
import qualified	BishBosh.Attribute.Rank					as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa				as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates				as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Vector				as Cartesian.Vector
import qualified	BishBosh.Component.Move					as Component.Move
import qualified	BishBosh.Component.Piece				as Component.Piece
import qualified	BishBosh.Component.PieceSquareByCoordinatesByRank	as Component.PieceSquareByCoordinatesByRank
import qualified	BishBosh.Component.Zobrist				as Component.Zobrist
import qualified	BishBosh.Property.FixedMembership			as Property.FixedMembership
import qualified	BishBosh.Property.Opposable				as Property.Opposable
import qualified	BishBosh.State.MaybePieceByCoordinates			as State.MaybePieceByCoordinates
import qualified	BishBosh.StateProperty.Censor				as StateProperty.Censor
import qualified	BishBosh.StateProperty.Seeker				as StateProperty.Seeker
import qualified	BishBosh.Types						as T
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.Map.Strict
import qualified	Data.Maybe

-- | The /coordinate/s of all the pieces of one /rank/.
type CoordinatesByRank x y	= Attribute.Rank.ArrayByRank [Cartesian.Coordinates.Coordinates x y]

{- |
	* This structure allows one to determine the set of /coordinates/ where a type of /piece/ is located.

	* CAVEAT: the list of /coordinates/ is unordered, so test for equality using @ deconstruct . sortCoordinates @.
-}
newtype CoordinatesByRankByLogicalColour x y	= MkCoordinatesByRankByLogicalColour {
	deconstruct	:: Attribute.LogicalColour.ArrayByLogicalColour (CoordinatesByRank x y)
}

instance (
	Control.DeepSeq.NFData	x,
	Control.DeepSeq.NFData	y
 ) => Control.DeepSeq.NFData (CoordinatesByRankByLogicalColour x y) where
	rnf MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= Control.DeepSeq.rnf byLogicalColour

instance (Enum x, Enum y) => StateProperty.Censor.Censor (CoordinatesByRankByLogicalColour x y) where
	countPiecesByLogicalColour MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= ($ Attribute.LogicalColour.Black) &&& ($ Attribute.LogicalColour.White) $ Data.List.foldl' (\acc -> (+ acc) . length) 0 . (byLogicalColour !)

	countPieces MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= Data.Foldable.foldl' (
		Data.List.foldl' $ \acc -> (+ acc) . length
	 ) 0 byLogicalColour

	countPieceDifferenceByRank MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= Attribute.Rank.listArrayByRank . uncurry (
		zipWith (-)
	 ) . (
		($ Attribute.LogicalColour.White) &&& ($ Attribute.LogicalColour.Black)
	 ) $ map length . Data.Array.IArray.elems . (byLogicalColour !)

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
					byRank	<- Data.Array.IArray.elems byLogicalColour,
					rank	<- [Attribute.Rank.Knight, Attribute.Rank.Bishop]
			 ] -- List-comprehension.

			bishops	= blackBishops ++ whiteBishops

	hasBothKings MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= not $ Data.Foldable.any (null . (! Attribute.Rank.King)) byLogicalColour	-- CAVEAT: true for more than one King per side also.

instance (Enum x, Enum y, Ord x, Ord y) => Component.Zobrist.Hashable2D CoordinatesByRankByLogicalColour x y {-CAVEAT: FlexibleInstances, MultiParamTypeClasses-} where
	listRandoms2D MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour } zobrist	= [
		Component.Zobrist.dereferenceRandomByCoordinatesByRankByLogicalColour (logicalColour, rank, coordinates) zobrist |
			(logicalColour, byRank)	<- Data.Array.IArray.assocs byLogicalColour,
			(rank, coordinatesList)	<- Data.Array.IArray.assocs byRank,
			coordinates		<- coordinatesList
	 ] -- List-comprehension.

{- |
	* Find any @Knight@s of the specified /logical colour/, in attack-range around the specified /coordinates/.

	* CAVEAT: nothing is said about whether any /piece/ at the specified /coordinates/ belongs to the opponent, as one might expect.
-}
instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => StateProperty.Seeker.Seeker CoordinatesByRankByLogicalColour x y {-CAVEAT: MultiParamTypeClasses-} where
	{-# SPECIALISE instance StateProperty.Seeker.Seeker CoordinatesByRankByLogicalColour T.X T.Y #-}
	findProximateKnights logicalColour destination MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= filter (
		\source -> source /= destination {-guard against attempting to constructing a null vector-} && Cartesian.Vector.isKnightsMove (
			Cartesian.Vector.measureDistance source destination	:: Cartesian.Vector.VectorInt
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

-- | Constructor.
fromMaybePieceByCoordinates :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => State.MaybePieceByCoordinates.MaybePieceByCoordinates x y -> CoordinatesByRankByLogicalColour x y
fromMaybePieceByCoordinates maybePieceByCoordinates	= MkCoordinatesByRankByLogicalColour . (
	\(b, w) -> Attribute.LogicalColour.listArrayByLogicalColour $ map (
		Data.Array.IArray.accumArray (++) [] (minBound, maxBound) . map (Control.Arrow.first Component.Piece.getRank)
	) [b, w]
 ) $ Data.List.partition (
	Component.Piece.isBlack . fst {-piece-}
 ) [
	(piece, [coordinates]) |
		(coordinates, piece)	<- StateProperty.Seeker.findAllPieces maybePieceByCoordinates
 ] -- List-comprehension.

-- | Dereference the array.
dereference
	:: Attribute.LogicalColour.LogicalColour
	-> Attribute.Rank.Rank
	-> CoordinatesByRankByLogicalColour x y
	-> [Cartesian.Coordinates.Coordinates x y]
{-# INLINE dereference #-}
dereference logicalColour rank MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= byLogicalColour ! logicalColour ! rank

-- | Build an association-list.
assocs :: CoordinatesByRankByLogicalColour x y -> [(Component.Piece.Piece, [Cartesian.Coordinates.Coordinates x y])]
assocs MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= [
	(Component.Piece.mkPiece logicalColour rank, coordinatesList) |
		(logicalColour, byRank)	<- Data.Array.IArray.assocs byLogicalColour,
		(rank, coordinatesList)	<- Data.Array.IArray.assocs byRank
 ] -- List-comprehension.

-- | Access the coordinate-lists.
listCoordinates :: CoordinatesByRankByLogicalColour x y -> [Cartesian.Coordinates.Coordinates x y]
listCoordinates MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= [
	coordinates |
		byRank		<- Data.Array.IArray.elems byLogicalColour,
		coordinatesList	<- Data.Array.IArray.elems byRank,
		coordinates	<- coordinatesList
 ] -- List-comprehension.

-- | Get the /coordinates/ of the @King@ of the specified /logical colour/.
getKingsCoordinates
	:: Attribute.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the @King@ to find.
	-> CoordinatesByRankByLogicalColour x y
	-> Cartesian.Coordinates.Coordinates x y
{-# INLINE getKingsCoordinates #-}
getKingsCoordinates logicalColour MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= Control.Exception.assert (not $ null coordinates) $ head coordinates {-there should be exactly one-} where
	coordinates	= byLogicalColour ! logicalColour ! Attribute.Rank.King

-- | The number of /piece/s in each file, for each /logical colour/.
type NPiecesByFileByLogicalColour x	= Attribute.LogicalColour.ArrayByLogicalColour (Data.Map.Strict.Map x Component.Piece.NPieces)

{- |
	* Counts the number of @Pawn@s of each /logical colour/ with similar /x/-coordinates; their /y/-coordinate is irrelevant.

	* N.B.: files lacking any @Pawn@, don't feature in the results.
-}
countPawnsByFileByLogicalColour :: Ord x => CoordinatesByRankByLogicalColour x y -> NPiecesByFileByLogicalColour x
{-# INLINABLE countPawnsByFileByLogicalColour #-}
countPawnsByFileByLogicalColour MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= Data.Array.IArray.amap (
	Data.List.foldl' (
		\m coordinates -> Data.Map.Strict.insertWith (const succ) (Cartesian.Coordinates.getX coordinates) 1 m
	) Data.Map.Strict.empty . (! Attribute.Rank.Pawn)
 ) byLogicalColour

-- | Locate all /piece/s of the specified /logical colour/.
findPiecesOfColour
	:: Attribute.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the /piece/s to find.
	-> CoordinatesByRankByLogicalColour x y
	-> [Component.Piece.LocatedPiece x y]
findPiecesOfColour logicalColour MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= [
	(coordinates, Component.Piece.mkPiece logicalColour rank) |
		(rank, coordinatesList)	<- Data.Array.IArray.assocs $ byLogicalColour ! logicalColour,
		coordinates		<- coordinatesList
 ] -- List-comprehension.

-- | A list of /coordinates/ for each /logical colour/.
type CoordinatesByLogicalColour x y	= Attribute.LogicalColour.ArrayByLogicalColour [Cartesian.Coordinates.Coordinates x y]

-- | For each /logical colour/, find the /coordinates/ of any passed @Pawn@s (<https://en.wikipedia.org/wiki/Passed_pawn>).
findPassedPawnCoordinatesByLogicalColour :: (Enum x, Ord x, Ord y) => CoordinatesByRankByLogicalColour x y -> CoordinatesByLogicalColour x y
findPassedPawnCoordinatesByLogicalColour MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= Attribute.LogicalColour.listArrayByLogicalColour $ map (
	\logicalColour	-> let
		opponentsLogicalColour	= Property.Opposable.getOpposite logicalColour
		opposingPawnYByX	= Data.List.foldl' (
			\m coordinates -> uncurry (
				Data.Map.Strict.insertWith $ if Attribute.LogicalColour.isBlack opponentsLogicalColour
					then max
					else min
			) {-only compare with the least advanced opposing Pawn in each file-} (
				Cartesian.Coordinates.getX &&& Cartesian.Coordinates.getY $ coordinates
			) m
		 ) Data.Map.Strict.empty $ findPawns opponentsLogicalColour
	in filter (
		\coordinates -> all (
			Data.Maybe.maybe True {-the absence of an opposing Pawn doesn't impede advancement-} (
				(
					/= Attribute.Direction.advanceDirection logicalColour	-- Either equal or backwards is OK.
				) . (
					{-opponent-} `compare` Cartesian.Coordinates.getY coordinates
				) -- As a Pawn advances, it becomes "Passed" when the y-distance to the least advanced adjacent opposing Pawn, is either equal or backwards.
			 ) . (`Data.Map.Strict.lookup` opposingPawnYByX)
		) . uncurry (:) . (
			id &&& Cartesian.Abscissa.getAdjacents
		) $ Cartesian.Coordinates.getX coordinates
	) $ findPawns logicalColour
 ) Property.FixedMembership.members where
	findPawns	= (! Attribute.Rank.Pawn) . (byLogicalColour !)

-- | Calculate the total value of the /coordinates/ occupied by the /piece/s of either side.
sumPieceSquareValueByLogicalColour
	:: (Num pieceSquareValue)
	=> Component.PieceSquareByCoordinatesByRank.FindPieceSquareValues x y pieceSquareValue
	-> CoordinatesByRankByLogicalColour x y
	-> [pieceSquareValue]
{-# SPECIALISE sumPieceSquareValueByLogicalColour :: Component.PieceSquareByCoordinatesByRank.FindPieceSquareValues T.X T.Y T.PieceSquareValue -> CoordinatesByRankByLogicalColour T.X T.Y -> [T.PieceSquareValue] #-}
sumPieceSquareValueByLogicalColour findPieceSquareValues MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= map (
	\(logicalColour, byRank) -> Data.List.foldl' (
		\acc	-> Data.List.foldl' (+) acc . uncurry (findPieceSquareValues logicalColour)
	) 0 $ Data.Array.IArray.assocs byRank
 ) $ Data.Array.IArray.assocs byLogicalColour

-- | Self-documentation.
type Transformation x y	= CoordinatesByRankByLogicalColour x y -> CoordinatesByRankByLogicalColour x y

-- | Remove the specified /coordinates/ from those recorded for the specified /rank/.
deleteCoordinates
	:: (Eq x, Eq y)
	=> Cartesian.Coordinates.Coordinates x y
	-> Attribute.Rank.Rank
	-> CoordinatesByRank x y
	-> CoordinatesByRank x y
deleteCoordinates coordinates rank byRank	= byRank // [(rank, Data.List.delete coordinates $ byRank ! rank)]

-- | Adjust the array to reflect a new /move/.
movePiece
	:: (Eq x, Eq y)
	=> Component.Move.Move x y
	-> Component.Piece.Piece							-- ^ The piece which moved.
	-> Maybe Attribute.Rank.Rank							-- ^ The (possibly promoted) rank to place at the destination.
	-> Either (Cartesian.Coordinates.Coordinates x y) (Maybe Attribute.Rank.Rank)	-- ^ Either the destination of any passed @Pawn@, or the /rank/ of any /piece/ taken.
	-> Transformation x y
movePiece move sourcePiece maybePromotionRank eitherPassingPawnsDestinationOrMaybeTakenRank MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= MkCoordinatesByRankByLogicalColour $ byLogicalColour // either (
	(:) . (`deleteOpponentsCoordinates` Attribute.Rank.Pawn)
 ) (
	Data.Maybe.maybe id {-quiet move-} $ (:) . deleteOpponentsCoordinates destination
 ) eitherPassingPawnsDestinationOrMaybeTakenRank [
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
	deleteOpponentsCoordinates coordinates rank	= id &&& deleteCoordinates coordinates rank . (byLogicalColour !) $ Property.Opposable.getOpposite logicalColour

-- | Independently sort each list of /coordinates/.
sortCoordinates :: (Ord x, Ord y) => Transformation x y
sortCoordinates MkCoordinatesByRankByLogicalColour { deconstruct = byLogicalColour }	= MkCoordinatesByRankByLogicalColour $ Data.Array.IArray.amap (Data.Array.IArray.amap Data.List.sort) byLogicalColour
