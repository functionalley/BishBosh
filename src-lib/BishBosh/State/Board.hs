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

 [@DESCRIPTION@]

	* This data-type maintains the state of the board, but it doesn't know its history.
	In consequence it knows neither whether castling has occurred, nor which @Pawn@s have been promoted, nor whose turn it is.

	* It allows unvalidated access to the board, to place, move, or remove /piece/s.
	In consequence;
		it enforces neither a conventional layout for the /piece/s nor even that there is exactly one @King@ per side;
		it permits one to move into check or to take a @King@.

	* Two models of the board are simultaneously maintained; a square-centric model "State.MaybePieceByCoordinates" & a piece-centric model "State.CoordinatesByRankByLogicalColour".
	Though maintenance of two models is a burden, the duality permits alternative implementations of the required searches, & often one is more efficient than the other.
-}

module BishBosh.State.Board(
-- * Types
-- ** Type-synonyms
--	Transformation,
--	NDefendersByCoordinatesByLogicalColour,
-- ** Data-types
	Board(
--		MkBoard,
		getMaybePieceByCoordinates,
		getCoordinatesByRankByLogicalColour,
		getNDefendersByCoordinatesByLogicalColour,
		getNPiecesDifferenceByRank,
		getNPawnsByFileByLogicalColour,
		getNPieces,
		getPassedPawnCoordinatesByLogicalColour
	),
-- * Functions
	countDefendersByCoordinatesByLogicalColour,
	summariseNDefendersByLogicalColour,
	sumPieceSquareValueByLogicalColour,
	findAttackersOf,
	findAttacksBy,
-- ** Constructors
--	fromMaybePieceByCoordinates,
-- ** Mutators
	movePiece,
-- ** Predicates
	isKingChecked,
	exposesKing
) where

import			Control.Arrow((&&&), (***), (|||))
import			Data.Array.IArray((!), (//))
import qualified	BishBosh.Attribute.MoveType				as Attribute.MoveType
import qualified	BishBosh.Attribute.Rank					as Attribute.Rank
import qualified	BishBosh.Cartesian.Coordinates				as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Vector				as Cartesian.Vector
import qualified	BishBosh.Colour.LogicalColour				as Colour.LogicalColour
import qualified	BishBosh.Component.Accountant				as Component.Accountant
import qualified	BishBosh.Component.Move					as Component.Move
import qualified	BishBosh.Component.Piece				as Component.Piece
import qualified	BishBosh.Component.PieceSquareByCoordinatesByRank	as Component.PieceSquareByCoordinatesByRank
import qualified	BishBosh.Data.Exception					as Data.Exception
import qualified	BishBosh.Direction.Direction				as Direction.Direction
import qualified	BishBosh.Property.Empty					as Property.Empty
import qualified	BishBosh.Property.ExtendedPositionDescription		as Property.ExtendedPositionDescription
import qualified	BishBosh.Property.FixedMembership			as Property.FixedMembership
import qualified	BishBosh.Property.ForsythEdwards			as Property.ForsythEdwards
import qualified	BishBosh.Property.Opposable				as Property.Opposable
import qualified	BishBosh.Property.Reflectable				as Property.Reflectable
import qualified	BishBosh.Property.SelfValidating			as Property.SelfValidating
import qualified	BishBosh.State.CoordinatesByRankByLogicalColour		as State.CoordinatesByRankByLogicalColour
import qualified	BishBosh.State.MaybePieceByCoordinates			as State.MaybePieceByCoordinates
import qualified	BishBosh.StateProperty.Censor				as StateProperty.Censor
import qualified	BishBosh.StateProperty.Hashable				as StateProperty.Hashable
import qualified	BishBosh.StateProperty.Mutator				as StateProperty.Mutator
import qualified	BishBosh.StateProperty.Seeker				as StateProperty.Seeker
import qualified	BishBosh.StateProperty.View				as StateProperty.View
import qualified	BishBosh.Type.Count					as Type.Count
import qualified	BishBosh.Type.Mass					as Type.Mass
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.Default
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.Map.Strict						as Map
import qualified	Data.Maybe
import qualified	ToolShed.Data.List

-- | The type of a function which transforms a /board/.
type Transformation	= Board -> Board

-- | The number of defenders for each /piece/, belonging to each side.
type NDefendersByCoordinatesByLogicalColour	= Colour.LogicalColour.ArrayByLogicalColour (Map.Map Cartesian.Coordinates.Coordinates Type.Count.NPieces)

{- |
	* The board is modelled as two alternative structures representing the same data, but indexed by either /coordinates/ or /piece/.

	* For efficiency some ancillary structures are also maintained.
-}
data Board	= MkBoard {
	getMaybePieceByCoordinates			:: State.MaybePieceByCoordinates.MaybePieceByCoordinates,			-- ^ Defines any /piece/ currently located at each /coordinate/.
	getCoordinatesByRankByLogicalColour		:: State.CoordinatesByRankByLogicalColour.CoordinatesByRankByLogicalColour,	-- ^ The /coordinates/ of each /piece/.
	getNDefendersByCoordinatesByLogicalColour	:: NDefendersByCoordinatesByLogicalColour,					-- ^ The number of defenders of each /piece/, indexed by /logical colour/ & then by /coordinates/.
	getNPiecesDifferenceByRank			:: StateProperty.Censor.NPiecesByRank,						-- ^ The difference in the number of /piece/s of each /rank/ held by either side. @White@ /piece/s are arbitrarily considered positive & @Black@ ones negative.
	getNPawnsByFileByLogicalColour			:: StateProperty.Seeker.NPiecesByFileByLogicalColour,				-- ^ The number of @Pawn@s of each /logical colour/, for each /file/.
	getNPieces					:: Type.Count.NPieces,								-- ^ The total number of pieces on the board, including @Pawn@s.
	getPassedPawnCoordinatesByLogicalColour		:: State.CoordinatesByRankByLogicalColour.CoordinatesByLogicalColour		-- ^ The /coordinates/ of any /passed/ @Pawn@s.
}

instance Eq Board where
	MkBoard { getMaybePieceByCoordinates = maybePieceByCoordinates } == MkBoard { getMaybePieceByCoordinates = maybePieceByCoordinates' }	= maybePieceByCoordinates == maybePieceByCoordinates'	-- N.B.: the remaining fields are implied.

instance Control.DeepSeq.NFData Board where
	rnf MkBoard {
		getMaybePieceByCoordinates			= maybePieceByCoordinates,
		getCoordinatesByRankByLogicalColour		= coordinatesByRankByLogicalColour,
		getNDefendersByCoordinatesByLogicalColour	= nDefendersByCoordinatesByLogicalColour,
		getNPiecesDifferenceByRank			= nPiecesDifferenceByRank,
		getNPawnsByFileByLogicalColour			= nPawnsByFileByLogicalColour,
		getNPieces					= nPieces,
		getPassedPawnCoordinatesByLogicalColour		= passedPawnCoordinatesByLogicalColour
	} = Control.DeepSeq.rnf (
		maybePieceByCoordinates,
		coordinatesByRankByLogicalColour,
		nDefendersByCoordinatesByLogicalColour,
		nPiecesDifferenceByRank,
		nPawnsByFileByLogicalColour,
		nPieces,
		passedPawnCoordinatesByLogicalColour
	 )

instance Read Board where
	readsPrec _	= Property.ForsythEdwards.readsFEN

instance Show Board where
	showsPrec _	= Property.ForsythEdwards.showsFEN

instance Property.ExtendedPositionDescription.ReadsEPD Board where
	readsEPD	= map (Control.Arrow.first fromMaybePieceByCoordinates) . Property.ExtendedPositionDescription.readsEPD

instance Property.ExtendedPositionDescription.ShowsEPD Board where
	showsEPD MkBoard { getMaybePieceByCoordinates = maybePieceByCoordinates }	= Property.ExtendedPositionDescription.showsEPD maybePieceByCoordinates

instance Property.ForsythEdwards.ReadsFEN Board where
	readsFEN	= Property.ExtendedPositionDescription.readsEPD

instance Property.ForsythEdwards.ShowsFEN Board

instance Data.Default.Default Board where
	def	= fromMaybePieceByCoordinates Data.Default.def {-MaybePieceByCoordinates-}

instance Property.Empty.Empty Board where
	empty	= fromMaybePieceByCoordinates Property.Empty.empty {-MaybePieceByCoordinates-}

instance Property.Reflectable.ReflectableOnX Board where
	reflectOnX MkBoard { getMaybePieceByCoordinates = maybePieceByCoordinates }	= fromMaybePieceByCoordinates $ Property.Reflectable.reflectOnX maybePieceByCoordinates

instance Property.Reflectable.ReflectableOnY Board where
	reflectOnY MkBoard { getMaybePieceByCoordinates = maybePieceByCoordinates }	= fromMaybePieceByCoordinates $ Property.Reflectable.reflectOnY maybePieceByCoordinates

instance StateProperty.Hashable.Hashable Board where
	listRandoms zobrist MkBoard { getCoordinatesByRankByLogicalColour = coordinatesByRankByLogicalColour }	= StateProperty.Hashable.listRandoms zobrist coordinatesByRankByLogicalColour	-- Forward.

instance StateProperty.Mutator.Mutator Board where
	defineCoordinates maybePiece coordinates MkBoard {
		getMaybePieceByCoordinates	= maybePieceByCoordinates
	} = fromMaybePieceByCoordinates $ StateProperty.Mutator.defineCoordinates maybePiece coordinates maybePieceByCoordinates

	movePiece move sourcePiece maybePromotionRank eitherPassingPawnsDestinationOrMaybeTakenRank MkBoard {
		getMaybePieceByCoordinates	= maybePieceByCoordinates
	} = fromMaybePieceByCoordinates $ StateProperty.Mutator.movePiece move sourcePiece maybePromotionRank eitherPassingPawnsDestinationOrMaybeTakenRank maybePieceByCoordinates

instance StateProperty.Seeker.Seeker Board where
	findProximateKnights MkBoard {
		getCoordinatesByRankByLogicalColour	= coordinatesByRankByLogicalColour
	} = StateProperty.Seeker.findProximateKnights coordinatesByRankByLogicalColour	-- Forward the request.

	findPieces predicate MkBoard {
		getCoordinatesByRankByLogicalColour	= coordinatesByRankByLogicalColour
	} = StateProperty.Seeker.findPieces predicate coordinatesByRankByLogicalColour	-- Forward the request.

	countPawnsByFileByLogicalColour	MkBoard {
		getCoordinatesByRankByLogicalColour	= coordinatesByRankByLogicalColour
	} = StateProperty.Seeker.countPawnsByFileByLogicalColour coordinatesByRankByLogicalColour	-- Forward the request.

instance Property.SelfValidating.SelfValidating Board where
	findInvalidity	= Property.SelfValidating.findInvalidity . (getMaybePieceByCoordinates &&& getCoordinatesByRankByLogicalColour)

-- | Constructor.
fromMaybePieceByCoordinates :: State.MaybePieceByCoordinates.MaybePieceByCoordinates -> Board
fromMaybePieceByCoordinates maybePieceByCoordinates	= board where
	board@MkBoard { getCoordinatesByRankByLogicalColour = coordinatesByRankByLogicalColour }	= MkBoard {
		getMaybePieceByCoordinates			= maybePieceByCoordinates,
		getCoordinatesByRankByLogicalColour		= StateProperty.View.translate maybePieceByCoordinates,									-- Infer.
		getNDefendersByCoordinatesByLogicalColour	= countDefendersByCoordinatesByLogicalColour board,									-- Infer.
		getNPiecesDifferenceByRank			= StateProperty.Censor.countPieceDifferenceByRank coordinatesByRankByLogicalColour,					-- Infer.
		getNPawnsByFileByLogicalColour			= StateProperty.Seeker.countPawnsByFileByLogicalColour coordinatesByRankByLogicalColour,				-- Infer.
		getNPieces					= StateProperty.Censor.countPieces coordinatesByRankByLogicalColour,							-- Infer.
		getPassedPawnCoordinatesByLogicalColour		= State.CoordinatesByRankByLogicalColour.findPassedPawnCoordinatesByLogicalColour coordinatesByRankByLogicalColour	-- Infer.
	}

{- |
	* Moves the referenced /piece/.

	* CAVEAT: no validation is performed.

	* CAVEAT: /castling/ must be implemented by making two calls.
-}
movePiece
	:: Component.Move.Move			-- ^ N.B.: illegal moves are acceptable.
	-> Maybe Attribute.MoveType.MoveType	-- ^ N.B.: this may not be available to the caller, for example during the illegal moves required for rollback.
	-> Transformation
movePiece move maybeMoveType board@MkBoard {
	getMaybePieceByCoordinates			= maybePieceByCoordinates,
	getCoordinatesByRankByLogicalColour		= coordinatesByRankByLogicalColour,
	getNDefendersByCoordinatesByLogicalColour	= nDefendersByCoordinatesByLogicalColour,
	getNPiecesDifferenceByRank			= nPiecesDifferenceByRank,
	getNPieces					= nPieces
}
	| Just sourcePiece <- State.MaybePieceByCoordinates.dereference maybePieceByCoordinates source	= let
		oppositePiece				= Property.Opposable.getOpposite sourcePiece
		(logicalColour, opponentsLogicalColour)	= ($ sourcePiece) &&& ($ oppositePiece) $ Component.Piece.getLogicalColour

		moveType :: Attribute.MoveType.MoveType
		moveType -- CAVEAT: one can't call 'State.MaybePieceByCoordinates.inferMoveType', since that performs some move-validation, & therefore exceeds the remit of this module.
			| Just explicitMoveType	<- maybeMoveType					= explicitMoveType
			| State.MaybePieceByCoordinates.isEnPassantMove maybePieceByCoordinates move	= Attribute.MoveType.enPassant	-- N.B.: if this move is valid, then one's opponent must have just double-advanced an adjacent Pawn.
			| otherwise									= Attribute.MoveType.mkNormalMoveType (
				Component.Piece.getRank <$> State.MaybePieceByCoordinates.dereference maybePieceByCoordinates destination
			) $ if Component.Piece.isPawnPromotion sourcePiece destination
				then Just Attribute.Rank.defaultPromotionRank
				else Nothing

-- Derive the required values from moveType.
		(maybePromotionRank, maybeExplicitlyTakenRank)	= Attribute.Rank.getMaybePromotionRank &&& Attribute.MoveType.getMaybeExplicitlyTakenRank $ moveType	-- Deconstruct.
		destinationPiece				= Data.Maybe.maybe id Component.Piece.promote maybePromotionRank sourcePiece
		wasPawnTakenExplicitly				= maybeExplicitlyTakenRank == Just Attribute.Rank.Pawn

		eitherPassingPawnsDestinationOrMaybeTakenRank
			| Attribute.MoveType.isEnPassant moveType	= Left $ Cartesian.Coordinates.retreat logicalColour destination
			| otherwise					= Right maybeExplicitlyTakenRank

		eitherPassingPawnsDestinationOrMaybeTakenPiece	= fmap (Component.Piece.mkPiece opponentsLogicalColour) <$> eitherPassingPawnsDestinationOrMaybeTakenRank

		movePiece' :: StateProperty.Mutator.Mutator mutator => mutator -> mutator
		movePiece'	= StateProperty.Mutator.movePiece move sourcePiece maybePromotionRank eitherPassingPawnsDestinationOrMaybeTakenRank

		board'@MkBoard { getMaybePieceByCoordinates = maybePieceByCoordinates' }	= MkBoard {
			getMaybePieceByCoordinates			= movePiece' maybePieceByCoordinates,
			getCoordinatesByRankByLogicalColour		= movePiece' coordinatesByRankByLogicalColour,
			getNDefendersByCoordinatesByLogicalColour	= (
				\(nBlackDefendersByCoordinates, nWhiteDefendersByCoordinates)	-> Colour.LogicalColour.listArrayByLogicalColour [nBlackDefendersByCoordinates, nWhiteDefendersByCoordinates]
			) . foldr (
				\(affectedCoordinates, affectedPiece) -> if Component.Piece.isKing affectedPiece
					then id	-- N.B.: defence of the King is irrelevant, since one can't get to a position where it can be taken.
					else let
						logicalColour'	= Component.Piece.getLogicalColour affectedPiece
					in (
						if Colour.LogicalColour.isBlack logicalColour'
							then Control.Arrow.first
							else Control.Arrow.second
					) . Map.insert affectedCoordinates {-overwrite-} . fromIntegral . length $ findAttackersOf board' (
						Property.Opposable.getOpposite logicalColour'	-- Investigate an attack on the affected coordinates by the affected piece's own logical colour, i.e. defence.
					) affectedCoordinates
			) (
				(! Colour.LogicalColour.Black) &&& (! Colour.LogicalColour.White) $ nDefendersByCoordinatesByLogicalColour // (
					let
						nDefendersByCoordinates	= nDefendersByCoordinatesByLogicalColour ! opponentsLogicalColour
					in (
						\passingPawnsDestination -> (:) (
							opponentsLogicalColour,
							Map.delete passingPawnsDestination nDefendersByCoordinates	-- This Pawn has been taken.
						)
					) ||| (
						\maybeExplicitlyTakenRank' -> if Data.Maybe.isJust maybeExplicitlyTakenRank'
							then (:) (
								opponentsLogicalColour,
								Map.delete destination nDefendersByCoordinates	-- This piece has been taken.
							)
							else id
					) $ eitherPassingPawnsDestinationOrMaybeTakenRank
				 ) [
					(
						logicalColour,
						Map.delete source $ nDefendersByCoordinatesByLogicalColour ! logicalColour	-- This piece has been moved.
					) -- Pair.
				 ] -- Singleton.
			) . Data.List.nubBy (
				ToolShed.Data.List.equalityBy fst {-coordinates-}
			) $ [
				(affectedCoordinates, affectedPiece) |
					(knightsCoordinates, knight)	<- (source, sourcePiece) : (,) destination `map` (destinationPiece : (const [] ||| Data.Maybe.maybeToList) eitherPassingPawnsDestinationOrMaybeTakenPiece),
					Component.Piece.isKnight knight,
					Just affectedCoordinates	<- map (`Cartesian.Vector.maybeTranslate` knightsCoordinates) Cartesian.Vector.attackVectorsForKnight,
					affectedPiece			<- Data.Maybe.maybeToList $ State.MaybePieceByCoordinates.dereference maybePieceByCoordinates' affectedCoordinates,
					Component.Piece.isFriend knight affectedPiece
			] {-list-comprehension-} ++ [
				(blockingCoordinates, blockingPiece) |
					passingPawnsDestination			<- return {-to List-monad-} ||| const [] $ eitherPassingPawnsDestinationOrMaybeTakenRank,
					(direction, antiParallelDirection)	<- Direction.Direction.opposites,
					(blockingCoordinates, blockingPiece)	<- case ($ direction) &&& ($ antiParallelDirection) $ State.MaybePieceByCoordinates.findBlockingPiece maybePieceByCoordinates' passingPawnsDestination of
						(Just cp, Just cp')	-> [
							cp |
								let isDefendedBy from	= uncurry (&&) . uncurry (&&&) (Component.Piece.canAttackAlong from *** Component.Piece.isFriend $ cp),
								isDefendedBy passingPawnsDestination oppositePiece || uncurry isDefendedBy cp'
						 ] {-list-comprehension-} ++ [
							cp' |
								let isDefendedBy from	= uncurry (&&) . uncurry (&&&) (Component.Piece.canAttackAlong from *** Component.Piece.isFriend $ cp'),
								isDefendedBy passingPawnsDestination oppositePiece || uncurry isDefendedBy cp
						 ] -- List-comprehension.
						(Just cp, _)		-> [
							cp |
								uncurry (&&) $ uncurry (&&&) (Component.Piece.canAttackAlong passingPawnsDestination *** Component.Piece.isFriend $ cp) oppositePiece
						 ] -- List-comprehension.
						(_, Just cp')		-> [
							cp' |
								uncurry (&&) $ uncurry (&&&) (Component.Piece.canAttackAlong passingPawnsDestination *** Component.Piece.isFriend $ cp') oppositePiece
						 ] -- List-comprehension.
						_			-> []
			] {-list-comprehension-} ++ (destination, destinationPiece) : [
				(blockingCoordinates, blockingPiece) |
					let maybeExplicitlyTakenPiece	= const Nothing ||| id $ eitherPassingPawnsDestinationOrMaybeTakenPiece,
					(direction, antiParallelDirection)	<- Direction.Direction.opposites,
					(coordinates, piece)			<- [(source, sourcePiece), (destination, destinationPiece)],
					(blockingCoordinates, blockingPiece)	<- case ($ direction) &&& ($ antiParallelDirection) $ State.MaybePieceByCoordinates.findBlockingPiece maybePieceByCoordinates' coordinates of
						(Just cp, Just cp')	-> [
							cp |
								let isDefendedBy from	= uncurry (&&) . uncurry (&&&) (Component.Piece.canAttackAlong from *** Component.Piece.isFriend $ cp),
								isDefendedBy coordinates piece || coordinates == destination && Data.Maybe.maybe False (isDefendedBy destination) maybeExplicitlyTakenPiece || uncurry isDefendedBy cp'
						 ] {-list-comprehension-} ++ [
							cp' |
								let isDefendedBy from	= uncurry (&&) . uncurry (&&&) (Component.Piece.canAttackAlong from *** Component.Piece.isFriend $ cp'),
								isDefendedBy coordinates piece || coordinates == destination && Data.Maybe.maybe False (isDefendedBy destination) maybeExplicitlyTakenPiece || uncurry isDefendedBy cp
						 ] -- List-comprehension.
						(Just cp, _)		-> [
							cp |
								let isDefendedBy	= uncurry (&&) . uncurry (&&&) (Component.Piece.canAttackAlong coordinates *** Component.Piece.isFriend $ cp),
								isDefendedBy piece || coordinates == destination && Data.Maybe.maybe False isDefendedBy maybeExplicitlyTakenPiece
						 ] -- List-comprehension.
						(_, Just cp')		-> [
							cp' |
								let isDefendedBy	= uncurry (&&) . uncurry (&&&) (Component.Piece.canAttackAlong coordinates *** Component.Piece.isFriend $ cp'),
								isDefendedBy piece || coordinates == destination && Data.Maybe.maybe False isDefendedBy maybeExplicitlyTakenPiece
						 ] -- List-comprehension.
						_			-> []
			], -- List-comprehension. Define any pieces whose defence may be affected by the move.
			getNPiecesDifferenceByRank	= Data.Array.IArray.accum (
				if Colour.LogicalColour.isBlack logicalColour
					then (-)	-- Since White pieces are arbitrarily counted as positive, negate the adjustment if the current player is Black.
					else (+)
			) nPiecesDifferenceByRank $ if Attribute.MoveType.isEnPassant moveType
				then [(Attribute.Rank.Pawn, 1)]	-- Increment relative number of Pawns.
				else Data.Maybe.maybe id (
					(:) . flip (,) 1	-- Increment.
				) maybeExplicitlyTakenRank $ Data.Maybe.maybe [] (
					\promotionRank -> [
						(
							promotionRank,
							1	-- Increment.
						), (
							Attribute.Rank.Pawn,
							negate 1	-- Decrement relative number of Pawns.
						)
					]
				) maybePromotionRank,
			getNPawnsByFileByLogicalColour		= if Component.Piece.isPawn sourcePiece && not (Attribute.MoveType.isQuiet moveType) || wasPawnTakenExplicitly
				then StateProperty.Seeker.countPawnsByFileByLogicalColour coordinatesByRankByLogicalColour'	-- Recalculate.
				else getNPawnsByFileByLogicalColour board,
			getNPieces				= Attribute.MoveType.nPiecesMutator moveType nPieces,
			getPassedPawnCoordinatesByLogicalColour	= if Component.Piece.isPawn sourcePiece || wasPawnTakenExplicitly
				then State.CoordinatesByRankByLogicalColour.findPassedPawnCoordinatesByLogicalColour coordinatesByRankByLogicalColour'	-- Recalculate.
				else getPassedPawnCoordinatesByLogicalColour board
		}

		coordinatesByRankByLogicalColour'	= getCoordinatesByRankByLogicalColour board'
	in board'
	| otherwise	= Control.Exception.throw . Data.Exception.mkSearchFailure . showString "BishBosh.State.Board.movePiece:\tno piece exists at " . shows source . showString "; " $ shows board "."
	where
		(source, destination)	= Component.Move.getSource &&& Component.Move.getDestination $ move	-- Deconstruct.

-- | Calculate the total value of the /coordinates/ occupied by the /piece/s of either side, at a stage in the game's life-span defined by the total number of pieces remaining.
sumPieceSquareValueByLogicalColour
	:: Component.PieceSquareByCoordinatesByRank.PieceSquareByCoordinatesByRank
	-> Board
	->
#if defined(USE_UNBOXED_ARRAYS) && !defined(USE_PRECISION)
	Colour.LogicalColour.UArrayByLogicalColour
#else
	Colour.LogicalColour.ArrayByLogicalColour
#endif
		Type.Mass.Base	-- ^ Sum of PieceSquareValues.
sumPieceSquareValueByLogicalColour pieceSquareByCoordinatesByRank MkBoard {
	getCoordinatesByRankByLogicalColour	= coordinatesByRankByLogicalColour,
	getNPieces				= nPieces
} = Colour.LogicalColour.listArrayByLogicalColour $ Component.Accountant.sumPieceSquareValueByLogicalColour pieceSquareByCoordinatesByRank coordinatesByRankByLogicalColour nPieces

{- |
	* Lists the source-/coordinates/ from which the referenced destination can be attacked.

	* N.B.: the algorithm is independent of whose turn it actually is.

	* CAVEAT: checks neither the /logical colour/ of the defender, nor that their /piece/ even exists.

	* CAVEAT: may return the /coordinates/ of a diagonally adjacent @Pawn@; which would be an illegal move if there's not actually any /piece/ at the referenced destination.

	* CAVEAT: can't detect an en-passant attack, since this depends both on whether the previous move was a double advance & that the defender is a @Pawn@.
-}
findAttackersOf
	:: Board
	-> Colour.LogicalColour.LogicalColour				-- ^ The defender's /logical colour/.
	-> Cartesian.Coordinates.Coordinates				-- ^ The defender's location.
	-> [(Cartesian.Coordinates.Coordinates, Attribute.Rank.Rank)]	-- ^ The locations from which the specified square can be attacked by the opposite /logical colour/.
findAttackersOf board@MkBoard { getMaybePieceByCoordinates = maybePieceByCoordinates } destinationLogicalColour destination	= [
	(coordinates, Attribute.Rank.Knight) |
		coordinates	<- StateProperty.Seeker.findProximateKnights board (Property.Opposable.getOpposite destinationLogicalColour) destination
 ] {-list-comprehension-} ++ Data.Maybe.mapMaybe (
	State.MaybePieceByCoordinates.findAttackerInDirection maybePieceByCoordinates destinationLogicalColour destination
 ) Property.FixedMembership.members

{- |
	* Lists the source-/coordinates/ from which the referenced destination can be attacked by the specified type of /piece/.

	* N.B.: similar to 'findAttackersOf', but can be more efficient since the attacking /piece/ is known.

	* CAVEAT: can't detect an en-passant attack, since this depends both on whether the previous move was a double advance & that the defender is a @Pawn@.
-}
findAttacksBy
	:: Board
	-> Component.Piece.Piece		-- ^ The type of attacker.
	-> Cartesian.Coordinates.Coordinates	-- ^ The defender's location.
	-> [Cartesian.Coordinates.Coordinates]	-- ^ The sources from which the specified attacker could strike.
findAttacksBy board piece destination
	| rank == Attribute.Rank.Knight	= StateProperty.Seeker.findProximateKnights board logicalColour destination
	| otherwise			= filter (
		\source -> source /= destination && Component.Piece.canAttackAlong source destination piece && State.MaybePieceByCoordinates.isClear (getMaybePieceByCoordinates board) source destination
	) $ State.CoordinatesByRankByLogicalColour.dereference (getCoordinatesByRankByLogicalColour board) logicalColour rank
	where
		(logicalColour, rank)	= Component.Piece.getLogicalColour &&& Component.Piece.getRank $ piece

{- |
	* Whether the @King@ of the specified /logical colour/ is currently /checked/.

	* N.B.: independent of whose turn it actually is.

	* CAVEAT: assumes there's exactly one @King@ of the specified /logical colour/.
-}
isKingChecked
	:: Board
	-> Colour.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the @King@ in question.
	-> Bool
isKingChecked board@MkBoard { getCoordinatesByRankByLogicalColour = coordinatesByRankByLogicalColour }	= not . null . uncurry ($) . (
	findAttackersOf board &&& State.CoordinatesByRankByLogicalColour.getKingsCoordinates coordinatesByRankByLogicalColour
 )

{- |
	* Whether one's own @King@ has become exposed in the proposed /board/.

	* CAVEAT: assumes that one's @King@ wasn't already checked.

	* CAVEAT: this function is a performance-hotspot.
-}
exposesKing
	:: Board				-- ^ The original /board/, i.e. prior to the /move/.
	-> Colour.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the player proposing to move.
	-> Component.Move.Move			-- ^ The /move/.
	-> Bool
exposesKing board@MkBoard { getCoordinatesByRankByLogicalColour = coordinatesByRankByLogicalColour } logicalColour move
	| source == kingsCoordinates	= not . null . findAttackersOf board logicalColour $ Component.Move.getDestination move	-- CAVEAT: expensive, since all directions from the King may have to be explored.
	| Just directionFromKing	<- Cartesian.Vector.toMaybeDirection $ Cartesian.Vector.measureDistance kingsCoordinates source	-- Confirm that one's own King is on a straight line with the start of the move.
	, let maybePieceByCoordinates	= getMaybePieceByCoordinates board
	, State.MaybePieceByCoordinates.isClear maybePieceByCoordinates kingsCoordinates source	-- Confirm that the straight line from one's own King to the start of the move, is clear.
	, Data.Maybe.maybe True {-Knight's move-} (
		not . Direction.Direction.areAligned directionFromKing	-- The blocking piece has revealed any attacker.
	) . Cartesian.Vector.toMaybeDirection $ Component.Move.measureDistance move
	, Just (_, attackersRank)	<- State.MaybePieceByCoordinates.findAttackerInDirection maybePieceByCoordinates logicalColour source directionFromKing	-- Confirm the existence of an obscured attacker.
	= attackersRank `notElem` Attribute.Rank.plodders	-- Confirm sufficient range to bridge the vacated space.
	| otherwise	= False
	where
		source			= Component.Move.getSource move
		kingsCoordinates	= State.CoordinatesByRankByLogicalColour.getKingsCoordinates coordinatesByRankByLogicalColour logicalColour

-- | Count the number of defenders of each /piece/ on the /board/.
countDefendersByCoordinatesByLogicalColour :: Board -> NDefendersByCoordinatesByLogicalColour
countDefendersByCoordinatesByLogicalColour board@MkBoard { getCoordinatesByRankByLogicalColour = coordinatesByRankByLogicalColour }	= Colour.LogicalColour.listArrayByLogicalColour [
	Map.fromList [
		(
			coordinates,
			fromIntegral . length $ findAttackersOf board (
				Property.Opposable.getOpposite logicalColour	-- Investigate an attack on these coordinates by one's own logical colour.
			) coordinates
		) |
			rank		<- Attribute.Rank.expendable,	-- CAVEAT: there's no point defending one's own King.
			coordinates	<- State.CoordinatesByRankByLogicalColour.dereference coordinatesByRankByLogicalColour logicalColour rank
	] {-list-comprehension-} | logicalColour <- Property.FixedMembership.members
 ] -- List-comprehension.

-- | Collapses 'NDefendersByCoordinatesByLogicalColour' into the total number of defenders on either side.
summariseNDefendersByLogicalColour :: Board -> Colour.LogicalColour.ArrayByLogicalColour Type.Count.NPieces
summariseNDefendersByLogicalColour MkBoard { getNDefendersByCoordinatesByLogicalColour = nDefendersByCoordinatesByLogicalColour }	= Data.Array.IArray.amap (
	Data.Foldable.foldl' (+) 0	-- CAVEAT: 'Data.Foldable.sum' is too slow.
 ) nDefendersByCoordinatesByLogicalColour

