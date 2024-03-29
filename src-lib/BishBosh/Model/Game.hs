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

	* This module augments "State.Board" with the history of the game.

	* It therefore understands not only the current state of the /board/, but also; whose turn it is, whether /Castling/ has occured, which @Pawn@s have been /promoted/, when /piece/s were taken.

	* Moves made in this domain conform to the rules of chess, c.f. those made in "State.Board".
-}

module BishBosh.Model.Game(
-- * Types
-- ** Type-synonyms
--	InstancesByPosition,
--	AvailableQualifiedMoves,
--	AvailableQualifiedMovesByLogicalColour,
	Transformation,
-- ** Data-types
	Game(
--		MkGame,
		getNextLogicalColour,
		getCastleableRooksByLogicalColour,
		getBoard,
		getTurnsByLogicalColour,
		getMaybeChecked,
		getInstancesByPosition,
		getAvailableQualifiedMovesByLogicalColour,
		getMaybeTerminationReason
	),
-- * Functions
--	inferMaybeTerminationReason,
	countPliesAvailableTo,
	rollBack,
--	listMaybePromotionRanks,
--	listQualifiedMovesAvailableTo,
	sortAvailableQualifiedMoves,
	findQualifiedMovesAvailableTo,
	findQualifiedMovesAvailableToNextPlayer,
	listTurns,
	listTurnsChronologically,
	maybeLastTurn,
--	findAvailableCastlingMoves,
	validateQualifiedMove,
	validateEitherQualifiedMove,
	updateIncrementalPositionHash,
-- ** Constructors
	mkPosition,
--	mkInstancesByPosition,
	mkGame,
	fromBoard,
	mkAvailableQualifiedMovesFor,
-- ** Mutators
	takeTurn,
	applyQualifiedMove,
	applyEitherQualifiedMove,
	applyEitherQualifiedMoves,
	updateTerminationReasonWith,
--	resignationBy,
	resign,
--	agreeToADraw,
-- ** Predicates
	isValidQualifiedMove,
	isValidEitherQualifiedMove,
	isTerminated,
	cantConverge,
	(=~),
	(/~)
) where

import			Control.Arrow((&&&), (***), (|||))
import qualified	BishBosh.Attribute.MoveType			as Attribute.MoveType
import qualified	BishBosh.Attribute.Rank				as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa			as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates			as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Vector			as Cartesian.Vector
import qualified	BishBosh.Colour.LogicalColour			as Colour.LogicalColour
import qualified	BishBosh.Component.CastlingMove			as Component.CastlingMove
import qualified	BishBosh.Component.EitherQualifiedMove		as Component.EitherQualifiedMove
import qualified	BishBosh.Component.Move				as Component.Move
import qualified	BishBosh.Component.Piece			as Component.Piece
import qualified	BishBosh.Component.QualifiedMove		as Component.QualifiedMove
import qualified	BishBosh.Component.Turn				as Component.Turn
import qualified	BishBosh.Component.Zobrist			as Component.Zobrist
import qualified	BishBosh.Data.Exception				as Data.Exception
import qualified	BishBosh.Notation.MoveNotation			as Notation.MoveNotation
import qualified	BishBosh.Notation.Notation			as Notation.Notation
import qualified	BishBosh.Notation.PureCoordinate		as Notation.PureCoordinate
import qualified	BishBosh.Property.Empty				as Property.Empty
import qualified	BishBosh.Property.ExtendedPositionDescription	as Property.ExtendedPositionDescription
import qualified	BishBosh.Property.FixedMembership		as Property.FixedMembership
import qualified	BishBosh.Property.ForsythEdwards		as Property.ForsythEdwards
import qualified	BishBosh.Property.Null				as Property.Null
import qualified	BishBosh.Property.Opposable			as Property.Opposable
import qualified	BishBosh.Property.Orientated			as Property.Orientated
import qualified	BishBosh.Property.Reflectable			as Property.Reflectable
import qualified	BishBosh.Rule.DrawReason			as Rule.DrawReason
import qualified	BishBosh.Rule.GameTerminationReason		as Rule.GameTerminationReason
import qualified	BishBosh.Rule.Result				as Rule.Result
import qualified	BishBosh.State.Board				as State.Board
import qualified	BishBosh.State.CastleableRooksByLogicalColour	as State.CastleableRooksByLogicalColour
import qualified	BishBosh.State.CoordinatesByRankByLogicalColour	as State.CoordinatesByRankByLogicalColour
import qualified	BishBosh.State.EnPassantAbscissa		as State.EnPassantAbscissa
import qualified	BishBosh.State.InstancesByPosition		as State.InstancesByPosition
import qualified	BishBosh.State.MaybePieceByCoordinates		as State.MaybePieceByCoordinates
import qualified	BishBosh.State.Position				as State.Position
import qualified	BishBosh.StateProperty.Censor			as StateProperty.Censor
import qualified	BishBosh.StateProperty.Hashable			as StateProperty.Hashable
import qualified	BishBosh.StateProperty.Mutator			as StateProperty.Mutator
import qualified	BishBosh.StateProperty.Seeker			as StateProperty.Seeker
import qualified	BishBosh.State.TurnsByLogicalColour		as State.TurnsByLogicalColour
import qualified	BishBosh.Text.ShowList				as Text.ShowList
import qualified	BishBosh.Type.Count				as Type.Count
import qualified	BishBosh.Type.Crypto				as Type.Crypto
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Bits
import qualified	Data.Default
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.List.Extra
import qualified	Data.Map					as Map
import qualified	Data.Maybe
import qualified	Data.Ord
import qualified	ToolShed.Data.List

infix 4 =~, /~	-- Same as (==) & (/=).

{- |
	* Focus the polymorphic key-type used by 'State.InstancesByPosition.InstancesByPosition'.

	* N.B. ideally a hash of the position would be used as the key,
	but to achieve that the same random numbers from which it is constructed, would have to be passed to 'takeTurn' throughout the life-time of the 'Game'.
	Regrettably, class-instances can only use @ Data.Default.def :: Zobrist @, which must then be assumed by the users of all methods.
	Building 'Component.Zobrist.Zobrist' into 'Game' would break the instance of 'Eq'.
	Building a hash-constructor into 'Game' would break the instance of @ (Eq, Read, Show) @.
-}
type InstancesByPosition	= State.InstancesByPosition.InstancesByPosition State.Position.Position

-- | The /move/s available to one player, indexed by the source-/coordinates/ of the /move/.
type AvailableQualifiedMoves	= (
	Map.Map Cartesian.Coordinates.Coordinates	-- Source.
 ) [
	(
		Cartesian.Coordinates.Coordinates,	-- Destination.
		Attribute.MoveType.MoveType
	)
 ]

-- | Sort the lists of destinations to faciliate testing for equality.
sortAvailableQualifiedMoves :: AvailableQualifiedMoves -> AvailableQualifiedMoves
sortAvailableQualifiedMoves	= Map.map . Data.List.sortBy $ Data.Ord.comparing fst {-destination-}

-- | The /move/s available to both players.
type AvailableQualifiedMovesByLogicalColour	= Map.Map Colour.LogicalColour.LogicalColour AvailableQualifiedMoves

{- |
	* The first three fields represent the state of the /game/.

	* These are augmented by the /game/'s history, i.e. the sequence of /move/s.

	* For efficiency the list of available /move/s is stored.
-}
data Game	= MkGame {
	getNextLogicalColour				:: Colour.LogicalColour.LogicalColour,					-- ^ N.B.: can be derived from 'getTurnsByLogicalColour', unless 'Property.Reflectable.reflectOnX' has been called.
	getCastleableRooksByLogicalColour		:: State.CastleableRooksByLogicalColour.CastleableRooksByLogicalColour,	-- ^ Those @Rook@s which can still participate in castling.
	getBoard					:: State.Board.Board,							-- ^ The current state of the /board/.
	getTurnsByLogicalColour				:: State.CastleableRooksByLogicalColour.TurnsByLogicalColour,		-- ^ Successive /move/s & any /piece/ taken, recorded by player.
	getMaybeChecked					:: Maybe Colour.LogicalColour.LogicalColour,				-- ^ The player (if any), whose currently /checked/; which will typically be 'getNextLogicalColour', but 'listQualifiedMovesAvailableTo' can be called for either player.
	getInstancesByPosition				:: InstancesByPosition,							-- ^ The number of instances of various positions since the last unrepeatable move.
	getAvailableQualifiedMovesByLogicalColour	:: AvailableQualifiedMovesByLogicalColour,				-- ^ The /move/s available to each player. Since this is merely required for efficiency, it needn't have an entry for both players; & typically doesn't when checked, since radical pruning would otherwise be required. CAVEAT: doesn't account for game-termination.
	getMaybeTerminationReason			:: Maybe Rule.GameTerminationReason.GameTerminationReason		-- ^ The reason (where appropriate) why the game was terminated.
}

instance Eq Game where
	MkGame {
		getNextLogicalColour				= nextLogicalColour,
		getCastleableRooksByLogicalColour		= castleableRooksByLogicalColour,
		getBoard					= board,
		getTurnsByLogicalColour				= turnsByLogicalColour,
		getMaybeChecked					= maybeChecked,
		getInstancesByPosition				= instancesByPosition,
		getAvailableQualifiedMovesByLogicalColour	= availableQualifiedMovesByLogicalColour,
		getMaybeTerminationReason			= maybeTerminationReason
	} == MkGame {
		getNextLogicalColour				= nextLogicalColour',
		getCastleableRooksByLogicalColour		= castleableRooksByLogicalColour',
		getBoard					= board',
		getTurnsByLogicalColour				= turnsByLogicalColour',
		getMaybeChecked					= maybeChecked',
		getInstancesByPosition				= instancesByPosition',
		getAvailableQualifiedMovesByLogicalColour	= availableQualifiedMovesByLogicalColour',
		getMaybeTerminationReason			= maybeTerminationReason'
	} = (
		nextLogicalColour,
		castleableRooksByLogicalColour,
		board,
		turnsByLogicalColour,
		maybeChecked,
		instancesByPosition,
		Map.map sortAvailableQualifiedMoves availableQualifiedMovesByLogicalColour,
		maybeTerminationReason
	 ) == (
		nextLogicalColour',
		castleableRooksByLogicalColour',
		board',
		turnsByLogicalColour',
		maybeChecked',
		instancesByPosition',
		Map.map sortAvailableQualifiedMoves availableQualifiedMovesByLogicalColour',
		maybeTerminationReason'
	 )

instance Control.DeepSeq.NFData Game where
	rnf MkGame {
		getNextLogicalColour				= nextLogicalColour,
		getCastleableRooksByLogicalColour		= castleableRooksByLogicalColour,
		getBoard					= board,
		getTurnsByLogicalColour				= turnsByLogicalColour,
		getMaybeChecked					= maybeChecked,
		getInstancesByPosition				= instancesByPosition,
		getAvailableQualifiedMovesByLogicalColour	= availableQualifiedMovesByLogicalColour,
		getMaybeTerminationReason			= maybeTerminationReason
	} = Control.DeepSeq.rnf (
		nextLogicalColour,
		castleableRooksByLogicalColour,
		board,
		turnsByLogicalColour,
		maybeChecked,
		instancesByPosition,
		availableQualifiedMovesByLogicalColour,
		maybeTerminationReason
	 ) -- Represent as a tuple.

instance Show Game where
	showsPrec precedence MkGame {
		getBoard			= board,
		getTurnsByLogicalColour		= turnsByLogicalColour,
		getMaybeTerminationReason	= maybeTerminationReason
	} = showsPrec precedence (
		board,
		turnsByLogicalColour,
		maybeTerminationReason
	 ) -- Represent as a tuple those fields which can't be inferred.

instance Read Game where
	readsPrec precedence	= map (
		Control.Arrow.first $ \(
			board,
			turnsByLogicalColour,
			maybeTerminationReason
		) {-tuple-} -> let
			game = (
				uncurry mkGame (
					State.TurnsByLogicalColour.inferNextLogicalColour &&& State.CastleableRooksByLogicalColour.fromTurnsByLogicalColour $ turnsByLogicalColour
				) board turnsByLogicalColour
			 ) {
				getInstancesByPosition		= mkInstancesByPosition game,
				getMaybeTerminationReason	= maybeTerminationReason
			}
		in game
	 ) . readsPrec precedence

instance Data.Default.Default Game where
	def = (
		mkGame Colour.LogicalColour.White Data.Default.def {-castleableRooksByLogicalColour-} Data.Default.def {-board-} Data.Default.def {-turnsByLogicalColour-}
	 ) {
		getMaybeChecked					= Nothing,
		getAvailableQualifiedMovesByLogicalColour	= Map.fromAscList $ map (
			id &&& mkAvailableQualifiedMovesFor Data.Default.def {-game-}
		) Property.FixedMembership.members
	}

instance Property.ExtendedPositionDescription.ReadsEPD Game where
	readsEPD s	= [
		(
			mkGame nextLogicalColour castleableRooksByLogicalColour board turnsByLogicalColour,
			s4
		) |
			(board, s1)				<- Property.ExtendedPositionDescription.readsEPD s,
			(nextLogicalColour, s2)			<- Property.ExtendedPositionDescription.readsEPD s1,
			(castleableRooksByLogicalColour, s3)	<- Property.ExtendedPositionDescription.readsEPD s2,
			(turnsByLogicalColour, s4)		<- case Data.List.Extra.trimStart s3 of
				'-' : s4'	-> [(Property.Empty.empty {-TurnsByLogicalColour-}, s4')]
				s3'		-> Control.Arrow.first (
					\enPassantDestination -> let
						opponentsLogicalColour	= Property.Opposable.getOpposite nextLogicalColour
					in State.TurnsByLogicalColour.fromAssocs [
						(
							nextLogicalColour,
							[]
						), (
							opponentsLogicalColour,
							[
								Component.Turn.mkTurn (
									Component.QualifiedMove.mkQualifiedMove (
										uncurry Component.Move.mkMove $ (
											uncurry Cartesian.Coordinates.retreat &&& uncurry Cartesian.Coordinates.advance
										) (opponentsLogicalColour, enPassantDestination)	-- Reconstruct the recent Pawn double-advance.
									) Data.Default.def {-move-type-}
								) Attribute.Rank.Pawn
							] -- Singleton.
						) -- Pair.
					]
				 ) `map` Notation.Notation.readsCoordinates Notation.PureCoordinate.notation s3' -- En-passant destination.
	 ] -- List-comprehension.

instance Property.ExtendedPositionDescription.ShowsEPD Game where
	showsEPD game@MkGame {
		getNextLogicalColour			= nextLogicalColour,
		getCastleableRooksByLogicalColour	= castleableRooksByLogicalColour,
		getBoard				= board
	 } = Text.ShowList.showsDelimitedList Property.ExtendedPositionDescription.showsSeparator id id [
		Property.ExtendedPositionDescription.showsEPD board,				-- 1. Placement of pieces.
		Property.ExtendedPositionDescription.showsEPD nextLogicalColour,		-- 2. Active colour.
		Property.ExtendedPositionDescription.showsEPD castleableRooksByLogicalColour,	-- 3. Castling availability.
		Data.Maybe.maybe Property.ExtendedPositionDescription.showsNullField (
			\turn -> if Component.Turn.isPawnDoubleAdvance turn $ Property.Opposable.getOpposite nextLogicalColour
				then Notation.MoveNotation.showsNotation Data.Default.def {-Smith is the same as the required Algebraic notation in this limited role-} . Cartesian.Coordinates.advance nextLogicalColour . Component.Move.getDestination . Component.QualifiedMove.getMove $ Component.Turn.getQualifiedMove turn
				else Property.ExtendedPositionDescription.showsNullField
		) $ maybeLastTurn game	-- 4. En-passant target square. CAVEAT: in contrast to X-EPD, this is required even when there's no opposing Pawn in a suitable position to take en-passant.
	 ]

-- CAVEAT: some information is lost during 'showsFEN', which can't subsequently be recovered by 'readsFEN'.
instance Property.ForsythEdwards.ReadsFEN Game where
	readsFEN s	= [
		(game, s3) |
			(game, s1)		<- Property.ExtendedPositionDescription.readsEPD s,
			(_halfMoveClock, s2)	<- reads s1 :: [(Int, String)],
			(_fullMoveCounter, s3)	<- reads s2 :: [(Int, String)]
	 ] -- List-comprehension.

instance Property.ForsythEdwards.ShowsFEN Game where
	showsFEN game@MkGame {
		getTurnsByLogicalColour	= turnsByLogicalColour,
		getInstancesByPosition	= instancesByPosition
	 } = Text.ShowList.showsDelimitedList Property.ExtendedPositionDescription.showsSeparator id id [
		Property.ExtendedPositionDescription.showsEPD game,
		shows $ State.InstancesByPosition.countConsecutiveRepeatablePlies instancesByPosition, -- 5. Half move clock.
		shows . succ {-the full-move counter starts at '1', before any move has occurred-} . length $ State.TurnsByLogicalColour.dereference turnsByLogicalColour Colour.LogicalColour.Black	-- 6. Full move counter.
	 ]

instance Property.Empty.Empty Game where
	empty	= Data.Default.def	-- i.e. zero turns have been taken, rather than zero pieces remain (which is illegal).

instance Property.Null.Null Game where
	isNull MkGame { getTurnsByLogicalColour = turnsByLogicalColour }	= Property.Null.isNull turnsByLogicalColour

{- |
	* Create an alternative game in which @Black@ moved first; <https://www.chessprogramming.org/Color_Flipping>.

	* N.B.: 'Property.Reflectable.ReflectableOnY' isn't implemented,
	since /reflectOnY/ produces a mirror-image /board/ in which the royal /piece/s start in a non-standard position & castling occurs the wrong way.
-}
instance Property.Reflectable.ReflectableOnX Game where
	reflectOnX MkGame {
		getNextLogicalColour			= nextLogicalColour,
		getCastleableRooksByLogicalColour	= castleableRooksByLogicalColour,
		getBoard				= board,
		getTurnsByLogicalColour			= turnsByLogicalColour,
		getInstancesByPosition			= instancesByPosition,
		getMaybeTerminationReason		= maybeTerminationReason
	} = (
		mkGame (
			Property.Opposable.getOpposite nextLogicalColour
		) (
			Property.Reflectable.reflectOnX castleableRooksByLogicalColour
		) (
			Property.Reflectable.reflectOnX board
		) (
			Property.Reflectable.reflectOnX turnsByLogicalColour
		)
	 ) {
		getInstancesByPosition		= Property.Reflectable.reflectOnX instancesByPosition,
		getMaybeTerminationReason	= fmap Property.Opposable.getOpposite maybeTerminationReason
	}

instance StateProperty.Hashable.Hashable Game where
	listRandoms zobrist game@MkGame {
		getNextLogicalColour			= nextLogicalColour,
		getCastleableRooksByLogicalColour	= castleableRooksByLogicalColour,
		getBoard				= board
	} = (
		if Colour.LogicalColour.isBlack nextLogicalColour
			then (Component.Zobrist.getRandomForBlacksMove zobrist :)
			else id
	 ) $ StateProperty.Hashable.listRandoms zobrist (
		maybeLastTurn game >>= State.EnPassantAbscissa.mkMaybeEnPassantAbscissa nextLogicalColour (
			State.Board.getMaybePieceByCoordinates board
		),
		castleableRooksByLogicalColour,
		board
	 ) -- Triple.

-- | Smart constructor.
mkGame
	:: Colour.LogicalColour.LogicalColour	-- ^ The player who is required to move next.
	-> State.CastleableRooksByLogicalColour.CastleableRooksByLogicalColour
	-> State.Board.Board
	-> State.CastleableRooksByLogicalColour.TurnsByLogicalColour
	-> Game
mkGame nextLogicalColour castleableRooksByLogicalColour board turnsByLogicalColour
	| not . StateProperty.Censor.hasBothKings $ State.Board.getCoordinatesByRankByLogicalColour board	= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Model.Game.mkGame:\tboth Kings must exist; " $ shows board "."
	| State.Board.isKingChecked board $ Property.Opposable.getOpposite nextLogicalColour			= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Model.Game.mkGame:\tthe player who last moved, is still checked; " $ shows board "."
	| otherwise	= game
	where
		game = MkGame {
			getNextLogicalColour				= nextLogicalColour,
			getCastleableRooksByLogicalColour		= castleableRooksByLogicalColour,
			getBoard					= board,
			getTurnsByLogicalColour				= turnsByLogicalColour,
			getMaybeChecked					= Data.List.find (State.Board.isKingChecked board) Property.FixedMembership.members,
			getInstancesByPosition				= State.InstancesByPosition.mkSingleton $ mkPosition game,
			getAvailableQualifiedMovesByLogicalColour	= Map.fromAscList [
				(logicalColour, mkAvailableQualifiedMovesFor game logicalColour) |
					logicalColour	<- Property.FixedMembership.members,
					getMaybeChecked game /= Just logicalColour	-- Define the available qualified moves for unchecked players only.
			], -- List-comprehension.
			getMaybeTerminationReason			= inferMaybeTerminationReason game
		}

{- | Constructor.
	For convenience, the following assumptions are made in the absence of any move-history:

		* The next player's /logical colour/ is assumed to be @White@;

		* Provided that the @King@ is at its starting /coordinates/, then all @Rook@s which exist at their starting /coordinates/ are considered to be castleable;

		* There're zero previous turns.
-}
fromBoard :: State.Board.Board -> Game
fromBoard board	= mkGame Colour.LogicalColour.White (
	State.CastleableRooksByLogicalColour.fromBoard board
 ) board Property.Empty.empty {-TurnsByLogicalColour-}

-- | Gets the sequence of /turn/s, with the latest at the head & the opening one last.
listTurns :: Game -> [Component.Turn.Turn]
listTurns MkGame {
	getNextLogicalColour	= nextLogicalColour,
	getTurnsByLogicalColour	= turnsByLogicalColour
} = uncurry ToolShed.Data.List.interleave $ (
	State.TurnsByLogicalColour.dereference turnsByLogicalColour . Property.Opposable.getOpposite &&& State.TurnsByLogicalColour.dereference turnsByLogicalColour
 ) nextLogicalColour

-- | Gets the sequence of /turn/s in the order they occured.
listTurnsChronologically :: Game -> [Component.Turn.Turn]
listTurnsChronologically	= reverse . listTurns

-- | The last /turn/, if there was one.
maybeLastTurn :: Game -> Maybe Component.Turn.Turn
maybeLastTurn MkGame {
	getNextLogicalColour	= nextLogicalColour,
	getTurnsByLogicalColour	= turnsByLogicalColour
} = Data.Maybe.listToMaybe . State.TurnsByLogicalColour.dereference turnsByLogicalColour $ Property.Opposable.getOpposite nextLogicalColour

{- |
	* Returns the castling /move/s currently available to the @King@ of the specified /logical colour/.

	* N.B.: only the @King@'s component of the /move/ is returned.

	* CAVEAT: this is a performance-hotspot; refactor => re-profile.
-}
findAvailableCastlingMoves :: Game -> Colour.LogicalColour.LogicalColour -> [Component.QualifiedMove.QualifiedMove]
findAvailableCastlingMoves MkGame {
	getCastleableRooksByLogicalColour	= castleableRooksByLogicalColour,
	getBoard				= board,
	getMaybeChecked				= maybeChecked
} logicalColour
	| Just checkedLogicalColour	<- maybeChecked
	, checkedLogicalColour == logicalColour	= []	-- One can't Castle out of check.
	| Just rooksStartingXs	<- State.CastleableRooksByLogicalColour.locateForLogicalColour castleableRooksByLogicalColour logicalColour	= [
		Component.QualifiedMove.mkQualifiedMove castlingKingsMove $ Component.CastlingMove.getMoveType castlingMove |
			x		<- rooksStartingXs,
			castlingMove	<- Component.CastlingMove.getCastlingMoves logicalColour,
			let castlingRooksSource	= Component.Move.getSource $ Component.CastlingMove.getRooksMove castlingMove,
			Cartesian.Coordinates.getX castlingRooksSource == x,
			State.MaybePieceByCoordinates.isClear (
				State.Board.getMaybePieceByCoordinates board
			) (
				Cartesian.Coordinates.kingsStartingCoordinates logicalColour
			) castlingRooksSource,
			let castlingKingsMove	= Component.CastlingMove.getKingsMove castlingMove,
			not $ State.Board.passesThroughCheck board logicalColour castlingKingsMove
	] {-list-comprehension-}
	| otherwise	= [] {-have already Castled-}

-- | List any /rank/s to which the specified /piece/ can be promoted on moving to the specified /destination/.
listMaybePromotionRanks
	:: Cartesian.Coordinates.Coordinates	-- ^ Destination.
	-> Component.Piece.Piece
	-> [Maybe Attribute.Rank.Rank]
{-# INLINE listMaybePromotionRanks #-}
listMaybePromotionRanks destination piece
	| Component.Piece.isPawnPromotion piece destination	= map Just Attribute.Rank.promotionProspects
	| otherwise						= [Nothing]

-- | The type of a function which transforms a /game/.
type Transformation	= Game -> Game

{- |
	* Moves the referenced /piece/ between the specified /coordinates/.

	* As a result of the /turn/, the next logical colour is changed, the /move/s available to each player are updated, & any reason for game-termination recorded.

	* CAVEAT: no validation of the /turn/ is performed since the /move/ may have been automatically selected & therefore known to be valid.

	* CAVEAT: doesn't account for any previous game-termination when updating 'getAvailableQualifiedMovesByLogicalColour'.
-}
takeTurn :: Component.Turn.Turn -> Transformation
takeTurn turn game@MkGame {
	getNextLogicalColour				= nextLogicalColour,
	getCastleableRooksByLogicalColour		= castleableRooksByLogicalColour,
	getBoard					= board,
	getTurnsByLogicalColour				= turnsByLogicalColour,
	getInstancesByPosition				= instancesByPosition,
	getAvailableQualifiedMovesByLogicalColour	= availableQualifiedMovesByLogicalColour
} = Control.Exception.assert (
	not $ isTerminated game	-- CAVEAT: otherwise any resignation will be overwritten.
 ) game' where
	((move, moveType), sourceRank)	= (Component.QualifiedMove.getMove &&& Component.QualifiedMove.getMoveType) . Component.Turn.getQualifiedMove &&& Component.Turn.getRank $! turn	-- Deconstruct.
	(source, destination)		= Component.Move.getSource &&& Component.Move.getDestination $ move	-- Deconstruct.

	opponentsLogicalColour :: Colour.LogicalColour.LogicalColour
	opponentsLogicalColour	= Property.Opposable.getOpposite nextLogicalColour

	inferredRooksMove	= Data.Maybe.maybe (
		Control.Exception.throw . Data.Exception.mkSearchFailure . showString "BishBosh.Model.Game.takeTurn:\tfailed to find any Rook's move corresponding to " $ shows (move, moveType) "."
	 ) Component.CastlingMove.getRooksMove . Data.List.find (
		(== move) . Component.CastlingMove.getKingsMove
	 ) $ Component.CastlingMove.getCastlingMoves nextLogicalColour

	board'	= (
		if Attribute.MoveType.isCastle moveType
			then State.Board.movePiece inferredRooksMove $ Just Data.Default.def {-move-type for the Rook's component of the Castling-}
			else id
	 ) $ State.Board.movePiece move (Just moveType) board

	maybePieceByCoordinates'	= State.Board.getMaybePieceByCoordinates board'	-- Deconstruct.

	game' = game {
		getNextLogicalColour				= opponentsLogicalColour,
		getCastleableRooksByLogicalColour		= State.CastleableRooksByLogicalColour.takeTurn nextLogicalColour turn castleableRooksByLogicalColour,
		getBoard					= board',
		getTurnsByLogicalColour				= State.TurnsByLogicalColour.prepend nextLogicalColour turn turnsByLogicalColour,
		getMaybeChecked					= Data.List.find (State.Board.isKingChecked board') [opponentsLogicalColour],
		getInstancesByPosition				= State.InstancesByPosition.insertPosition (Component.Turn.getIsRepeatableMove turn) (mkPosition game') instancesByPosition,
		getAvailableQualifiedMovesByLogicalColour	= let
			moveEndpoints :: [Cartesian.Coordinates.Coordinates]
			moveEndpoints	= (
				case moveType of
					Attribute.MoveType.Castle _	-> (++) [
						Component.Move.getSource inferredRooksMove,
						Component.Move.getDestination inferredRooksMove
					 ] -- The move-type of a move by the Castler's opponent, to either of the corresponding Rook's end-points, has now changed.
					Attribute.MoveType.EnPassant	-> (Cartesian.Coordinates.retreat nextLogicalColour destination :)	-- An opposing piece may have been blocked by their own Pawn, which has just been taken En-passant.
					_				-> id
			 ) [source, destination]

			kingsByCoordinates :: [Component.Piece.LocatedPiece]
			kingsByCoordinates	= map (
				State.CoordinatesByRankByLogicalColour.getKingsCoordinates (State.Board.getCoordinatesByRankByLogicalColour board') &&& Component.Piece.mkKing
			 ) Property.FixedMembership.members

			affected, affected'	:: [Component.Piece.LocatedPiece]
			(affected, affected')	= (
				Data.List.nub . (:) (
					destination,
					Component.Piece.mkPiece nextLogicalColour . Data.Maybe.fromMaybe sourceRank $ Attribute.Rank.getMaybePromotionRank moveType
				) *** Data.List.nub
			 ) . Data.List.partition (
				(== nextLogicalColour) . Component.Piece.getLogicalColour . snd {-piece-}
			 ) . (
				if Component.Turn.isPawnDoubleAdvance turn nextLogicalColour
					then (++) [
						(pawnCoordinates, oppositePiece) |
							let oppositePiece	= Component.Piece.mkPiece opponentsLogicalColour sourceRank,
							pawnCoordinates	<- Cartesian.Coordinates.getAdjacents destination,
							State.MaybePieceByCoordinates.dereference (State.Board.getMaybePieceByCoordinates board) pawnCoordinates == Just oppositePiece	-- Find any opposing Pawn which can capture En-passant.
					] {-list-comprehension-}
					else id
			 ) $ kingsByCoordinates {-moves available to either King may be constrained or liberated, even if misaligned with move-endpoints-} ++ [
				(knightsCoordinates, Component.Piece.mkKnight knightsColour) |
					knightsColour		<- Property.FixedMembership.members,	-- The moves for one's own Knights may be have been blocked by a friendly piece occupying an end-point, whereas the moves for opposing Knights will have a new move-type.
					knightsCoordinates	<- concatMap (StateProperty.Seeker.findProximateKnights board' knightsColour) moveEndpoints
			 ] {-list-comprehension-} ++ (
				if sourceRank == Attribute.Rank.King
					then concatMap (
						flip (
							State.MaybePieceByCoordinates.findBlockingPieces maybePieceByCoordinates'
						) Nothing {-omni-directional-} . fst {-coordinates-}
					) kingsByCoordinates	-- Re-evaluate the moves available to all pieces aligned with a King.
					else [
						(blockingCoordinates, blockingPiece) |
							(kingsCoordinates, _)			<- kingsByCoordinates,
							direction				<- Data.Maybe.mapMaybe (Cartesian.Vector.toMaybeDirection . Cartesian.Vector.measureDistance kingsCoordinates) moveEndpoints, -- N.B. null when the King isn't aligned with any move-endpoint.
							let findBlockingPieceFrom	= flip (State.MaybePieceByCoordinates.findBlockingPiece maybePieceByCoordinates') direction,
							(blockingCoordinates, blockingPiece)	<- Data.Maybe.maybeToList $ (
								\pair@(coordinates, _) -> if coordinates /= destination
									then Just pair
									else {-blocker is destination-} if Cartesian.Vector.toMaybeDirection (Cartesian.Vector.measureDistance kingsCoordinates source) == Just direction
										then Nothing
										else findBlockingPieceFrom coordinates	-- Look through the destination to the previous blocker; which might be the source.
							) =<< findBlockingPieceFrom kingsCoordinates
					] -- List-comprehension. Re-evaluate the moves available to all pieces aligned with a King & a move-endpoint.
			 ) ++ [
				locatedAffectedPiece |
					moveEndpoint		<- moveEndpoints,
					locatedAffectedPiece	<- filter (
						not . uncurry (||) . (
							(== destination) *** uncurry (||) . (
								Component.Piece.isKnight &&& Component.Piece.isKing
							) -- Added above.
						)
					) $ State.MaybePieceByCoordinates.findBlockingPieces maybePieceByCoordinates' moveEndpoint Nothing {-omni-directional-},
					uncurry (flip Component.Piece.canMoveBetween) locatedAffectedPiece moveEndpoint
			 ] -- List-comprehension. Re-evaluate the moves available to all pieces, which either could move to the source, or can now move to the destination, of the requested move.

			insertMovesFrom	:: AvailableQualifiedMoves -> [Component.Piece.LocatedPiece] -> AvailableQualifiedMoves
			insertMovesFrom	= foldr $ \(source', piece') -> let
				logicalColour			= Component.Piece.getLogicalColour piece'
				isSafeDestination destination'	= not . State.Board.exposesKing board' logicalColour $ Component.Move.mkMove source' destination'
			 in case [
				(destination', Attribute.MoveType.EnPassant) |
					Cartesian.Coordinates.isEnPassantRank source' logicalColour,
					Component.Piece.isPawn piece',
					destination'	<- Component.Piece.findAttackDestinations piece' source',
					State.MaybePieceByCoordinates.isVacant maybePieceByCoordinates' destination',
					uncurry (&&) . (
						(== Just (Property.Opposable.getOpposite piece')) . State.MaybePieceByCoordinates.dereference maybePieceByCoordinates' &&& (== move) . Component.Move.mkMove (Cartesian.Coordinates.advance logicalColour destination')
					) $ Cartesian.Coordinates.retreat logicalColour destination',	-- Did an opposing Pawn just double-advance to the expected position ?
					isSafeDestination destination'
			 ] {-list-comprehension-} ++ [
				(
					destination',
					Attribute.MoveType.mkNormalMoveType maybeTakenRank maybePromotionRank
				) |
					(destination', maybeTakenRank)	<- State.MaybePieceByCoordinates.listDestinationsFor maybePieceByCoordinates' source' piece',
					maybeTakenRank /= Just Attribute.Rank.King,	-- This move can never be made; the option will either be immediately removed or check-mate declared.
					isSafeDestination destination',
					maybePromotionRank		<- listMaybePromotionRanks destination' piece'
			 ] {-list-comprehension-} of
				[]			-> Map.delete source'				-- There're zero moves from here.
				qualifiedDestinations	-> Map.insert source' qualifiedDestinations	-- Overwrite any existing moves.

			insertCastlingMoves :: Colour.LogicalColour.LogicalColour -> AvailableQualifiedMoves -> AvailableQualifiedMoves
			insertCastlingMoves logicalColour	= case findAvailableCastlingMoves game' logicalColour of
				[]			-> id
				validCastlingMoves	-> uncurry (
					Map.insertWith (++)
				 ) $ (
					Component.Move.getSource {-the King-} . Component.QualifiedMove.getMove . head &&& map (
						Component.Move.getDestination . Component.QualifiedMove.getMove &&& Component.QualifiedMove.getMoveType
					)
				 ) validCastlingMoves
		in (
			\availableQualifiedMovesByLogicalColour' -> (
				case (Map.member opponentsLogicalColour availableQualifiedMovesByLogicalColour', Data.Maybe.isJust $ getMaybeChecked game') of
					(True, isChecked)
						| isChecked	-> Map.delete opponentsLogicalColour	-- Many changes result from the King being checked.
						| otherwise	-> Map.adjust (
							insertCastlingMoves opponentsLogicalColour . (
								`insertMovesFrom` affected'	-- Reconstruct any moves for affected pieces.
							) . (
								if Attribute.MoveType.isEnPassant moveType
									then Map.delete $ Cartesian.Coordinates.retreat nextLogicalColour destination
									else id
							) . Map.delete destination	-- Delete the moves originally available to any taken piece.
						) opponentsLogicalColour
					(_, isChecked)
						| isChecked	-> id	-- We neither want an entry in the map, nor is there one.
						| otherwise	-> Map.insert opponentsLogicalColour $ mkAvailableQualifiedMovesFor game' opponentsLogicalColour	-- Reconstruct.
			) availableQualifiedMovesByLogicalColour'
		) $ (
			if Data.Maybe.maybe True {-not a member-} (
				\availableQualifiedMoves -> sourceRank == Attribute.Rank.King || Data.Maybe.maybe False {-zero previous turns-} (
					`Component.Turn.isPawnDoubleAdvance` opponentsLogicalColour
				) (
					maybeLastTurn game	-- I.E. one's opponent.
				) {-only required for efficiency-} && Data.Foldable.any (
					any $ Attribute.MoveType.isEnPassant . snd {-moveType-}
				) availableQualifiedMoves
			) $ Map.lookup nextLogicalColour availableQualifiedMovesByLogicalColour
				then Map.insert nextLogicalColour $ mkAvailableQualifiedMovesFor game' nextLogicalColour	-- Reconstruct.
				else Map.adjust (
					insertCastlingMoves nextLogicalColour . (
						`insertMovesFrom` affected	-- Reconstruct any moves for affected pieces.
					) . Map.delete source		-- Delete the moves originally available to the moved piece.
				) nextLogicalColour
		) availableQualifiedMovesByLogicalColour,
		getMaybeTerminationReason	= inferMaybeTerminationReason game'	-- CAVEAT: this will overwrite any previous resignation.
	}

-- | Construct a /turn/ & relay the request to 'takeTurn'.
applyQualifiedMove :: Component.QualifiedMove.QualifiedMove -> Transformation
applyQualifiedMove qualifiedMove game@MkGame { getBoard = board }
	| Just piece	<- State.MaybePieceByCoordinates.dereference (State.Board.getMaybePieceByCoordinates board) $! Component.Move.getSource move
			= takeTurn (Component.Turn.mkTurn qualifiedMove $! Component.Piece.getRank piece) game
	| otherwise	= Control.Exception.throw . Data.Exception.mkSearchFailure . showString "BishBosh.Model.Game.applyQualifiedMove:\tthere isn't a piece at the source of " . shows move . showString "; " $ shows game "."
	where
		move	= Component.QualifiedMove.getMove qualifiedMove

-- | Construct a /qualifiedMove/ & relay the request to "applyQualifiedMove".
applyEitherQualifiedMove :: Component.EitherQualifiedMove.EitherQualifiedMove -> Transformation
applyEitherQualifiedMove eitherQualifiedMove game@MkGame { getBoard = board } = applyQualifiedMove (
	Component.QualifiedMove.mkQualifiedMove move . (
		State.MaybePieceByCoordinates.inferMoveType (State.Board.getMaybePieceByCoordinates board) move ||| id
	) $ Component.EitherQualifiedMove.getPromotionRankOrMoveType eitherQualifiedMove
 ) game where
	move	= Component.EitherQualifiedMove.getMove eitherQualifiedMove

-- | Constructs /eitherQualifiedMove/s from the data provided, validating & applying each in the specified order.
applyEitherQualifiedMoves
	:: (a -> Either String Component.EitherQualifiedMove.EitherQualifiedMove)	-- ^ A constructor which can return an error-message.
	-> Game										-- ^ The /game/ to which the /move/s should be sequentially applied.
	-> [a]										-- ^ An ordered sequence of data from which /move/s are constructed.
	-> Either (a, String) Game							-- ^ Either a rogue datum & the corresponding error-message, or the resulting /game/.
applyEitherQualifiedMoves moveConstructor	= Data.Foldable.foldl' (
	\eitherGame datum -> eitherGame >>= (
		\game -> Left . (,) datum {-Constructor failed-} ||| (
			\eitherQualifiedMove -> Data.Maybe.maybe (
				Right $ applyEitherQualifiedMove eitherQualifiedMove game
			) (
				\errorMessage -> Left (
					datum,
					showString "board" . Text.ShowList.showsAssociation . shows (getBoard game) . showString " (" $ shows errorMessage ")"
				) -- Pair.
			) $ validateEitherQualifiedMove game eitherQualifiedMove
		) $ moveConstructor datum
	)
 ) . Right

{- |
	* True if the specified /move/ is valid, given the implied /piece/ & the current state of the /game/.

	* N.B.: it is considered valid to take a @King@, one just never has the opportunity, since the game terminates the move before.
-}
validateQualifiedMove
	:: Game	-- ^ Prior to playing the /qualified move/.
	-> Component.QualifiedMove.QualifiedMove
	-> Maybe String	-- ^ Error-message.
validateQualifiedMove game@MkGame {
	getNextLogicalColour		= nextLogicalColour,
	getBoard			= board,
	getMaybeChecked			= maybeChecked,
	getMaybeTerminationReason	= maybeTerminationReason
} qualifiedMove = Control.Exception.assert (
	StateProperty.Censor.hasBothKings (
		State.Board.getCoordinatesByRankByLogicalColour board
	) && maybeChecked == Data.List.find (State.Board.isKingChecked board) Property.FixedMembership.members
 ) $ Data.Maybe.maybe (
	Data.Maybe.maybe (
		Just "there isn't a piece at the specified source-coordinates"	-- N.B.: this is also caught by 'validateEitherQualifiedMove'.
	) (
		\sourcePiece -> let
			sourceLogicalColour	= Component.Piece.getLogicalColour sourcePiece	-- Deconstruct.
		in lookup True $ Data.Maybe.maybe id (
			\destinationPiece -> (++) [
				(
					Component.Piece.isKing destinationPiece,			-- N.B.: this would otherwise prevent construction of the move-type.
					showString "a '" $ shows destinationPiece "' can't be taken"	-- N.B.: one should never be in a position where this can arise.
				), (
					Component.Piece.isFriend destinationPiece sourcePiece,
					showString "your own '" $ shows destinationPiece "' occupies the requested destination"
				)
			] -- Tests which depend on any taken piece.
		) maybeDestinationPiece [
			(
				sourceLogicalColour /= nextLogicalColour,
				showString "it's " . shows nextLogicalColour . showString "'s turn, but the referenced piece is " $ show sourceLogicalColour
			), (
				Attribute.MoveType.isPromotion moveType && not (Component.Piece.isPawn sourcePiece),
				showString "only a '" $ shows (Component.Piece.mkPawn sourceLogicalColour) "' can be promoted"
			)
		] {-tests which are independent of the type of the moving piece-} ++ map (
			Control.Arrow.second $ showString "regarding moving your '" . shows sourcePiece . showString "', "	-- Provide context.
		) (
			(
				case Component.Piece.getRank sourcePiece of
					Attribute.Rank.Pawn
						| destination `elem` Component.Piece.findAttackDestinations sourcePiece source	-> Data.Maybe.maybe (
							let
								opponentsCoordinates	= Cartesian.Coordinates.retreat sourceLogicalColour destination
								opponentsPawn		= Property.Opposable.getOpposite sourcePiece
							in [
								(
									not $ Cartesian.Coordinates.isEnPassantRank source sourceLogicalColour,
									showString "one can't take a '" $ shows opponentsPawn "' en-passant, from this rank"
								), (
									State.MaybePieceByCoordinates.isOccupied maybePieceByCoordinates destination,
									showString "taking a '" $ shows opponentsPawn "' en-passant, requires a move to a vacant square"
								), (
									State.MaybePieceByCoordinates.dereference maybePieceByCoordinates opponentsCoordinates /= Just opponentsPawn,
									shows "en-passant" . showString " requires a '" $ shows opponentsPawn "' to be taken"
								), (
									Data.Maybe.maybe True {-zero previous turns-} (
										(
											/= Component.Move.mkMove (Cartesian.Coordinates.advance sourceLogicalColour destination) opponentsCoordinates
										) . Component.QualifiedMove.getMove . Component.Turn.getQualifiedMove
									) $ maybeLastTurn game,
									showString "a '" $ shows opponentsPawn "' can only be taken en-passant, immediately after it has advanced two squares"
								)
							] -- En-Passant.
						) (
							const []	-- The Pawn is moving diagonally forwards, to a square occupied by the opponent's piece => valid.
						) maybeDestinationPiece
						| otherwise {-advance-}	-> (
							Cartesian.Vector.getXDistance distance /= 0,
							"it may only have a sideways component during attack"
						) : (
							case (
								if Colour.LogicalColour.isBlack sourceLogicalColour
									then negate
									else id
							) $ Cartesian.Vector.getYDistance distance of
								1	-> id
								2	-> (++) [
									(
										not $ Cartesian.Coordinates.isPawnsFirstRank source sourceLogicalColour,
										"it only has the option to advance two squares on its first move"
									), (
										isObstructed,
										"an obstruction can't be jumped"
									)
								 ]
								nSquares	-> (:) (
									True,
									if nSquares == 0
										then "it must advance"
										else if nSquares > 0
											then showString "it can't advance " $ shows nSquares " squares"
											else "it can't retreat"
								 )
						) [
							(
								Data.Maybe.isJust maybeDestinationPiece,
								"an advance must be to a vacant square"
							)
						] -- Singleton.
					Attribute.Rank.Rook	-> [
						(
							not $ Property.Orientated.isParallel move,
							"only moves parallel to the edges of the board are permissible"
						), (
							isObstructed,
							"an obstruction can't be jumped"
						)
					 ]
					Attribute.Rank.Knight	-> [
						(
							distance `notElem` Cartesian.Vector.attackVectorsForKnight,
							"the jump must be to the opposite corner of a 3 x 2 rectangle"
						) -- Pair.
					 ]
					Attribute.Rank.Bishop	-> [
						(
							not $ Property.Orientated.isDiagonal move,
							"only moves diagonal to the edges of the board are permissible"
						), (
							isObstructed,
							"an obstruction can't be jumped"
						)
					 ]
					Attribute.Rank.Queen	-> [
						(
							not $ Property.Orientated.isStraight move,
							"only straight moves are permissible"
						), (
							isObstructed,
							"an obstruction can't be jumped"
						)
					 ]
					Attribute.Rank.King
						| distance `elem` Cartesian.Vector.attackVectorsForKing	-> []	-- i.e. a normal move.
						| otherwise {-castling-}				-> Data.Maybe.maybe [
							(
								True,	-- i.e. validation-failure.
								"it can only castle (move two squares left or right from its starting position), or move one square in any direction"
							) -- Pair.
						] (
							(
								\rooksSource -> [
									(
										not $ State.CastleableRooksByLogicalColour.canCastleWith (getCastleableRooksByLogicalColour game) sourceLogicalColour rooksSource,
										showString "it has either already castled or lost the right to castle with the implied '" $ shows (Component.Piece.mkRook sourceLogicalColour) "'"
									), (
										State.MaybePieceByCoordinates.isObstructed maybePieceByCoordinates source rooksSource,
										"it can't castle through an obstruction"
									)
								]
							) . Component.Move.getSource . Component.CastlingMove.getRooksMove
						) (
							Data.List.find (
								(== move) . Component.CastlingMove.getKingsMove
							) $ Component.CastlingMove.getCastlingMoves sourceLogicalColour

						) ++ [
							(
								maybeChecked == Just sourceLogicalColour,
								"it can't castle out of check"
							), (
								State.Board.passesThroughCheck board sourceLogicalColour move,	-- The King mustn't pass through check when moving from source to destination (inclusive); a long castle still permits the square right of the Rook to be checked.
								"it can't castle through check"
							)
						] -- Tests which are independent of the implied Rook.
			) {-rank-specific test-} ++ [
				Control.Arrow.second (
					if Component.Piece.isKing sourcePiece
						then showString "it"
						else showString "your '" . shows (Component.Piece.mkKing sourceLogicalColour) . showChar '\''
				) $ if maybeChecked == Just sourceLogicalColour
					then (
						State.Board.isKingChecked (State.Board.movePiece move (Just moveType) board) sourceLogicalColour,	-- CAVEAT: don't perform an unvalidated move at the Game-level.
						" remains checked"
					) -- Pair.
					else (
						State.Board.exposesKing board sourceLogicalColour move,
						" would become exposed"
					) -- Pair.
			] -- Post-move tests on one's King.
		)
	) $ State.MaybePieceByCoordinates.dereference maybePieceByCoordinates source
 ) (
	Just . show	-- The game has been terminated, so there aren't any valid moves.
 ) maybeTerminationReason where
	(move, moveType)	= Component.QualifiedMove.getMove &&& Component.QualifiedMove.getMoveType $ qualifiedMove
	(source, destination)	= Component.Move.getSource &&& Component.Move.getDestination $ move	-- Deconstruct.
	maybePieceByCoordinates	= State.Board.getMaybePieceByCoordinates board
	maybeDestinationPiece	= State.MaybePieceByCoordinates.dereference maybePieceByCoordinates destination	-- Query.
	distance		= Component.Move.measureDistance move

	isObstructed :: Bool
	isObstructed	= State.MaybePieceByCoordinates.isObstructed maybePieceByCoordinates source destination

-- | Validates the /move-type/ then forwards the request to 'validateQualifiedMove'.
validateEitherQualifiedMove
	:: Game	-- ^ Prior to playing the /move/.
	-> Component.EitherQualifiedMove.EitherQualifiedMove
	-> Maybe String	-- ^ Error-message.
validateEitherQualifiedMove game@MkGame { getBoard = board } eitherQualifiedMove
	| State.MaybePieceByCoordinates.isVacant maybePieceByCoordinates (
		Component.Move.getSource move
	)		= Just "there isn't a piece at the specified source-coordinates"	-- Guard the call to 'State.MaybePieceByCoordinates.inferMoveType'.
	| Right moveType	<- promotionRankOrMoveType
	, moveType /= inferredMoveType		= Just . showString "the implied " . showString Attribute.MoveType.tag . Text.ShowList.showsAssociation . shows moveType . showString " /= " $ show inferredMoveType
	| otherwise				= validateQualifiedMove game $ Component.QualifiedMove.mkQualifiedMove move inferredMoveType
	where
		(move, promotionRankOrMoveType)	= Component.EitherQualifiedMove.getMove &&& Component.EitherQualifiedMove.getPromotionRankOrMoveType $ eitherQualifiedMove

		maybePieceByCoordinates		= State.Board.getMaybePieceByCoordinates board

		inferredMoveType :: Attribute.MoveType.MoveType
		inferredMoveType	= State.MaybePieceByCoordinates.inferMoveType maybePieceByCoordinates move $ (
			id ||| Attribute.Rank.getMaybePromotionRank
		 ) promotionRankOrMoveType	-- Discard any move-type.

-- | Whether the specified /QualifiedMove/ is valid.
isValidQualifiedMove :: Game -> Component.QualifiedMove.QualifiedMove -> Bool
isValidQualifiedMove game	= Data.Maybe.isNothing . validateQualifiedMove game

-- | Whether the specified /EitherQualifiedMove/ is valid.
isValidEitherQualifiedMove :: Game -> Component.EitherQualifiedMove.EitherQualifiedMove -> Bool
isValidEitherQualifiedMove game	= Data.Maybe.isNothing . validateEitherQualifiedMove game

{- |
	* Roll-back the specified /game/ until the start, returning each previous /game/ paired with the /ply/ which was then made.

	* The list-head contains the most recent /ply/, while the tail contains the first.
-}
rollBack :: Game -> [(Game, Component.Turn.Turn)]
rollBack	= Data.List.unfoldr (
	\game@MkGame {
		getNextLogicalColour	= nextLogicalColour,
		getBoard		= board,
		getTurnsByLogicalColour	= turnsByLogicalColour,
		getInstancesByPosition	= instancesByPosition
	} -> let
		previousColour	= Property.Opposable.getOpposite nextLogicalColour
	 in case State.TurnsByLogicalColour.dereference turnsByLogicalColour previousColour of
		turn : previousTurns	-> let
			(move, moveType)	= (Component.QualifiedMove.getMove &&& Component.QualifiedMove.getMoveType) $ Component.Turn.getQualifiedMove turn	-- Deconstruct.
			destination		= Component.Move.getDestination move	-- Deconstruct.

			game'@MkGame {
				getBoard		= board',
				getTurnsByLogicalColour	= turnsByLogicalColour',
				getMaybeChecked		= maybeChecked'
			} = game {
				getNextLogicalColour				= previousColour,
				getCastleableRooksByLogicalColour		= State.CastleableRooksByLogicalColour.fromTurnsByLogicalColour turnsByLogicalColour',
				getMaybeChecked					= Data.List.find (State.Board.isKingChecked board') [previousColour],
				getBoard					= (
					case moveType of
						Attribute.MoveType.Castle isShort	-> State.Board.movePiece (
							uncurry Component.Move.mkMove $ (
								Cartesian.Coordinates.translateX (
									if isShort then pred else succ
								) {-rook's source relative to the King-} &&& Cartesian.Coordinates.translateX (
									const $ if isShort then Cartesian.Abscissa.xMax else Cartesian.Abscissa.xMin
								) {-rook's destination-}
							) destination
						 ) $ Just Data.Default.def {-move-type-}	-- CAVEAT: this is only the Rook's part of the Castling.
						Attribute.MoveType.EnPassant		-> StateProperty.Mutator.placePiece (
							Component.Piece.mkPawn nextLogicalColour
						 ) $ Cartesian.Coordinates.advance nextLogicalColour destination	-- Re-instate the opponent's passing Pawn.
						_ {-normal-}
							| Attribute.MoveType.isPromotion moveType	-> StateProperty.Mutator.placePiece (
								Component.Piece.mkPawn previousColour	-- Demote the piece just returned to the source of the move.
							) $ Component.Move.getSource move
							| otherwise					-> id
				 ) . Data.Maybe.maybe id (
					(`StateProperty.Mutator.placePiece` destination) . Component.Piece.mkPiece nextLogicalColour
				 ) (
					Attribute.MoveType.getMaybeExplicitlyTakenRank moveType	-- Reconstruct any piece taken (except en-passant), inferring the logical colour.
				 ) $ State.Board.movePiece (Property.Opposable.getOpposite move) Nothing {-MoveType-} board,	-- N.B.: operate directly on the board to avoid creating a new Turn in the Game-structure.
				getTurnsByLogicalColour				= State.TurnsByLogicalColour.update [(previousColour, previousTurns)] turnsByLogicalColour,
				getInstancesByPosition				= if Component.Turn.getIsRepeatableMove turn
					then State.InstancesByPosition.deletePosition (mkPosition game) instancesByPosition
					else mkInstancesByPosition game',	-- Reconstruct the map prior to the unrepeatable move.
				getAvailableQualifiedMovesByLogicalColour	= Map.fromAscList [
					id &&& mkAvailableQualifiedMovesFor game' $ logicalColour |
						logicalColour	<- Property.FixedMembership.members,
						maybeChecked' /= Just logicalColour
				], -- List-comprehension.
				getMaybeTerminationReason			= Nothing
			}
		 in Just ((game', turn), game')
		_	-> Nothing
 )

{- |
	* List all the /move/s available to the specified player; which may not be the player who is required to move next.

	* CAVEAT: to avoid an infinite loop, this doesn't check whether the game has already terminated.
-}
listQualifiedMovesAvailableTo
	:: Game
	-> Colour.LogicalColour.LogicalColour	-- ^ Define the player for whom the moves are required.
	-> [Component.QualifiedMove.QualifiedMove]
listQualifiedMovesAvailableTo game@MkGame {
	getBoard	= board,
	getMaybeChecked	= maybeChecked
} logicalColour
	| maybeChecked == Just logicalColour = let
		kingsCoordinates	= State.CoordinatesByRankByLogicalColour.getKingsCoordinates coordinatesByRankByLogicalColour logicalColour
	in [
		Component.QualifiedMove.mkQualifiedMove move moveType |
			(destination, maybeTakenRank)	<- State.MaybePieceByCoordinates.listDestinationsFor maybePieceByCoordinates kingsCoordinates $ Component.Piece.mkKing logicalColour,
			let
				move		= Component.Move.mkMove kingsCoordinates destination
				moveType	= Attribute.MoveType.mkNormalMoveType maybeTakenRank Nothing {-promotion-rank-},
			null $ State.Board.findAttackersOf (State.Board.movePiece move (Just moveType) board) logicalColour destination -- Avoid moving the King into another check. CAVEAT: one can't merely use 'Board.exposesKing' since that assumes that one isn't already checked.
	] {-list-comprehension-} ++ case State.Board.findAttackersOf board logicalColour kingsCoordinates of
		[(checkedFrom, checkedByRank)]	-> Control.Exception.assert (checkedByRank /= Attribute.Rank.King) . filter isSafeQualifiedMove $ (
			if checkedByRank == Attribute.Rank.Pawn
				then Data.Maybe.maybe [] {-CAVEAT: this can occur if the game has just been read from FEN-} (
					(
						\lastMove -> let
							lastDestination	= Component.Move.getDestination lastMove
							pawn		= Component.Piece.mkPawn logicalColour
						in [
							Component.QualifiedMove.mkQualifiedMove (
								Component.Move.mkMove source $ Cartesian.Coordinates.advance logicalColour lastDestination	-- Construct a move which takes the attacker.
							) Attribute.MoveType.enPassant |
								Component.Move.isPawnDoubleAdvance lastMove opponentsLogicalColour,
								source	<- Cartesian.Coordinates.getAdjacents lastDestination,
								State.MaybePieceByCoordinates.dereference maybePieceByCoordinates source == Just pawn
						] -- List-comprehension.
					) . Component.QualifiedMove.getMove . Component.Turn.getQualifiedMove
				) $ maybeLastTurn game	-- The King is checked by a Pawn, which must also have been the last piece to move.
				else [] -- The King is checked by a piece other than a Pawn, so even if one can legitimately take en-passant, it won't resolve the issue.
		 ) ++ [
			Component.QualifiedMove.mkQualifiedMove (
				Component.Move.mkMove source checkedFrom	-- Construct a move which takes the attacker.
			) $ Attribute.MoveType.mkNormalMoveType (Just checkedByRank) maybePromotionRank |
				(source, attackersRank)	<- State.Board.findAttackersOf board opponentsLogicalColour checkedFrom,	-- See if the attacker can be taken (excluding en-passant).
				attackersRank /= Attribute.Rank.King,	-- The King can take its attacker, but it's already addressed above.
				maybePromotionRank	<- listMaybePromotionRanks checkedFrom {-destination-} $ Component.Piece.mkPiece logicalColour attackersRank
		 ] {-list-comprehension-} ++ [
			Component.QualifiedMove.mkQualifiedMove (
				Component.Move.mkMove source destination
			) $ Attribute.MoveType.mkNormalMoveType Nothing {-taken rank-} maybePromotionRank |
				checkedByRank /= Attribute.Rank.Knight,	-- A Knight can't be blocked.
				rank			<- Attribute.Rank.expendable,	-- Find pieces that might be able to block the checking piece.
				let piece	= Component.Piece.mkPiece logicalColour rank,
				source			<- State.CoordinatesByRankByLogicalColour.dereference coordinatesByRankByLogicalColour logicalColour rank,	-- Find the source of a potential blocking move.
				(destination, Nothing)	<- State.MaybePieceByCoordinates.listDestinationsFor maybePieceByCoordinates source piece,	-- The blocker must move to an empty square, otherwise the checker was already blocked.
				Control.Exception.assert (checkedFrom /= kingsCoordinates) $ Cartesian.Coordinates.isBetween kingsCoordinates checkedFrom destination,
				maybePromotionRank	<- listMaybePromotionRanks destination piece
		 ] -- List-comprehension.
		attackers		-> Control.Exception.assert (
			length attackers == 2	-- Triple-check isn't possible.
		 ) []	-- If checked by more than one piece, then the King must be moved; see options above.
	| otherwise {-not checked-}	= findAvailableCastlingMoves game logicalColour ++ filter isSafeQualifiedMove (
		[
			Component.QualifiedMove.mkQualifiedMove (
				Component.Move.mkMove source destination
			) Attribute.MoveType.enPassant |
				let pawn	= Component.Piece.mkPawn logicalColour,
				source		<- State.CoordinatesByRankByLogicalColour.dereference coordinatesByRankByLogicalColour logicalColour Attribute.Rank.Pawn,
				Cartesian.Coordinates.isEnPassantRank source logicalColour,
				destination	<- Component.Piece.findAttackDestinations pawn source,
				State.MaybePieceByCoordinates.isVacant maybePieceByCoordinates destination,
				let opponentsCoordinates	= Cartesian.Coordinates.retreat logicalColour destination,
				State.MaybePieceByCoordinates.dereference maybePieceByCoordinates opponentsCoordinates == Just (Property.Opposable.getOpposite pawn),
				Data.Maybe.maybe False {-zero previous turns-} (
					uncurry (&&) . (
						(== opponentsCoordinates) . Component.Move.getDestination &&& (
							== Cartesian.Coordinates.advance logicalColour destination
						) . Component.Move.getSource
					 ) . Component.QualifiedMove.getMove . Component.Turn.getQualifiedMove
				) $ maybeLastTurn game
		] {-List-comprehension. Include en-passant moves-} ++ [
			Component.QualifiedMove.mkQualifiedMove (
				Component.Move.mkMove source destination
			) $ Attribute.MoveType.mkNormalMoveType maybeTakenRank maybePromotionRank |
				(source, piece)			<- State.CoordinatesByRankByLogicalColour.findPiecesOfColour coordinatesByRankByLogicalColour logicalColour,
				(destination, maybeTakenRank)	<- State.MaybePieceByCoordinates.listDestinationsFor maybePieceByCoordinates source piece,
				maybeTakenRank /= Just Attribute.Rank.King,	-- This move can never be made; the option will either be immediately removed or check-mate declared.
				maybePromotionRank		<- listMaybePromotionRanks destination piece
		] -- List-comprehension.
	)
	where
		opponentsLogicalColour						= Property.Opposable.getOpposite logicalColour
		(maybePieceByCoordinates, coordinatesByRankByLogicalColour)	= State.Board.getMaybePieceByCoordinates &&& State.Board.getCoordinatesByRankByLogicalColour $ board
		isSafeQualifiedMove qualifiedMove				= not . State.Board.exposesKing board logicalColour $ Component.QualifiedMove.getMove qualifiedMove

-- | Construct 'AvailableQualifiedMoves' for the player of the specified /logical colour/.
mkAvailableQualifiedMovesFor :: Game -> Colour.LogicalColour.LogicalColour -> AvailableQualifiedMoves
mkAvailableQualifiedMovesFor game	= foldr {-maintains destination-order-} (
	\qualifiedMove -> let
		move	= Component.QualifiedMove.getMove qualifiedMove
	in Map.insertWith (++) (
		Component.Move.getSource move	-- Key.
	) [
		(
			Component.Move.getDestination move,
			Component.QualifiedMove.getMoveType qualifiedMove
		) -- Pair.
	] {-singleton-}
 ) Property.Empty.empty . listQualifiedMovesAvailableTo game

{- |
	* Retrieve the recorded value, or generate the list of /move/s available to the player of the specified /logical colour/.

	* CAVEAT: doesn't account for game-termination.
-}
findQualifiedMovesAvailableTo
	:: Game
	-> Colour.LogicalColour.LogicalColour
	-> [Component.QualifiedMove.QualifiedMove]
findQualifiedMovesAvailableTo game@MkGame { getAvailableQualifiedMovesByLogicalColour = availableQualifiedMovesByLogicalColour } logicalColour
	| Just availableQualifiedMoves <- Map.lookup logicalColour availableQualifiedMovesByLogicalColour	= [
		Component.QualifiedMove.mkQualifiedMove (Component.Move.mkMove source destination) moveType |
			(source, qualifiedDestinations)	<- Map.toList availableQualifiedMoves,
			(destination, moveType)		<- qualifiedDestinations
	] -- List-comprehension.
	| otherwise	= listQualifiedMovesAvailableTo game logicalColour	-- Generate the list of moves for this player.

-- | Count the number of plies available to the specified player.
countPliesAvailableTo :: Game -> Colour.LogicalColour.LogicalColour -> Type.Count.NPlies
countPliesAvailableTo game@MkGame { getAvailableQualifiedMovesByLogicalColour = availableQualifiedMovesByLogicalColour } logicalColour
	| isTerminated game	= 0
	| Just availableQualifiedMoves	<- Map.lookup logicalColour availableQualifiedMovesByLogicalColour	-- N.B.: 'findQualifiedMovesAvailableToNextPlayer' unnecessarily constructs a list.
	= fromIntegral $ Data.Foldable.foldl' (\acc -> (+ acc) . length) 0 availableQualifiedMoves
	| otherwise		= fromIntegral . length $ listQualifiedMovesAvailableTo game logicalColour

-- | Retrieve the recorded value, or generate the list of /move/s available to the next player.
findQualifiedMovesAvailableToNextPlayer :: Game -> [Component.QualifiedMove.QualifiedMove]
findQualifiedMovesAvailableToNextPlayer game@MkGame { getNextLogicalColour = nextLogicalColour }	= findQualifiedMovesAvailableTo game nextLogicalColour

-- | Let the specified player resign.
resignationBy :: Colour.LogicalColour.LogicalColour -> Transformation
resignationBy logicalColour game
	| isTerminated game	= game	-- Already terminated.
	| otherwise		= game {
		getMaybeTerminationReason	= Just $ Rule.GameTerminationReason.mkResignation logicalColour
	}

-- | Resignation by the player who currently holds the choice of /move/.
resign :: Transformation
resign game@MkGame { getNextLogicalColour = nextLogicalColour }	= resignationBy nextLogicalColour game

-- | Agree to a draw.
agreeToADraw :: Transformation
agreeToADraw game
	| isTerminated game	= game	-- Already terminated.
	| otherwise		= game {
		getMaybeTerminationReason	= Just $ Rule.GameTerminationReason.mkDraw Rule.DrawReason.byAgreement
	}

-- | Whether the game has been terminated.
isTerminated :: Game -> Bool
isTerminated MkGame { getMaybeTerminationReason	= maybeTerminationReason }	= Data.Maybe.isJust maybeTerminationReason

{- |
	* Inspects the current state of the /board/ to infer any reason for termination.

	* N.B.: resignation isn't included, because it leaves no evidence on the board.
-}
inferMaybeTerminationReason :: Game -> Maybe Rule.GameTerminationReason.GameTerminationReason
inferMaybeTerminationReason game@MkGame {
	getBoard		= board,
	getInstancesByPosition	= instancesByPosition
}
	| haveZeroMoves
	, Just logicalColour <- getMaybeChecked game	= Just $ Rule.GameTerminationReason.mkCheckMate logicalColour
	| otherwise					= Rule.GameTerminationReason.mkDraw <$> maybeDrawReason
	where
		haveZeroMoves :: Bool
		haveZeroMoves	= null $ findQualifiedMovesAvailableToNextPlayer game

		maybeDrawReason :: Maybe Rule.DrawReason.DrawReason
		maybeDrawReason
			| haveZeroMoves																= Just Rule.DrawReason.staleMate
			| State.InstancesByPosition.anyInstancesByPosition (== Rule.DrawReason.maximumConsecutiveRepeatablePositions) instancesByPosition	= Just Rule.DrawReason.fiveFoldRepetition
			| State.InstancesByPosition.countConsecutiveRepeatablePlies instancesByPosition == Rule.DrawReason.maximumConsecutiveRepeatablePlies	= Just Rule.DrawReason.seventyFiveMoveRule
			| StateProperty.Censor.hasInsufficientMaterial $ State.Board.getCoordinatesByRankByLogicalColour board					= Just Rule.DrawReason.insufficientMaterial
			| otherwise																= Nothing

-- | Provided that the game hasn't already terminated, update the termination-reason according to whether the specified result implies either a /draw by agreement/ or a /resignation/.
updateTerminationReasonWith :: Rule.Result.Result -> Transformation
updateTerminationReasonWith result game
	| Just victorsLogicalColour <- Rule.Result.findMaybeVictor result	= resignationBy (Property.Opposable.getOpposite victorsLogicalColour) game
	| otherwise								= agreeToADraw game

-- | Forwards request to "State.CastleableRooksByLogicalColour".
cantConverge :: Game -> Game -> Bool
cantConverge MkGame {
	getCastleableRooksByLogicalColour	= castleableRooksByLogicalColour
} MkGame {
	getCastleableRooksByLogicalColour	= castleableRooksByLogicalColour'
} = State.CastleableRooksByLogicalColour.cantConverge castleableRooksByLogicalColour castleableRooksByLogicalColour'

-- | Constructor.
mkPosition :: Game -> State.Position.Position
mkPosition game@MkGame {
	getNextLogicalColour			= nextLogicalColour,
	getBoard				= board,
	getCastleableRooksByLogicalColour	= castleableRooksByLogicalColour
} = State.Position.mkPosition nextLogicalColour (State.Board.getMaybePieceByCoordinates board) castleableRooksByLogicalColour $ maybeLastTurn game

-- | Constructor. Count the instances of each repeatable /position/.
mkInstancesByPosition :: Game -> InstancesByPosition
mkInstancesByPosition game	= State.InstancesByPosition.mkInstancesByPosition mkPosition . (game :) . map fst {-game-} . takeWhile (
	Component.Turn.getIsRepeatableMove . snd {-turn-}
 ) $ rollBack game

{- |
	* Whether the specified /game/'s /position/s have converged, & despite perhaps having reached this /position/ from different /move/-sequences, now have equal opportunities.

	* CAVEAT: this is different from equality.

	* CAVEAT: this test doesn't account for the possibility that one game may more quickly be drawn according to either the "Seventy-five-move Rule" or "Five-fold Repetition".

	* CAVEAT: though convenient, this function shouldn't be called for repeated tests against a constant /position/, resulting in unnecessary repeated construction of that /position/.
-}
(=~) :: Game -> Game -> Bool
game =~ game'	= mkPosition game == mkPosition game'

-- | Whether the state of the specified /game/s is different.
(/~) :: Game -> Game -> Bool
game /~ game'	= not $ game =~ game'

-- | Update the /position-hash/ of the /game/ prior to application of the last /move/.
updateIncrementalPositionHash
	:: Data.Bits.Bits positionHash
	=> Game		-- ^ The /game/ before application of the last move.
	-> positionHash	-- ^ The value before application of the last move.
	-> Game		-- ^ The current game.
	-> Component.Zobrist.Zobrist positionHash
	-> positionHash
{-# SPECIALISE updateIncrementalPositionHash :: Game -> Type.Crypto.PositionHash -> Game -> Component.Zobrist.Zobrist Type.Crypto.PositionHash -> Type.Crypto.PositionHash #-}
updateIncrementalPositionHash game positionHash game' zobrist	= StateProperty.Hashable.combine positionHash . (++) randomsFromMoveType . (
	let
		(castleableRooksByLogicalColour, castleableRooksByLogicalColour')	= ($ game) &&& ($ game') $ getCastleableRooksByLogicalColour
	in if isCastle || castleableRooksByLogicalColour /= castleableRooksByLogicalColour'
		then (
			State.CastleableRooksByLogicalColour.listIncrementalRandoms zobrist castleableRooksByLogicalColour castleableRooksByLogicalColour' ++
		) -- Section.
		else id
 ) $ [
	random |
		Just enPassantAbscissa	<- map (
			\g -> maybeLastTurn g >>= State.EnPassantAbscissa.mkMaybeEnPassantAbscissa (
				getNextLogicalColour g
			) (
				State.Board.getMaybePieceByCoordinates $ getBoard g
			) -- CAVEAT: accounts for any change to the En-passant option, rather than the act of taking En-passant.
		) [game, game'],
		random			<- StateProperty.Hashable.listRandoms zobrist enPassantAbscissa
 ] {-list-comprehension-} ++ Component.Zobrist.getRandomForBlacksMove zobrist : [
	Component.Zobrist.dereferenceRandomByCoordinatesByRankByLogicalColour zobrist (lastLogicalColour, rankAccessor turn, coordinatesAccessor move) |
		(rankAccessor, coordinatesAccessor)	<- zip [Component.Turn.getRank, (`Data.Maybe.fromMaybe` Attribute.Rank.getMaybePromotionRank moveType) . Component.Turn.getRank] coordinatesAccessors
 ] {-list-comprehension-} where
	lastLogicalColour	= getNextLogicalColour game
	turn			= Data.Maybe.fromMaybe (
		Control.Exception.throw $ Data.Exception.mkNullDatum "BishBosh.Model.Game.updateIncrementalPositionHash:\tzero turns have been made."
	 ) $ maybeLastTurn game'
	(move, moveType)	= Component.QualifiedMove.getMove &&& Component.QualifiedMove.getMoveType $ Component.Turn.getQualifiedMove turn
	isCastle		= Attribute.MoveType.isCastle moveType
	coordinatesAccessors	= [Component.Move.getSource, Component.Move.getDestination]

	randomsFromMoveType
		| Just rank <- Attribute.MoveType.getMaybeExplicitlyTakenRank moveType	= [Component.Zobrist.dereferenceRandomByCoordinatesByRankByLogicalColour zobrist (nextLogicalColour, rank, destination)] -- Singleton.
		| isCastle	= map (
			\coordinatesAccessor	-> Component.Zobrist.dereferenceRandomByCoordinatesByRankByLogicalColour zobrist (
				lastLogicalColour,
				Attribute.Rank.Rook,
				Data.Maybe.maybe (
					Control.Exception.throw $ Data.Exception.mkSearchFailure "BishBosh.Model.Game.updateIncrementalPositionHash.randomsFromMoveType:\tfailed to find castling-move."
				) (
					coordinatesAccessor . Component.CastlingMove.getRooksMove
				) . Data.List.find (
					(== move) . Component.CastlingMove.getKingsMove
				) $ Component.CastlingMove.getCastlingMoves lastLogicalColour
			)
		) coordinatesAccessors
		| Attribute.MoveType.isEnPassant moveType	= [Component.Zobrist.dereferenceRandomByCoordinatesByRankByLogicalColour zobrist (nextLogicalColour, Attribute.Rank.Pawn, Cartesian.Coordinates.advance nextLogicalColour destination)] -- Singleton.
		| otherwise	= []
		where
			nextLogicalColour	= getNextLogicalColour game'
			destination		= Component.Move.getDestination move

