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

 [@DESCRIPTION@]	The abscissae of those @Rook@s, for the player of each /logicalColour/, which can still participate in castling.
-}

module BishBosh.State.CastleableRooksByLogicalColour(
-- * Types
-- ** Type-synonyms
--	AbscissaeByLogicalColour,
	TurnsByLogicalColour,
--	Transformation,
-- ** Data-types
	CastleableRooksByLogicalColour(),
-- * Functions
--	sortByLogicalColour,
--	inferRooksCoordinates,
	locateForLogicalColour,
-- ** Constructors
	fromAssocs,
	fromBoard,
	fromTurnsByLogicalColour,
	listIncrementalRandoms,
-- ** Mutators
--	castle,
--	relinquishCastlingRights,
--	removeX,
	unify,
	takeTurn,
-- ** Predicates
	hasCastled,
	canCastle,
--	canCastleWith',
	canCastleWith,
	cantConverge
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.LogicalColour		as Attribute.LogicalColour
import qualified	BishBosh.Attribute.MoveType			as Attribute.MoveType
import qualified	BishBosh.Attribute.Rank				as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa			as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates			as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate			as Cartesian.Ordinate
import qualified	BishBosh.Component.Move				as Component.Move
import qualified	BishBosh.Component.Piece			as Component.Piece
import qualified	BishBosh.Component.QualifiedMove		as Component.QualifiedMove
import qualified	BishBosh.Component.Turn				as Component.Turn
import qualified	BishBosh.Component.Zobrist			as Component.Zobrist
import qualified	BishBosh.Data.Enum				as Data.Enum
import qualified	BishBosh.Data.Exception				as Data.Exception
import qualified	BishBosh.Property.ExtendedPositionDescription	as Property.ExtendedPositionDescription
import qualified	BishBosh.Property.FixedMembership		as Property.FixedMembership
import qualified	BishBosh.Property.ForsythEdwards		as Property.ForsythEdwards
import qualified	BishBosh.Property.Opposable			as Property.Opposable
import qualified	BishBosh.Property.Reflectable			as Property.Reflectable
import qualified	BishBosh.State.Board				as State.Board
import qualified	BishBosh.State.CoordinatesByRankByLogicalColour	as State.CoordinatesByRankByLogicalColour
import qualified	BishBosh.State.TurnsByLogicalColour		as State.TurnsByLogicalColour
import qualified	BishBosh.Types					as T
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.Char
import qualified	Data.Default
import qualified	Data.List
import qualified	Data.List.Extra
import qualified	Data.Maybe
import qualified	Data.Ord

{- |
	* For the players of each /logical colour/, identifies the abscissae of those @Rook@s which can still participate in castling (when other constraints are removed).

	* Lack of an entry for the specified /logical colour/ implies that castling has already occurred, whereas a null list of abscissae implies that castling can no longer happen.

	* N.B.: both the outer list (indexed by logical colour) & the inner list of abscissae, are kept ordered, otherwise the derived instance of 'Eq' would be unpredictable.
-}
type AbscissaeByLogicalColour x	= [(Attribute.LogicalColour.LogicalColour, [x])]

-- | Ensure a predictable order, to facilitate '(==)'.
sortByLogicalColour :: AbscissaeByLogicalColour x -> AbscissaeByLogicalColour x
sortByLogicalColour	= Data.List.sortBy $ Data.Ord.comparing fst {-logicalColour-}

-- | Update to account for the specified player castling.
castle :: Attribute.LogicalColour.LogicalColour -> AbscissaeByLogicalColour x -> AbscissaeByLogicalColour x
castle logicalColour	= filter $ (/= logicalColour) . fst {-logicalColour-}	-- N.B.: if 'Data.List.deleteBy' took a simple predicate, it would have been ideal.

-- | Update to account for the specified player losing the right to castle.
relinquishCastlingRights :: Attribute.LogicalColour.LogicalColour -> AbscissaeByLogicalColour x -> AbscissaeByLogicalColour x
relinquishCastlingRights logicalColour	= map $ \pair@(logicalColour', _) -> (
	if logicalColour' == logicalColour
		then Control.Arrow.second $ const []
		else id
 ) pair

-- | Remove the right to castle, from the referenced @Rook@.
removeX :: Eq x => Attribute.LogicalColour.LogicalColour -> x -> AbscissaeByLogicalColour x -> AbscissaeByLogicalColour x
removeX logicalColour x	= map $ \pair@(logicalColour', _) -> (
	if logicalColour' == logicalColour
		then Control.Arrow.second $ Data.List.delete x
		else id
 ) pair

-- | Predicate.
canCastleWith'
	:: Eq x
	=> Attribute.LogicalColour.LogicalColour
	-> x	-- ^ @Rook@'s abscissa.
	-> AbscissaeByLogicalColour x
	-> Bool
canCastleWith' logicalColour x	= Data.Maybe.maybe False {-has castled-} (elem x) . lookup logicalColour

-- | For the players of each /logical colour/, identifies the abscissae of those @Rook@s which can still participate in castling (when other constraints are removed).
newtype CastleableRooksByLogicalColour x	= MkCastleableRooksByLogicalColour {
	getAssocs	:: AbscissaeByLogicalColour x
} deriving (Eq, Ord)

instance Show x => Show (CastleableRooksByLogicalColour x) where
	showsPrec _ MkCastleableRooksByLogicalColour { getAssocs = assocs }	= shows assocs

instance (
	Enum	x,
	Ord	x,
	Read	x,
	Show	x
 ) => Read (CastleableRooksByLogicalColour x) where
	readsPrec _ s	= Control.Arrow.first fromAssocs `map` reads s

instance Control.DeepSeq.NFData x => Control.DeepSeq.NFData (CastleableRooksByLogicalColour x) where
	rnf MkCastleableRooksByLogicalColour { getAssocs = assocs }	= Control.DeepSeq.rnf assocs

instance Enum x => Data.Default.Default (CastleableRooksByLogicalColour x) where
	def = MkCastleableRooksByLogicalColour $ map (
		flip (,) [Cartesian.Abscissa.xMin, Cartesian.Abscissa.xMax]
	 ) Property.FixedMembership.members

instance Property.Reflectable.ReflectableOnX (CastleableRooksByLogicalColour x) where
	reflectOnX MkCastleableRooksByLogicalColour { getAssocs = assocs }	= MkCastleableRooksByLogicalColour . reverse $ map (
		Control.Arrow.first Property.Opposable.getOpposite
	 ) assocs

instance (
	Enum	x,
	Ord	x,
	Show	x
 ) => Property.ExtendedPositionDescription.ReadsEPD (CastleableRooksByLogicalColour x) where
	readsEPD s	= case Data.List.Extra.trimStart s of
		'-' : remainder	-> [
			(
				MkCastleableRooksByLogicalColour $ Property.FixedMembership.members `zip` repeat [],	-- CAVEAT: can't disambiguate between this potential value & '[]' which have different semantics for this application.
				remainder
			) -- Pair.
		 ] -- Singleton.
		s1		-> let
			readsAssocs s'
				| null s' || Data.Char.isSpace (head s')	= terminate	-- CAVEAT: white space separates this field from the start of the En-passant destination, which should it begin with a 'b' might be interpreted as a Bishop.
				| otherwise					= case reads s' of
					[(piece, s'')]	-> case Component.Piece.getRank piece of
						Attribute.Rank.Queen	-> Control.Arrow.first (
							(
								logicalColour,
								Cartesian.Abscissa.xMin
							) : {-prepend-}
						 ) `map` readsAssocs s''	-- Recurse.
						Attribute.Rank.King	-> Control.Arrow.first (
							(
								logicalColour,
								Cartesian.Abscissa.xMax
							) : {-prepend-}
						 ) `map` readsAssocs s''	-- Recurse.
						_			-> []	-- Inappropriate rank => parse-failure.
						where
							logicalColour	= Component.Piece.getLogicalColour piece
					_		-> terminate
				where
					terminate	= [([], s')]
		 in case readsAssocs s1 of
			[([], _)]	-> []	-- Zero pieces were read => parse-failure.
			l		-> Control.Arrow.first (fromAssocs . Data.List.Extra.groupSort) `map` l

instance (Enum x, Eq x) => Property.ExtendedPositionDescription.ShowsEPD (CastleableRooksByLogicalColour x) where
	showsEPD MkCastleableRooksByLogicalColour { getAssocs = assocs }
		| all (null . snd) assocs	= Property.ExtendedPositionDescription.showsNullField
		| otherwise			= foldr (
			(.) . Property.ExtendedPositionDescription.showsEPD
		) id [
			pieceConstructor logicalColour |
				logicalColour			<- [Attribute.LogicalColour.White, Attribute.LogicalColour.Black],	-- N.B.: the order is standardised.
				(rooksX, pieceConstructor)	<- [(Cartesian.Abscissa.xMax, Component.Piece.mkKing), (Cartesian.Abscissa.xMin, Component.Piece.mkQueen)],	-- N.B.: the order is defined as King-side (short) before Queen-side (long), which is also alphabetical.
				canCastleWith' logicalColour rooksX assocs
		] -- List-comprehension.

instance (
	Enum	x,
	Ord	x,
	Show	x
 ) => Property.ForsythEdwards.ReadsFEN (CastleableRooksByLogicalColour x)

instance (Enum x, Eq x) => Property.ForsythEdwards.ShowsFEN (CastleableRooksByLogicalColour x)

-- | Get the list of random numbers required to represent the current castling potential.
instance Eq x => Component.Zobrist.Hashable1D CastleableRooksByLogicalColour x {-CAVEAT: FlexibleInstances, MultiParamTypeClasses-} where
	listRandoms1D MkCastleableRooksByLogicalColour { getAssocs = assocs } zobrist	= Data.Maybe.catMaybes [
		Component.Zobrist.dereferenceRandomByCastleableRooksXByLogicalColour logicalColour x zobrist |
			logicalColour	<- Property.FixedMembership.members,
			x		<- Data.Maybe.fromMaybe [] $ lookup logicalColour assocs
	 ] -- List-comprehension.

-- | Smart constructor.
fromAssocs :: (
	Enum	x,
	Ord	x,
	Show	x
 ) => AbscissaeByLogicalColour x -> CastleableRooksByLogicalColour x
fromAssocs assocs
	| Data.List.Extra.anySame $ map fst {-logicalColour-} assocs	= Control.Exception.throw . Data.Exception.mkDuplicateData . showString "BishBosh.State.CastleableRooksByLogicalColour.fromAssocs:\tduplicate logical colours have been defined; " $ shows assocs "."
	| any (Data.List.Extra.anySame . snd) assocs			= Control.Exception.throw . Data.Exception.mkDuplicateData . showString "BishBosh.State.CastleableRooksByLogicalColour.fromAssocs:\tduplicate abscissae have been defined; " $ shows assocs "."
	| any (
		any (
			`notElem` [Cartesian.Abscissa.xMin, Cartesian.Abscissa.xMax]
		) . snd {-[x]-}
	) assocs							= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.State.CastleableRooksByLogicalColour.fromAssocs:\tall abscissae must reference unmoved Rooks; " $ shows assocs "."
	| otherwise	= MkCastleableRooksByLogicalColour . sortByLogicalColour $ map (Control.Arrow.second Data.List.sort) assocs

{- |
	* Smart constructor.

	* CAVEAT: doesn't know the move-history, so the wrong answer is possible.
-}
fromBoard :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Show	x
 ) => State.Board.Board x y -> CastleableRooksByLogicalColour x
fromBoard board
	| any (
		\logicalColour -> hasCastled logicalColour castleableRooksByLogicalColour && all (
			`elem` State.CoordinatesByRankByLogicalColour.dereference logicalColour Attribute.Rank.Pawn coordinatesByRankByLogicalColour
		) [
			Cartesian.Coordinates.mkCoordinates x (
				Cartesian.Ordinate.pawnsFirstRank logicalColour
			) |
				bishopsAbscissa	<- [
					Cartesian.Abscissa.translate (Data.Enum.translate (+ 2)) Cartesian.Abscissa.xMin,
					Cartesian.Abscissa.translate (Data.Enum.translate $ subtract 2) Cartesian.Abscissa.xMax
				 ],
				x		<- Cartesian.Abscissa.getAdjacents bishopsAbscissa
		] -- List-comprehension.
	) Property.FixedMembership.members	= Control.Exception.throw . Data.Exception.mkIncompatibleData . showString "BishBosh.State.CastleableRooksByLogicalColourFromBoard.fromBoard:\tfor castling to have occurred, a Bishop must have been moved, which can only happen when a blocking Pawn is moved; " $ shows (castleableRooksByLogicalColour, board) "."
	| otherwise	= castleableRooksByLogicalColour
	where
		coordinatesByRankByLogicalColour	= State.Board.getCoordinatesByRankByLogicalColour board
		castleableRooksByLogicalColour		= fromAssocs $ map (
			\logicalColour -> (
				logicalColour,
				[
					Cartesian.Coordinates.getX rooksCoordinates |
						State.CoordinatesByRankByLogicalColour.getKingsCoordinates logicalColour coordinatesByRankByLogicalColour == Cartesian.Coordinates.kingsStartingCoordinates logicalColour,
						rooksCoordinates	<- State.CoordinatesByRankByLogicalColour.dereference logicalColour Attribute.Rank.Rook coordinatesByRankByLogicalColour,
						rooksCoordinates `elem` Cartesian.Coordinates.rooksStartingCoordinates logicalColour
				] -- List-comprehension.
			) -- Pair.
		 ) Property.FixedMembership.members

-- | Narrow the type, so the /turn/ can be queried.
type TurnsByLogicalColour x y	= State.TurnsByLogicalColour.TurnsByLogicalColour (Component.Turn.Turn x y)

-- | Constructor.
fromTurnsByLogicalColour :: (
	Enum	x,
	Enum	y,
	Eq	x,
	Eq	y
 ) => TurnsByLogicalColour x y -> CastleableRooksByLogicalColour x
fromTurnsByLogicalColour turnsByLogicalColour	= MkCastleableRooksByLogicalColour $ foldr (
	\logicalColour -> let
		turns	= State.TurnsByLogicalColour.dereference logicalColour turnsByLogicalColour
	in if any (Attribute.MoveType.isCastle . Component.QualifiedMove.getMoveType . Component.Turn.getQualifiedMove) turns
		then id	-- Have Castled.
		else (:) (
			logicalColour,
			[
				Cartesian.Coordinates.getX coordinates |
					not $ haveMovedFrom (Cartesian.Coordinates.kingsStartingCoordinates logicalColour) turns,
					coordinates	<- Cartesian.Coordinates.rooksStartingCoordinates logicalColour,
					not $ haveMovedFrom coordinates turns || haveMovedTo coordinates (State.TurnsByLogicalColour.dereference (Property.Opposable.getOpposite logicalColour) turnsByLogicalColour)
			] -- List-comprehension.
		) -- Pair.
 ) [] Property.FixedMembership.members where
	haveMovedFrom, haveMovedTo :: (Eq x, Eq y) => Cartesian.Coordinates.Coordinates x y -> [Component.Turn.Turn x y] -> Bool
	haveMovedFrom coordinates	= any $ (== coordinates) . Component.Move.getSource . Component.QualifiedMove.getMove . Component.Turn.getQualifiedMove
	haveMovedTo coordinates		= any $ (== coordinates) . Component.Move.getDestination . Component.QualifiedMove.getMove . Component.Turn.getQualifiedMove

-- | Predicate.
hasCastled :: Attribute.LogicalColour.LogicalColour -> CastleableRooksByLogicalColour x -> Bool
hasCastled logicalColour MkCastleableRooksByLogicalColour { getAssocs = assocs }	= all ((/= logicalColour) . fst) assocs

-- | Predicate.
canCastle :: Attribute.LogicalColour.LogicalColour -> CastleableRooksByLogicalColour x -> Bool
canCastle logicalColour MkCastleableRooksByLogicalColour { getAssocs = assocs }	= Data.Maybe.maybe False {-has castled-} (not . null) $ lookup logicalColour assocs

-- | Infer the @Rook@'s ordinate from the /piece/'s /logical colour/.
inferRooksOrdinate :: Enum y => Attribute.LogicalColour.LogicalColour -> y
inferRooksOrdinate logicalColour
	| Attribute.LogicalColour.isBlack logicalColour	= Cartesian.Ordinate.yMax
	| otherwise					= Cartesian.Ordinate.yMin

-- | Predicate.
canCastleWith :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 )
	=> Attribute.LogicalColour.LogicalColour
	-> Cartesian.Coordinates.Coordinates x y	-- ^ @Rook@'s coordinates.
	-> CastleableRooksByLogicalColour x
	-> Bool
canCastleWith logicalColour rookSource MkCastleableRooksByLogicalColour { getAssocs = assocs }	= Data.Maybe.maybe False {-has castled-} (
	any $ (== rookSource) . (`Cartesian.Coordinates.mkCoordinates` inferRooksOrdinate logicalColour)
 ) $ lookup logicalColour assocs

-- | Find the abscissae of all @Rook@s of the specified /logical colour/, which can still participate in castling.
locateForLogicalColour :: Attribute.LogicalColour.LogicalColour -> CastleableRooksByLogicalColour x -> Maybe [x]
{-# INLINE locateForLogicalColour #-}
locateForLogicalColour logicalColour MkCastleableRooksByLogicalColour { getAssocs = assocs }	= lookup logicalColour assocs

-- | Self-documentation.
type Transformation x	= CastleableRooksByLogicalColour x -> CastleableRooksByLogicalColour x

-- | Relinquish the ability to disambiguate between "have Castled" (& therefore can't subsequently), & "Have lost the option to castle".
unify :: Transformation x
unify MkCastleableRooksByLogicalColour { getAssocs = assocs }	= MkCastleableRooksByLogicalColour $ foldr (
	\logicalColour assocs'	-> (
		if any ((== logicalColour) . fst) assocs
			then id
			else sortByLogicalColour . (
				(logicalColour, []) :
			)
	) assocs'
 ) assocs Property.FixedMembership.members

-- | Update with the latest /turn/.
takeTurn :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 )
	=> Attribute.LogicalColour.LogicalColour	-- ^ Defines the side who took the specified turn.
	-> Component.Turn.Turn x y
	-> Transformation x
{-# SPECIALISE takeTurn :: Attribute.LogicalColour.LogicalColour -> Component.Turn.Turn T.X T.Y -> Transformation T.X #-}
takeTurn logicalColour turn MkCastleableRooksByLogicalColour { getAssocs = assocs }	= MkCastleableRooksByLogicalColour $ (
	case lookup logicalColour assocs of
		Just []	-> id	-- This is a terminal state.
		Just rooksXs
			| Attribute.MoveType.isCastle $ Component.QualifiedMove.getMoveType qualifiedMove	-> castle logicalColour
			| Component.Turn.getRank turn == Attribute.Rank.King {-but not castling-}		-> relinquishCastlingRights logicalColour
			| let source	= Component.Move.getSource move
			, any (
				(== source) . (`Cartesian.Coordinates.mkCoordinates` inferRooksOrdinate logicalColour)
			) rooksXs										-> removeX logicalColour $ Cartesian.Coordinates.getX source
			| otherwise										-> id
		_												-> id	-- This is a terminal state.
 ) $ (
	let
		opponentsLogicalColour	= Property.Opposable.getOpposite logicalColour
	in case lookup opponentsLogicalColour assocs of
		Just rooksXs
			| let destination	= Component.Move.getDestination move
			, any (
				(== destination) . (`Cartesian.Coordinates.mkCoordinates` inferRooksOrdinate opponentsLogicalColour)
			) rooksXs	-> removeX opponentsLogicalColour $ Cartesian.Coordinates.getX destination
			| otherwise	-> id
		_			-> id	-- This is a terminal state.
 ) assocs where
	qualifiedMove	= Component.Turn.getQualifiedMove turn
	move		= Component.QualifiedMove.getMove qualifiedMove

{- |
	* Determines whether two /position/s can't converge on each other.

	* N.B.: in this function, the two /positions/ are considered to be peers; nothing is assumed regarding which must do the convergence, perhaps both.

	* From the initial board, one may converge onto any other /position/, but any of a set of irreversible changes may compromise this;
		the total number of /piece/s & specifically @Pawn@s, of each /logical colour/, can't increase;
		@Pawn@s can only advance;
		the difference in the /rank/s of all /piece/s of each /logical colour/, which can only be reduced through promotion of a @Pawn@;
		castling can't be undone.
	This function only assesses this final change.

	* CAVEAT: since the potential of one /position/ to converge on another, depends on a wider set of criteria,
	this function can only be definitive regarding when convergence is impossible, rather than when is possible.

	* CAVEAT: this function depends on one side having lost the right to castle, when the other side already has; this is quite rare.
-}
cantConverge
	:: CastleableRooksByLogicalColour x
	-> CastleableRooksByLogicalColour x
	-> Bool
cantConverge castleableRooksByLogicalColour castleableRooksByLogicalColour'	= any (
	\logicalColour -> case ($ castleableRooksByLogicalColour) &&& ($ castleableRooksByLogicalColour') $ locateForLogicalColour logicalColour of
		(Just [], Nothing)	-> True
		(Nothing, Just [])	-> True
		_			-> False
 ) Property.FixedMembership.members

-- | Generate the additional random-numbers required to correct the hash resulting from a change to the castleable @Rook@s.
listIncrementalRandoms
	:: Data.Array.IArray.Ix x
	=> CastleableRooksByLogicalColour x	-- ^ The old value.
	-> CastleableRooksByLogicalColour x	-- ^ The new value.
	-> Component.Zobrist.Zobrist x y random
	-> [random]
listIncrementalRandoms castleableRooksByLogicalColour castleableRooksByLogicalColour' zobrist	= [
	random |
		hashable	<- [castleableRooksByLogicalColour, castleableRooksByLogicalColour'],
		random		<- Component.Zobrist.listRandoms1D hashable zobrist
 ] -- List-comprehension.

