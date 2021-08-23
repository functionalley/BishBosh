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

	Builds a rose-tree from a /PGN Database/,
	each node of which contains a /move/ qualified by a /move-type/, & possibly also the ultimate result & the game's identifier.
-}

module BishBosh.ContextualNotation.QualifiedMoveForest(
-- * Types
-- ** Type-synonyms
	Name,
	OnymousResult,
--	QualifiedMoveTree,
-- ** Data-types
	QualifiedMoveForest(
--		MkQualifiedMoveForest,
		deconstruct
	),
-- * Functions
	showsNames,
--	drawForest,
	findMinimumPieces,
	count,
-- ** Constructors
	fromPGNDatabase,
	toGameTree,
-- ** Mutators
	mergePGNDatabase,
 ) where

import			Control.Applicative((<|>))
import			Control.Arrow((&&&), (***))
import qualified	BishBosh.Attribute.MoveType			as Attribute.MoveType
import qualified	BishBosh.Component.Move				as Component.Move
import qualified	BishBosh.Component.Piece			as Component.Piece
import qualified	BishBosh.Component.QualifiedMove		as Component.QualifiedMove
import qualified	BishBosh.Component.Turn				as Component.Turn
import qualified	BishBosh.ContextualNotation.PGN			as ContextualNotation.PGN
import qualified	BishBosh.ContextualNotation.PGNDatabase		as ContextualNotation.PGNDatabase
import qualified	BishBosh.Data.RoseTree				as Data.RoseTree
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.Model.GameTerminationReason		as Model.GameTerminationReason
import qualified	BishBosh.Model.GameTree				as Model.GameTree
import qualified	BishBosh.Model.Result				as Model.Result
import qualified	BishBosh.Notation.MoveNotation			as Notation.MoveNotation
import qualified	BishBosh.Property.Empty				as Property.Empty
import qualified	BishBosh.Property.Null				as Property.Null
import qualified	BishBosh.State.Board				as State.Board
import qualified	BishBosh.Text.ShowList				as Text.ShowList
import qualified	BishBosh.Types					as T
import qualified	Control.Arrow
import qualified	Data.Default
import qualified	Data.List
import qualified	Data.Maybe
import qualified	Data.Tree

-- | Each /game/ has a name.
type Name	= String

-- | The name of a /game/, & it's /result/.
type OnymousResult	= (Name, Model.Result.Result)

{- |
	* Terminal nodes contain the unique name of the /move/-sequence leading to them, from which other information can be found as required, from the original database.

	* N.B.: non-terminal nodes would only need to be labelled with a /name/, if a /game/ exists in the database which is a truncated version of other /game/s in the database.

	* N.B.: provided there are no duplicate /game/s with different /name/s, there's no requirement for more than one /name/ at a node.

	* CAVEAT: since zero moves have been made in the default initial game, the move-tree for the whole game of chess has no apex, so a forest is a more natural structure; though sub-trees can exist.
-}
type QualifiedMoveTree x y	= Data.Tree.Tree (Component.QualifiedMove.QualifiedMove x y, Maybe OnymousResult)

{- |
	* A representation of a PGN-database, where initial /move/s shared between /game/s are merged into the trunk of a tree from which they each branch.

	* Many /game/s will share standard opening /move/s, & a tree-structure (cf. a list) uses this to increase space-time efficiency.

	* Since there are many different initial moves, the structure is a flat-topped /forest/ rather than a single apex /tree/.
-}
newtype QualifiedMoveForest x y	= MkQualifiedMoveForest {
	deconstruct	:: [QualifiedMoveTree x y]
} deriving (
	Eq,
	Show	-- CAVEAT: required by QuickCheck, but shouldn't actually be called.
 )

instance Property.Empty.Empty (QualifiedMoveForest x y) where
	empty	= MkQualifiedMoveForest Property.Empty.empty

instance Property.Null.Null (QualifiedMoveForest x y) where
	isNull MkQualifiedMoveForest { deconstruct = [] }	= True
	isNull _						= False

instance (Enum x, Enum y) => Notation.MoveNotation.ShowNotation (QualifiedMoveForest x y) where
	showsNotation moveNotation MkQualifiedMoveForest { deconstruct = forest }	= showString $ Data.RoseTree.drawForest (
		\(qualifiedMove, maybeOnymousResult)	-> Notation.MoveNotation.showsNotation moveNotation qualifiedMove $ Data.Maybe.maybe id (
			\onymousResult -> showChar ' ' . shows onymousResult
		) maybeOnymousResult ""
	 ) forest

-- | Shows a list of the names of archived games, optionally capped at the specified number.
showsNames
	:: Maybe Int	-- ^ The optional maximum number of names to show.
	-> [Name]
	-> ShowS
showsNames maybeMaximumPGNNames names	= Text.ShowList.showsUnterminatedList . map (
	\name -> showString "\n\t" . showString name
 ) $ Data.Maybe.maybe id (
	\maximumPGNNames -> (
		if maximumPGNNames < length names'
			then (++ ["..."])
			else id
	) . take maximumPGNNames
 ) maybeMaximumPGNNames names' where
	names'	= Data.List.nub $ Data.List.sort names

-- | Include the specified PGN-database into the /forest/, thus allowing more than one 'ContextualNotation.PGNDatabase.PGNDatabase' to be read.
mergePGNDatabase
	:: (Eq x, Eq y)
	=> ContextualNotation.PGNDatabase.PGNDatabase x y
	-> QualifiedMoveForest x y
	-> QualifiedMoveForest x y
mergePGNDatabase pgnDatabase MkQualifiedMoveForest { deconstruct = initialForest }	= MkQualifiedMoveForest $ foldr (
	\pgn -> merge (
		mkCompositeIdentifier &&& Data.Maybe.maybe (
			Model.Result.mkResult Nothing	-- The game is still in progress.
		) Model.GameTerminationReason.toResult . Model.Game.getMaybeTerminationReason . ContextualNotation.PGN.getGame $ pgn	-- Construct an onymous result.
	) (
		map Component.Turn.getQualifiedMove . Model.Game.listTurnsChronologically $ ContextualNotation.PGN.getGame pgn	-- Extract the list of qualified moves defining this game.
	)
 ) initialForest pgnDatabase where
	mkCompositeIdentifier :: ContextualNotation.PGN.PGN x y -> Name
	mkCompositeIdentifier	= unwords . map snd {-value-} . ContextualNotation.PGN.getIdentificationTagPairs

	merge
		:: (Eq x, Eq y)
		=> OnymousResult				-- ^ The name of this move-sequence, & the result.
		-> [Component.QualifiedMove.QualifiedMove x y]	-- ^ A chronological sequence of /qualified move/s.
		-> [QualifiedMoveTree x y]
		-> [QualifiedMoveTree x y]
	merge onymousResult qualifiedMoves@(qualifiedMove : remainingQualifiedMoves) forest	= case span (
		\Data.Tree.Node { Data.Tree.rootLabel = (qualifiedMove', _) } -> Component.QualifiedMove.getMove qualifiedMove /= Component.QualifiedMove.getMove qualifiedMove'
	 ) forest of
		(unmatchedForest, matchingTree : remainingForest)	-> unmatchedForest ++ (
			if null remainingQualifiedMoves	-- i.e. the terminal move in this game.
				then matchingTree {
					Data.Tree.rootLabel	= Control.Arrow.second (
						<|> Just onymousResult	-- CAVEAT: in the event of identical move-sequences, arbitrarily preserve the incumbant (whose result may differ if decided by resignation).
					) $ Data.Tree.rootLabel matchingTree
				}
				else matchingTree {
					Data.Tree.subForest	= merge onymousResult remainingQualifiedMoves $ Data.Tree.subForest matchingTree	-- Recurse.
				}
		 ) : remainingForest
		_ {-no match-}						-> mkLinkedList onymousResult qualifiedMoves : forest
	merge _ [] forest					= forest

	mkLinkedList :: OnymousResult -> [Component.QualifiedMove.QualifiedMove x y] -> QualifiedMoveTree x y
	mkLinkedList onymousResult ~(qualifiedMove : remainingQualifiedMoves)
		| null remainingQualifiedMoves	= Data.Tree.Node {
			Data.Tree.rootLabel	= (qualifiedMove, Just onymousResult),
			Data.Tree.subForest	= []
		} -- The terminal node.
		| otherwise	= Data.Tree.Node {
			Data.Tree.rootLabel	= (qualifiedMove, Nothing),
			Data.Tree.subForest	= [mkLinkedList onymousResult remainingQualifiedMoves {-recurse-}]
		}

-- | Constructor.
fromPGNDatabase :: (Eq x, Eq y) => ContextualNotation.PGNDatabase.PGNDatabase x y -> QualifiedMoveForest x y
fromPGNDatabase	= (`mergePGNDatabase` Property.Empty.empty {-QualifiedMoveForest-})

-- | Find the minimum number of /piece/s in any of the recorded /game/s.
findMinimumPieces :: QualifiedMoveForest x y -> Component.Piece.NPieces
findMinimumPieces	= slave (
	State.Board.getNPieces (
		Data.Default.def	:: State.Board.Board T.X T.Y	-- CAVEAT: this assumes the game to which the moves in the forest refer.
	)
 ) . deconstruct where
	slave nPieces []	= nPieces
	slave nPieces forest	= minimum $ map (
		\Data.Tree.Node {
			Data.Tree.rootLabel	= (qualifiedMove, _),
			Data.Tree.subForest	= subForest
		} -> slave (
			Attribute.MoveType.nPiecesMutator (Component.QualifiedMove.getMoveType qualifiedMove) nPieces
		) subForest	-- Recurse.
	 ) forest

-- | Count the number of /game/s & /plies/.
count :: QualifiedMoveForest x y -> (Model.Game.NGames, Component.Move.NPlies)
count	= slave . deconstruct where
	slave	= Data.List.foldl' (
		\(nGames, nPlies) Data.Tree.Node {
			Data.Tree.rootLabel	= (_, maybeOnymousResult),
			Data.Tree.subForest	= forest
		} -> let
			acc@(nGames', nPlies')	= (
				(+ nGames) . (
					if Data.Maybe.isJust maybeOnymousResult
						then succ
						else id
				) *** (+ nPlies) . succ
			 ) $ slave forest {-recurse-}
		in nGames' `seq` nPlies' `seq` acc
	 ) (0, 0)

{- |
	* Convert the specified /qualified-move forest/ to a /game-tree/.

	* N.B.: to construct a tree from the specified forest, the default initial /game/ is included at the apex.
-}
toGameTree :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Show	x,
	Show	y
 ) => QualifiedMoveForest x y -> Model.GameTree.GameTree x y
{-# SPECIALISE toGameTree :: QualifiedMoveForest T.X T.Y -> Model.GameTree.GameTree T.X T.Y #-}
toGameTree MkQualifiedMoveForest { deconstruct = qualifiedMoveForest }	= Model.GameTree.fromBareGameTree Data.Tree.Node {
	Data.Tree.rootLabel	= initialGame,
	Data.Tree.subForest	= map (slave initialGame) qualifiedMoveForest
} where
	initialGame	= Data.Default.def

	slave game Data.Tree.Node {
		Data.Tree.rootLabel	= (qualifiedMove, _),
		Data.Tree.subForest	= qualifiedMoveForest'
	} = Data.Tree.Node {
		Data.Tree.rootLabel	= game',
		Data.Tree.subForest	= map (slave game') qualifiedMoveForest'	-- Recurse.
	} where
		game'	= Model.Game.applyQualifiedMove qualifiedMove game
