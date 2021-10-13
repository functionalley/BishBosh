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
import qualified	BishBosh.Attribute.MoveType		as Attribute.MoveType
import qualified	BishBosh.Component.QualifiedMove	as Component.QualifiedMove
import qualified	BishBosh.Component.Turn			as Component.Turn
import qualified	BishBosh.ContextualNotation.PGN		as ContextualNotation.PGN
import qualified	BishBosh.ContextualNotation.PGNDatabase	as ContextualNotation.PGNDatabase
import qualified	BishBosh.Data.RoseTree			as Data.RoseTree
import qualified	BishBosh.Model.Game			as Model.Game
import qualified	BishBosh.Model.GameTree			as Model.GameTree
import qualified	BishBosh.Notation.MoveNotation		as Notation.MoveNotation
import qualified	BishBosh.Property.Empty			as Property.Empty
import qualified	BishBosh.Property.Null			as Property.Null
import qualified	BishBosh.Rule.GameTerminationReason	as Rule.GameTerminationReason
import qualified	BishBosh.Rule.Result			as Rule.Result
import qualified	BishBosh.State.Board			as State.Board
import qualified	BishBosh.Text.ShowList			as Text.ShowList
import qualified	BishBosh.Type.Count			as Type.Count
import qualified	Control.Arrow
import qualified	Data.Default
import qualified	Data.List
import qualified	Data.Maybe
import qualified	Data.Tree

-- | Each /game/ has a name.
type Name	= String

-- | The name of a /game/, & it's /result/.
type OnymousResult	= (Name, Rule.Result.Result)

{- |
	* Terminal nodes contain the unique name of the /move/-sequence leading to them, from which other information can be found as required, from the original database.

	* N.B.: non-terminal nodes would only need to be labelled with a /name/, if a /game/ exists in the database which is a truncated version of other /game/s in the database.

	* N.B.: provided there are no duplicate /game/s with different /name/s, there's no requirement for more than one /name/ at a node.

	* CAVEAT: since zero moves have been made in the default initial game, the move-tree for the whole game of chess has no apex, so a forest is a more natural structure; though sub-trees can exist.
-}
type QualifiedMoveTree	= Data.Tree.Tree (Component.QualifiedMove.QualifiedMove, Maybe OnymousResult)

{- |
	* A representation of a PGN-database, where initial /move/s shared between /game/s are merged into the trunk of a tree from which they each branch.

	* Many /game/s will share standard opening /move/s, & a tree-structure (cf. a list) uses this to increase space-time efficiency.

	* Since there are many different initial moves, the structure is a flat-topped /forest/ rather than a single apex /tree/.
-}
newtype QualifiedMoveForest	= MkQualifiedMoveForest {
	deconstruct	:: [QualifiedMoveTree]
} deriving (
	Eq,
	Show	-- CAVEAT: required by QuickCheck, but shouldn't actually be called.
 )

instance Property.Empty.Empty QualifiedMoveForest where
	empty	= MkQualifiedMoveForest Property.Empty.empty

instance Property.Null.Null QualifiedMoveForest where
	isNull MkQualifiedMoveForest { deconstruct = [] }	= True
	isNull _						= False

instance Notation.MoveNotation.ShowNotation QualifiedMoveForest where
	showsNotation moveNotation MkQualifiedMoveForest { deconstruct = forest }	= showString $ Data.RoseTree.drawForest (
		\(qualifiedMove, maybeOnymousResult)	-> Notation.MoveNotation.showsNotation moveNotation qualifiedMove $ Data.Maybe.maybe id (
			\onymousResult -> showChar ' ' . shows onymousResult
		) maybeOnymousResult ""
	 ) forest

-- | Shows an optional capped list of the names of archived games.
showsNames
	:: Maybe Type.Count.NGames	-- ^ The optional maximum number of names to show.
	-> [Name]
	-> ShowS
showsNames maybeMaximumPGNNames names	= Text.ShowList.showsUnterminatedList . map (
	\name -> showString "\n\t" . showString name
 ) $ Data.Maybe.maybe id (
	\maximumPGNNames -> (
		if fromIntegral maximumPGNNames < length names'
			then (++ ["..."])
			else id
	) . take (fromIntegral maximumPGNNames)
 ) maybeMaximumPGNNames names' where
	names'	= Data.List.nub $ Data.List.sort names

-- | Include the specified PGN-database into the /forest/, thus allowing more than one 'ContextualNotation.PGNDatabase.PGNDatabase' to be read.
mergePGNDatabase
	:: ContextualNotation.PGNDatabase.PGNDatabase
	-> QualifiedMoveForest
	-> QualifiedMoveForest
mergePGNDatabase pgnDatabase MkQualifiedMoveForest { deconstruct = initialForest }	= MkQualifiedMoveForest $ foldr (
	\pgn -> merge (
		mkCompositeIdentifier &&& Data.Maybe.maybe (
			Rule.Result.mkResult Nothing	-- The game is still in progress.
		) Rule.GameTerminationReason.toResult . Model.Game.getMaybeTerminationReason . ContextualNotation.PGN.getGame $ pgn	-- Construct an onymous result.
	) (
		map Component.Turn.getQualifiedMove . Model.Game.listTurnsChronologically $ ContextualNotation.PGN.getGame pgn	-- Extract the list of qualified moves defining this game.
	)
 ) initialForest pgnDatabase where
	mkCompositeIdentifier :: ContextualNotation.PGN.PGN -> Name
	mkCompositeIdentifier	= unwords . map snd {-value-} . ContextualNotation.PGN.getIdentificationTagPairs

	merge
		:: OnymousResult				-- ^ The name of this move-sequence, & the result.
		-> [Component.QualifiedMove.QualifiedMove]	-- ^ A chronological sequence of /qualified move/s.
		-> [QualifiedMoveTree]
		-> [QualifiedMoveTree]
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

	mkLinkedList :: OnymousResult -> [Component.QualifiedMove.QualifiedMove] -> QualifiedMoveTree
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
fromPGNDatabase :: ContextualNotation.PGNDatabase.PGNDatabase -> QualifiedMoveForest
fromPGNDatabase	= (`mergePGNDatabase` Property.Empty.empty {-QualifiedMoveForest-})

-- | Find the minimum number of /piece/s in any of the recorded /game/s.
findMinimumPieces :: QualifiedMoveForest -> Type.Count.NPieces
findMinimumPieces	= slave (
	State.Board.getNPieces (
		Data.Default.def	:: State.Board.Board	-- CAVEAT: this assumes the game to which the moves in the forest refer.
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

-- | Count the number of /game/s & distinct /positions/.
count :: QualifiedMoveForest -> (Type.Count.NGames, Type.Count.NPositions)
count	= slave . deconstruct where
	slave	= Data.List.foldl' (
		\(nGames, nPositions) Data.Tree.Node {
			Data.Tree.rootLabel	= (_, maybeOnymousResult),
			Data.Tree.subForest	= forest
		} -> let
			acc@(nGames', nPositions')	= (
				(+ nGames) . (
					if Data.Maybe.isJust maybeOnymousResult
						then succ
						else id
				) *** (+ nPositions) . succ
			 ) $ slave forest {-recurse-}
		in nGames' `seq` nPositions' `seq` acc
	 ) (0, 0)

{- |
	* Convert the specified /qualified-move forest/ to a /game-tree/.

	* N.B.: to construct a tree from the specified forest, the default initial /game/ is included at the apex.
-}
toGameTree :: QualifiedMoveForest -> Model.GameTree.GameTree
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

