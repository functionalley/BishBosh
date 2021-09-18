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
-}

module BishBosh.Model.PositionHashTree(
-- * Types
-- ** Type-synonyms
--	BarePositionHashTree,
-- ** Data-types
	PositionHashTree(),
-- * Function
	countDistinctPositions,
-- ** Constructors
	mkPositionHashTree
) where

import qualified	BishBosh.Component.Zobrist	as Component.Zobrist
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Model.GameTree		as Model.GameTree
import qualified	BishBosh.Property.Arboreal	as Property.Arboreal
import qualified	BishBosh.Text.ShowList		as Text.ShowList
import qualified	BishBosh.Type.Count		as Type.Count
import qualified	BishBosh.Type.Crypto		as Type.Crypto
import qualified	BishBosh.Type.Length		as Type.Length
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.Bits
import qualified	Data.Default
import qualified	Data.List
import qualified	Data.Set
import qualified	Data.Tree
import qualified	System.Random

-- | The hash of a /game-tree/.
type BarePositionHashTree positionHash	= Data.Tree.Tree positionHash

-- | Wrap a 'BarePositionHashTree'.
newtype PositionHashTree positionHash	= MkPositionHashTree {
	deconstruct	:: BarePositionHashTree positionHash
}

instance (
	Data.Bits.FiniteBits	positionHash,
	System.Random.Random	positionHash
 ) => Data.Default.Default (PositionHashTree positionHash) where
	def	= mkPositionHashTree Data.Default.def (Data.Default.def :: Model.GameTree.GameTree Type.Length.X Type.Length.Y)

-- | Hash the specified 'game-tree/.
mkPositionHashTree :: (
	Data.Array.IArray.Ix	x,
	Data.Bits.Bits		positionHash,
	Enum			x,
	Enum			y,
	Ord			y
 )
	=> Component.Zobrist.Zobrist x y positionHash
	-> Model.GameTree.GameTree x y
	-> PositionHashTree positionHash
mkPositionHashTree zobrist	= MkPositionHashTree . fmap (`Component.Zobrist.hash2D` zobrist) . Model.GameTree.deconstruct

-- | Count the number of distinct positions, irrespective of the sequence of moves taken to reach that terminal state.
countDistinctPositions
	:: Ord positionHash
	=> Property.Arboreal.Depth
	-> PositionHashTree positionHash
	-> Type.Count.NPositions
{-# SPECIALISE countDistinctPositions :: Property.Arboreal.Depth -> PositionHashTree Type.Crypto.PositionHash -> Type.Count.NPositions #-}
countDistinctPositions depth MkPositionHashTree { deconstruct = barePositionHashTree }
	| depth < 0	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Component.PositionHashTree.countDistinctPositions:\tdepth" . Text.ShowList.showsAssociation $ shows depth "must be positive"
	| otherwise	= fromIntegral . Data.Set.size $ slave depth barePositionHashTree
	where
		slave 0 Data.Tree.Node { Data.Tree.rootLabel = hash }		= Data.Set.singleton hash	-- Having reached the maximum depth, include this game's hash.
		slave _ Data.Tree.Node {
			Data.Tree.rootLabel	= hash,
			Data.Tree.subForest	= []
		}								= Data.Set.singleton hash	-- Being unable to descend further, include the terminal game's hash.
		slave depth' Data.Tree.Node { Data.Tree.subForest = forest }	= Data.List.foldl' (
			\s -> Data.Set.union s . slave (pred depth') {-recurse-}
		 ) Data.Set.empty forest

