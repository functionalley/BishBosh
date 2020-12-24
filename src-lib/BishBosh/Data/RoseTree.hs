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

 [@DESCRIPTION@]	Defines operations on an arbitrary rose-tree.
-}

module BishBosh.Data.RoseTree(
-- * Types
-- ** Type-synonyms
--	Transformation,
	IsMatch,
-- * Function
	countTerminalNodes,
	drawTree,
	drawForest,
	traceRoute,
-- ** Mutators
	promote,
	reduce,
	mapForest
) where

import qualified	Data.List
import qualified	Data.Tree

-- Counts the number of terminal nodes.
countTerminalNodes :: Num nodes => Data.Tree.Tree a -> nodes
{-
countTerminalNodes Data.Tree.Node { Data.Tree.subForest = [] }		= 1
countTerminalNodes Data.Tree.Node { Data.Tree.subForest = forest }	= Data.List.foldl' (
	\acc -> (+ acc) . countTerminalNodes {-recurse-}
 ) 0 forest
-}
countTerminalNodes = go 0 where
	go acc Data.Tree.Node { Data.Tree.subForest = [] }	= acc + 1
	go acc Data.Tree.Node { Data.Tree.subForest = forest }	= Data.List.foldl' go acc forest

-- | Returns a string which graphically represents the tree.
drawTree :: (a -> String) -> Data.Tree.Tree a -> String
drawTree toString	= Data.Tree.drawTree . fmap toString

-- | Returns a string which graphically represents the forest.
drawForest :: (a -> String) -> Data.Tree.Forest a -> String
drawForest toString	= Data.Tree.drawForest . map (fmap toString)

-- | Whether a datum matches.
type IsMatch a	= a -> Bool

-- | Trace a path down the specified tree, of matching nodes.
traceRoute
	:: (datum -> IsMatch a)	-- ^ Whether a datum matches.
	-> Data.Tree.Tree a
	-> [datum]		-- ^ The data against which, nodes from the tree should be matched.
	-> Maybe [a]		-- ^ Returns 'Nothing' on match-failure.
traceRoute isMatch	= slave . Data.Tree.subForest where
	slave forest (datum : remainingData)	= Data.List.find (
		isMatch datum . Data.Tree.rootLabel
	 ) forest >>= (
		\Data.Tree.Node {
			Data.Tree.rootLabel	= rootLabel,
			Data.Tree.subForest	= subForest
		} -> (rootLabel :) `fmap` slave subForest remainingData {-recurse-}
	 )
	slave _ _				= Just []

{- |
	* Recursively advances the position within the forest, of the first node which matches the next datum, at successively deeper levels.

	* CAVEAT: each datum is expected to match exactly one item from the forest at each level.
-}
promote
	:: (datum -> IsMatch a)		-- ^ Whether a node matches.
	-> [datum]			-- ^ The data against which nodes from the forest should be matched.
	-> [Data.Tree.Tree a]
	-> Maybe [Data.Tree.Tree a]	-- ^ Returns 'Nothing' on match-failure.
promote isMatch	= slave where
	slave (datum : remainingData) forest	= case break (isMatch datum . Data.Tree.rootLabel) forest of
		(mismatches, match@Data.Tree.Node { Data.Tree.subForest = forest' } : remainingNodes)	-> (
			\forest'' -> match {
				Data.Tree.subForest	= forest''
			} : mismatches ++ remainingNodes
		 ) `fmap` slave remainingData forest'	-- Recurse.
		_											-> Nothing	-- Match-failure.
	slave _ forest				= Just forest	-- Data exhausted => Terminate normally.

-- | Reduce the tree to the first matching datum in the forest.
reduce
	:: IsMatch a
	-> Data.Tree.Tree a
	-> Maybe (Data.Tree.Tree a)
reduce isMatch Data.Tree.Node { Data.Tree.subForest = subForest }	= Data.List.find (isMatch . Data.Tree.rootLabel) subForest

-- | The type of a function which changes the structure (but not the type) of the specified tree.
type Transformation a	= Data.Tree.Tree a -> Data.Tree.Tree a

{- |
	* Apply an arbitrary mapping to all subForests; cf 'fmap' which applies an arbitrary function to all rootLabels.

	* The mapping is given access to the label at each forest.
-}
mapForest :: (a -> Data.Tree.Forest a -> Data.Tree.Forest a) -> Transformation a
mapForest f	= slave where
	slave node@Data.Tree.Node {
		Data.Tree.rootLabel	= label,
		Data.Tree.subForest	= forest
	} = node { Data.Tree.subForest = map slave {-recurse-} $ f label forest }

