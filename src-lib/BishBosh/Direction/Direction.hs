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

	* Describes a /direction/.

	* The resolution of the measure is merely sufficient for Chess-moves.

	* The IO-format is based on 8 points of the compass.
-}

module BishBosh.Direction.Direction(
-- * Types
-- ** Type-synonyms
	ArrayByDirection,
-- ** Data-types
	Direction(),
-- * Constants
	parallels,
	s,
	n,
	w,
	e,
	diagonals,
	sw,
	se,
	nw,
	ne,
	opposites,
-- * Functions
	attackDirectionsForPawn,
	listArrayByDirection,
-- ** Predicates
	areAligned
) where

import			Control.Arrow((&&&), (|||), (+++))
import qualified	BishBosh.Colour.LogicalColour		as Colour.LogicalColour
import qualified	BishBosh.Direction.Diagonal		as Direction.Diagonal
import qualified	BishBosh.Direction.Parallel		as Direction.Parallel
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	BishBosh.Property.Orientated		as Property.Orientated
import qualified	BishBosh.Property.Reflectable		as Property.Reflectable
import qualified	Control.DeepSeq
import qualified	Data.Array.IArray
import qualified	Data.List.Extra

-- | Define the /direction/ in which a piece moves relative to the board's edges.
newtype Direction	= MkDirection (Either Direction.Parallel.Parallel Direction.Diagonal.Diagonal) deriving (Eq, Ord)

instance Control.DeepSeq.NFData Direction where
	rnf (MkDirection d)	= Control.DeepSeq.rnf ||| Control.DeepSeq.rnf $ d

instance Show Direction where
	showsPrec precedence (MkDirection d)	= showsPrec precedence ||| showsPrec precedence $ d

instance Read Direction where
	readsPrec precedence string	= let
		s'	= Data.List.Extra.trimStart string
	 in case readsPrec precedence s' of
		[(diagonal, s'')]	-> [(MkDirection $ Right diagonal, s'')]
		_			-> case readsPrec precedence s' of
			[(parallel, s'')]	-> [(MkDirection $ Left parallel, s'')]
			_			-> []	-- No parse.

instance Property.Opposable.Opposable Direction where
	getOpposite (MkDirection d)	= MkDirection $ (Property.Opposable.getOpposite +++ Property.Opposable.getOpposite) d

instance Property.Orientated.Orientated Direction where
	isVertical (MkDirection (Left p))	= Property.Orientated.isVertical p
	isVertical _				= False

	isHorizontal (MkDirection (Left p))	= Property.Orientated.isHorizontal p
	isHorizontal _				= False

	isParallel (MkDirection (Left _))	= True
	isParallel _				= False

	isDiagonal (MkDirection (Right _))	= True
	isDiagonal _				= False

	isStraight				= const True

instance Property.Reflectable.ReflectableOnX Direction where
	reflectOnX (MkDirection d)	= MkDirection $ (Property.Reflectable.reflectOnX +++ Property.Reflectable.reflectOnX) d

instance Property.Reflectable.ReflectableOnY Direction where
	reflectOnY (MkDirection d)	= MkDirection $ (Property.Reflectable.reflectOnY +++ Property.Reflectable.reflectOnY) d

instance Property.FixedMembership.FixedMembership Direction where
	members	= parallels ++ diagonals

instance Bounded Direction where
	minBound	= head Property.FixedMembership.members
	maxBound	= last Property.FixedMembership.members

instance Data.Array.IArray.Ix Direction where
	range _			= Property.FixedMembership.members
	inRange _ _		= True
	index _ (MkDirection d)	= fromEnum ||| (+ fromIntegral Direction.Parallel.nParallels) . fromEnum $ d

-- | Constant directions.
parallels	:: [Direction]
s, n, w, e	:: Direction
parallels@[s, n, w, e]	= map (MkDirection . Left) Property.FixedMembership.members {-parallels-}

-- | Constant directions.
diagonals	:: [Direction]
sw, se, nw, ne	:: Direction
diagonals@[sw, se, nw, ne]	= map (MkDirection . Right) Property.FixedMembership.members

{- |
	* Returns a list of /direction/s, each paired with its anti-parallel.

	* CAVEAT: each /direction/ only appears once in the list, on an arbitrary side of a pair.
-}
opposites :: [(Direction, Direction)]
opposites	= map (id &&& Property.Opposable.getOpposite) [n, ne, e, se]

-- | The /direction/s in which a @Pawn@ can attack.
attackDirectionsForPawn :: Colour.LogicalColour.LogicalColour -> [Direction]
attackDirectionsForPawn Colour.LogicalColour.Black	= [sw, se]
attackDirectionsForPawn _				= [nw, ne]

-- | Whether the two /direction/s specified, are either parallel or anti-parallel.
areAligned :: Direction -> Direction -> Bool
areAligned l	= uncurry (||) . ((== l) &&& (== l) . Property.Opposable.getOpposite)

-- | A boxed array indexed by /direction/, of arbitrary elements.
type ArrayByDirection	= Data.Array.IArray.Array {-Boxed-} Direction

-- | Array-constructor.
listArrayByDirection :: Data.Array.IArray.IArray a e => [e] -> a Direction e
listArrayByDirection	= Data.Array.IArray.listArray (minBound, maxBound)

