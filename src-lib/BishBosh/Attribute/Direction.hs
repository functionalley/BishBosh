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

	* Describes a /direction/ in two parts; the sense of change in the /x/-coordinate & the sense of change in the /y/-coordinate.

	* The IO-format uses a more concise & familiar format based on 8 points of the compass.

	* CAVEAT: this separation of /direction/ into orthogonal components is driven by the typical use-case,
	but requires that one guards against accidental construction of a degenerate 9th /direction/ which defines a change in neither direction.
-}

module BishBosh.Attribute.Direction(
-- * Types
-- ** Type-synonyms
	NDirections,
	ArrayByDirection,
-- ** Data-types
	Direction(
--		MkDirection,
		getXDirection,
		getYDirection
	),
-- * Constants
	nw,
	n,
	ne,
	w,
	e,
	sw,
	s,
	se,
	tag,
	nDistinctDirections,
	parallels,
	diagonals,
--	range,
	opposites,
-- * Functions
--	reverseOrdering,
	advanceDirection,
	attackDirectionsForPawn,
	listArrayByDirection,
-- ** Constructor
	mkDirection,
-- ** Predicates
	areAligned
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Data.Exception			as Data.Exception
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	BishBosh.Property.Orientated		as Property.Orientated
import qualified	BishBosh.Property.Reflectable		as Property.Reflectable
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.List.Extra
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT
import qualified	Text.XML.HXT.Arrow.Pickle.Schema

-- | Used to qualify XML.
tag :: String
tag	= "direction"

-- | Constant direction.
nw :: Direction
nw	= MkDirection LT GT

-- | Constant direction.
n :: Direction
n	= MkDirection EQ GT

-- | Constant direction.
ne :: Direction
ne	= MkDirection GT GT

-- | Constant direction.
w :: Direction
w	= MkDirection LT EQ

-- | Constant direction.
e :: Direction
e	= MkDirection GT EQ

-- | Constant direction.
sw :: Direction
sw	= MkDirection LT LT

-- | Constant direction.
s :: Direction
s	= MkDirection EQ LT

-- | Constant direction.
se :: Direction
se	= MkDirection GT LT

-- | A number of /direction/s.
type NDirections	= Int	-- N.B.: 'Data.Int.Int8' saves neither time nor space.

-- | Define a /direction/ by the sense of change to /x/ & /y/ coordinates.
data Direction	= MkDirection {
	getXDirection	:: Ordering,	-- ^ The sense of the change in the /x/-coordinate.
	getYDirection	:: Ordering	-- ^ The sense of the change in the /y/-coordinate.
} deriving (Eq, Ord)

instance Bounded Direction where
	minBound	= sw
	maxBound	= ne

instance Control.DeepSeq.NFData Direction where
	rnf MkDirection {
		getXDirection	= xDirection,
		getYDirection	= yDirection
	} = xDirection `seq` yDirection `seq` ()

instance Show Direction where
	showsPrec _ MkDirection {
		getXDirection	= xDirection,
		getYDirection	= yDirection
	} = (
		case yDirection of
			LT	-> showChar 'S'
			EQ	-> id
			GT	-> showChar 'N'
	 ) . (
		case xDirection of
			LT	-> showChar 'W'
			EQ	-> id
			GT	-> showChar 'E'
	 )

instance Read Direction where
	readsPrec _ ss	= let
		s'	= Data.List.Extra.trimStart ss
	 in case Data.List.Extra.upper s' of
		'S' : remainder	-> case remainder of
			'W' : _	-> [(sw, drop 2 s')]
			'E' : _	-> [(se, drop 2 s')]
			_	-> [(s, tail s')]
		'N' : remainder	-> case remainder of
			'W' : _	-> [(nw, drop 2 s')]
			'E' : _	-> [(ne, drop 2 s')]
			_	-> [(n, tail s')]
		'W' : _	-> [(w, tail s')]
		'E' : _	-> [(e, tail s')]
		_	-> []	-- No parse.

-- | Get the opposite.
reverseOrdering :: Ordering -> Ordering
reverseOrdering LT	= GT
reverseOrdering GT	= LT
reverseOrdering _	= EQ

instance Property.Opposable.Opposable Direction where
	getOpposite MkDirection {
		getXDirection	= xDirection,
		getYDirection	= yDirection
	} = MkDirection {
		getXDirection	= reverseOrdering xDirection,
		getYDirection	= reverseOrdering yDirection
	}

instance Property.Orientated.Orientated Direction where
	isDiagonal MkDirection { getXDirection = xDirection, getYDirection = yDirection }	= xDirection /= EQ && yDirection /= EQ
	isParallel MkDirection { getXDirection = xDirection, getYDirection = yDirection }	= xDirection == EQ || yDirection == EQ
	isStraight										= const True

instance Property.Reflectable.ReflectableOnX Direction where
	reflectOnX direction@MkDirection { getYDirection = yDirection }	= direction {
		getYDirection	= reverseOrdering yDirection
	}

instance Property.Reflectable.ReflectableOnY Direction where
	reflectOnY direction@MkDirection { getXDirection = xDirection }	= direction {
		getXDirection	= reverseOrdering xDirection
	}

instance HXT.XmlPickler Direction where
	xpickle	= HXT.xpWrap (read, show) . HXT.xpAttr tag . HXT.xpTextDT . Text.XML.HXT.Arrow.Pickle.Schema.scEnum $ map show range

instance Data.Array.IArray.Ix Direction where
	range (lower, upper)						= Control.Exception.assert (lower == minBound && upper == maxBound) Property.FixedMembership.members
	inRange (lower, upper) _					= Control.Exception.assert (lower == minBound && upper == maxBound) True
	index (lower, upper) (MkDirection xDirection yDirection)	= Control.Exception.assert (lower == minBound && upper == maxBound) $ case xDirection of
		LT	-> case yDirection of
			LT	-> 0
			EQ	-> 1
			GT	-> 2
		EQ	-> case yDirection of
			LT	-> 3
			EQ	-> Control.Exception.throw $ Data.Exception.mkResultUndefined "BishBosh.Attribute.Direction.index:\tundefined direction."
			GT	-> 4
		GT	-> case yDirection of
			LT	-> 5
			EQ	-> 6
			GT	-> 7

-- | The ordered /direction/s in which /royalty/ can move.
range :: [Direction]
range	= [sw, w, nw, s, n, se, e, ne]

instance Property.FixedMembership.FixedMembership Direction where
	members	= range

-- | Smart-constructor.
mkDirection
	:: Ordering	-- ^ The sense of the change in the /x/-coordinate.
	-> Ordering	-- ^ The sense of the change in the /y/-coordinate.
	-> Direction
mkDirection EQ EQ			= Control.Exception.throw $ Data.Exception.mkResultUndefined "BishBosh.Attribute.Direction.mkDirection:\till-defined."
mkDirection xDirection yDirection	= MkDirection xDirection yDirection

-- | The ordered /direction/s in which a @Rook@ can move.
parallels :: [Direction]
parallels	= [w, s, n, e]

-- | The ordered /direction/s in which a @Bishop@ can move.
diagonals :: [Direction]
diagonals	= [sw, nw, se, ne]

-- | The constant number of distinct /direction/s.
nDistinctDirections :: NDirections
nDistinctDirections	= length range

{- |
	* Returns a list of /direction/s, each paired with its anti-parallel.

	* CAVEAT: each /direction/ only appears once in the list, on an arbitrary side of a pair.
-}
opposites :: [(Direction, Direction)]
opposites	= map (id &&& Property.Opposable.getOpposite) [sw, w, nw, s]

-- | The /y/-direction in which a @Pawn@ of the specified /logical colour/ advances.
advanceDirection :: Attribute.LogicalColour.LogicalColour -> Ordering
advanceDirection Attribute.LogicalColour.Black	= LT	-- Black moves down.
advanceDirection _				= GT	-- White moves up.

-- | The /direction/s in which a @Pawn@ can attack.
attackDirectionsForPawn :: Attribute.LogicalColour.LogicalColour -> [Direction]
attackDirectionsForPawn logicalColour	= map (`MkDirection` advanceDirection logicalColour) [LT, GT]

-- | Whether the two /direction/s specified, are either parallel or anti-parallel.
areAligned :: Direction -> Direction -> Bool
areAligned direction	= uncurry (||) . ((== direction) &&& (== Property.Opposable.getOpposite direction))

-- | A boxed array indexed by /direction/, of arbitrary elements.
type ArrayByDirection	= Data.Array.IArray.Array {-Boxed-} Direction

-- | Array-constructor.
listArrayByDirection :: Data.Array.IArray.IArray a e => [e] -> a Direction e
listArrayByDirection	= Data.Array.IArray.listArray (minBound, maxBound)

