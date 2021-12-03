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

 [@DESCRIPTION@]	Describes a line's magnitude & direction, irrespective of its position; cf. 'Component.Move.Move'.
-}

module BishBosh.Cartesian.Vector(
-- ** Data-types
	Vector(
--		MkVector,
		getXDistance,
		getYDistance
	),
-- * Constants
	attackVectorsForKnight,
	attackVectorsForKing,
-- * Functions
	attackVectorsForPawn,
	translate,
	maybeTranslate,
	toMaybeDirection,
-- ** Constructor
	measureDistance,
-- ** Predicates
--	hasDistance,
	isPawnAttack,
	isKnightsMove,
	isKingsMove,
	matchesPawnDoubleAdvance
) where

import			Control.Arrow((***))
import qualified	BishBosh.Cartesian.Coordinates	as Cartesian.Coordinates
import qualified	BishBosh.Colour.LogicalColour	as Colour.LogicalColour
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Direction.Direction	as Direction.Direction
import qualified	BishBosh.Property.Opposable	as Property.Opposable
import qualified	BishBosh.Property.Orientated	as Property.Orientated
import qualified	BishBosh.Text.ShowList		as Text.ShowList
import qualified	BishBosh.Type.Length		as Type.Length
import qualified	Control.DeepSeq
import qualified	Control.Exception

-- | The distance between two /coordinates/.
data Vector	= MkVector {
	getXDistance	:: ! Type.Length.X,
	getYDistance	:: ! Type.Length.Y
} deriving (Eq, Show)

instance Control.DeepSeq.NFData Vector where
	rnf MkVector {
		getXDistance	= xDistance,
		getYDistance	= yDistance
	} = Control.DeepSeq.rnf (xDistance, yDistance)

instance Property.Opposable.Opposable Vector where
	getOpposite MkVector {
		getXDistance	= xDistance,
		getYDistance	= yDistance
	} = MkVector (negate xDistance) (negate yDistance)

instance Property.Orientated.Orientated Vector where
	isVertical MkVector { getXDistance = xDistance }	= xDistance == 0

	isHorizontal MkVector { getYDistance = yDistance }	= yDistance == 0

	isParallel MkVector { getXDistance = 0 }	= True
	isParallel MkVector { getYDistance = 0 }	= True
	isParallel _					= False

	isDiagonal MkVector {
		getXDistance	= xDistance,
		getYDistance	= yDistance
	} = abs xDistance == fromIntegral (abs yDistance)

-- | Whether the vector has a non-zero length (or a well-defined direction).
hasDistance :: Type.Length.X -> Type.Length.Y -> Bool
hasDistance 0 0	= False
hasDistance _ _	= True

-- | Construct a /vector/ by measuring the signed distance between source-/coordinates/ & destination.
measureDistance
	:: Cartesian.Coordinates.Coordinates	-- ^ Source.
	-> Cartesian.Coordinates.Coordinates	-- ^ Destination.
	-> Vector
measureDistance source destination	= uncurry MkVector $ Cartesian.Coordinates.measureDistance source destination

{- |
	* The list of attack-vectors for a @Pawn@.

	* N.B.: the @Pawn@'s ability to advance without taking, isn't dealt with here.
-}
attackVectorsForPawn :: Colour.LogicalColour.LogicalColour -> [Vector]
attackVectorsForPawn logicalColour	= [
	MkVector {
		getXDistance	= x,
		getYDistance	= (
			if Colour.LogicalColour.isBlack logicalColour
				then negate	-- Black moves down.
				else id		-- White moves up.
		) 1
	} | x	<- [negate 1, 1]
 ] -- List-comprehension.

-- | The constant list of attack-vectors for a @Knight@.
attackVectorsForKnight :: [Vector]
attackVectorsForKnight	= [
	MkVector {
		getXDistance	= fX xDistance,
		getYDistance	= fY $ 3 - fromIntegral xDistance
	} |
		fX		<- [negate, id],
		fY		<- [negate, id],
		xDistance	<- [1, 2]
 ]

-- | The constant list of attack-vectors for a @King@.
attackVectorsForKing :: [Vector]
attackVectorsForKing	= [
	MkVector xDistance yDistance |
		xDistance	<- [negate 1, 0, 1],
		yDistance	<- [negate 1, 0, 1],
		hasDistance xDistance yDistance
 ]

{- |
	* Whether the specified /vector/ might represent an attack (rather than an advance) by a @Pawn@.

	* CAVEAT: if the move started at the first rank, then it can't be a @Pawn@, but that's beyond the scope of this module (since a /Vector/ doesn't define absolute /coordinate/s).
-}
isPawnAttack :: Vector -> Colour.LogicalColour.LogicalColour -> Bool
{-# INLINE isPawnAttack #-}
isPawnAttack MkVector {
	getXDistance	= xDistance,
	getYDistance	= yDistance
} logicalColour = abs xDistance == 1 && yDistance == if Colour.LogicalColour.isBlack logicalColour
	then negate 1
	else 1

-- | Whether the specified /vector/ represents a move a @Knight@ could make.
isKnightsMove :: Vector -> Bool
{-# INLINE isKnightsMove #-}
isKnightsMove MkVector {
	getXDistance	= xDistance,
	getYDistance	= yDistance
} = case abs xDistance of
	1	-> absYDistance == 2
	2	-> absYDistance == 1
	_	-> False
	where
		absYDistance	= abs yDistance

-- | Whether the specified /vector/ represents a move a @King@ could make.
isKingsMove :: Vector -> Bool
isKingsMove MkVector {
	getXDistance	= xDistance,
	getYDistance	= yDistance
} = abs xDistance <= 1 && abs yDistance <= 1

{- |
	* Whether the specified /vector/ matches a @Pawn@'s initial double-advance move.

	* CAVEAT: passing this test doesn't guarantee that it is a @Pawn@'s double-advance move, since the move may not relate to a @Pawn@, or could be invalid.
-}
matchesPawnDoubleAdvance :: Vector -> Colour.LogicalColour.LogicalColour -> Bool
matchesPawnDoubleAdvance MkVector {
	getXDistance	= 0,
	getYDistance	= yDistance
} logicalColour			= yDistance == if Colour.LogicalColour.isBlack logicalColour then negate 2 else 2
matchesPawnDoubleAdvance _ _	= False

-- | Translate the specified /coordinates/ by the specified /vector/.
translate :: Vector -> Cartesian.Coordinates.Coordinates -> Cartesian.Coordinates.Coordinates
translate MkVector {
	getXDistance	= xDistance,
	getYDistance	= yDistance
} = Cartesian.Coordinates.translate $ (+ xDistance) *** (+ yDistance)

-- | Where legal, translate the specified /coordinates/ by the specified /vector/.
maybeTranslate :: Vector -> Cartesian.Coordinates.Coordinates -> Maybe Cartesian.Coordinates.Coordinates
maybeTranslate MkVector {
	getXDistance	= xDistance,
	getYDistance	= yDistance
} = Cartesian.Coordinates.maybeTranslate $ (+ xDistance) *** (+ yDistance)

{- |
	* Where possible, converts the specified /vector/ into a /direction/.

	* @Nothing@ is returned for those /vector/s which don't translate into a legal /direction/ (e.g. a @Knight@'s move).
-}
toMaybeDirection :: Vector -> Maybe Direction.Direction.Direction
toMaybeDirection vector@(MkVector xDistance yDistance)	= case (xDistance `compare` 0, yDistance `compare` 0) of
	(LT, ySense)	-> case ySense of
		LT
			| xDistance == yDistance'		-> Just Direction.Direction.sw
			| otherwise				-> Nothing
		EQ						-> Just Direction.Direction.w
		GT
			| negate xDistance == yDistance'	-> Just Direction.Direction.nw
			| otherwise				-> Nothing
	(EQ, ySense)	-> Just $ case ySense of
		LT	-> Direction.Direction.s
		EQ	-> Control.Exception.throw . Data.Exception.mkRequestFailure . showString "BishBosh.Cartesian.Vector.toMaybeDirection:\tundefined direction" . Text.ShowList.showsAssociation $ shows vector "."
		GT	-> Direction.Direction.n
	(GT, ySense)	-> case ySense of
		LT
			| xDistance == negate yDistance'	-> Just Direction.Direction.se
			| otherwise				-> Nothing
		EQ						-> Just Direction.Direction.e
		GT
			| xDistance == yDistance'		-> Just Direction.Direction.ne
			| otherwise				-> Nothing
	where
		yDistance'	= fromIntegral yDistance

