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

 [@DESCRIPTION@]	Describes a line's magnitude & direction, irrespective of its position.
-}

module BishBosh.Cartesian.Vector(
-- * Types
-- ** Type-synonyms
	VectorInt,
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
	mkVector,
	measureDistance,
-- ** Predicates
	isDiagonal,
	isParallel,
	isStraight,
	isPawnAttack,
	isKnightsMove,
	isKingsMove,
	matchesPawnDoubleAdvance
) where

import			Control.Arrow((***))
import qualified	BishBosh.Attribute.Direction		as Attribute.Direction
import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Cartesian.Abscissa		as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate		as Cartesian.Ordinate
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	BishBosh.Property.Orientated		as Property.Orientated
import qualified	BishBosh.Types				as T
import qualified	Control.Exception

-- | The distance between two /coordinates/.
data Vector distance = MkVector {
	getXDistance	:: !distance,
	getYDistance	:: !distance
} deriving (Eq, Show)

instance Num distance => Property.Opposable.Opposable (Vector distance) where
	getOpposite (MkVector xDistance yDistance)	= MkVector (negate xDistance) (negate yDistance)

-- | Smart constructor.
mkVector :: (Num distance, Ord distance) => distance -> distance -> Vector distance
{-# INLINE mkVector #-}
mkVector xDistance yDistance	= Control.Exception.assert (
	(
		xDistance /= 0 || yDistance /= 0	-- Which would be neither a valid chess-move nor have a direction.
	) && abs xDistance < fromIntegral Cartesian.Abscissa.xLength && abs yDistance < fromIntegral Cartesian.Ordinate.yLength
 ) $ MkVector xDistance yDistance

-- | Construct a /vector/ by measuring the signed distance between source-/coordinates/ & destination.
measureDistance :: (
	Enum	x,
	Enum	y,
	Num	distance,
	Ord	distance
 )
	=> Cartesian.Coordinates.Coordinates x y	-- ^ Source.
	-> Cartesian.Coordinates.Coordinates x y	-- ^ Destination.
	-> Vector distance
{-# INLINE measureDistance #-}
measureDistance source destination	= uncurry mkVector $ Cartesian.Coordinates.measureDistance source destination

-- | Whether the specified /vector/ is at 45 degrees to an edge of the board, i.e. any move a @Bishop@ could make.
isDiagonal :: (Eq distance, Num distance) => Vector distance -> Bool
{-# INLINE isDiagonal #-}	-- N.B.: highly effective.
isDiagonal (MkVector xDistance yDistance)	= abs xDistance == abs yDistance

-- | Whether the specified /vector/ is parallel to an edge of the board, i.e. any move a @Rook@ could make.
isParallel :: (Eq distance, Num distance) => Vector distance -> Bool
{-# INLINE isParallel #-}
isParallel (MkVector xDistance yDistance)	= xDistance == 0 || yDistance == 0

-- | Whether the specified /vector/ is either parallel or at 45 degrees to an edge of the board, i.e. any move a @Queen@ could make.
isStraight :: (Eq distance, Num distance) => Vector distance -> Bool
{-# INLINE isStraight #-}
isStraight vector	= isParallel vector || isDiagonal vector

-- | A suitable concrete type.
type VectorInt	= Vector T.Distance

instance (Eq distance, Num distance) => Property.Orientated.Orientated (Vector distance) where
	{-# SPECIALISE instance Property.Orientated.Orientated VectorInt #-}
	isDiagonal	= isDiagonal
	isParallel	= isParallel

{- |
	* The list of attack-vectors for a @Pawn@.

	* N.B.: the @Pawn@'s ability to advance without taking, isn't dealt with here.
-}
attackVectorsForPawn :: Num distance => Attribute.LogicalColour.LogicalColour -> [Vector distance]
attackVectorsForPawn logicalColour	= [
	MkVector x $ (
		if Attribute.LogicalColour.isBlack logicalColour
			then negate	-- Black moves down.
			else id		-- White moves up.
	) 1 | x	<- [negate 1, 1]
 ] -- List-comprehension.

-- | The constant list of attack-vectors for a @Knight@.
attackVectorsForKnight :: Num distance => [Vector distance]
attackVectorsForKnight	= [
	MkVector (fX xDistance) (fY $ 3 - xDistance) |
		fX		<- [negate, id],
		fY		<- [negate, id],
		xDistance	<- [1, 2]
 ] -- List-comprehension.

-- | The constant list of attack-vectors for a @King@.
attackVectorsForKing :: (Eq distance, Num distance) => [Vector distance]
attackVectorsForKing	= [
	MkVector xDistance yDistance |
		xDistance	<- [negate 1, 0, 1],
		yDistance	<- [negate 1, 0, 1],
		xDistance /= 0 || yDistance /= 0
 ] -- List-comprehension.

{- |
	* Whether the specified /vector/ might represent an attack (rather than an advance) by a @Pawn@.

	* CAVEAT: if the move started at the first rank, then it can't be a @Pawn@, but that's unknown.
-}
isPawnAttack :: (Eq distance, Num distance) => Attribute.LogicalColour.LogicalColour -> Vector distance -> Bool
{-# INLINE isPawnAttack #-}
isPawnAttack logicalColour (MkVector xDistance yDistance)	= abs xDistance == 1 && yDistance == (
	if Attribute.LogicalColour.isBlack logicalColour
		then negate
		else id
 ) 1

-- | Whether the specified /vector/ represents a move a @Knight@ could make.
isKnightsMove :: (Eq distance, Num distance) => Vector distance -> Bool
{-# INLINE isKnightsMove #-}
isKnightsMove (MkVector xDistance yDistance)	= case abs xDistance of
	1	-> absYDistance == 2
	2	-> absYDistance == 1
	_	-> False
	where
		absYDistance	= abs yDistance

-- | Whether the specified /vector/ represents a move a @King@ could make.
isKingsMove :: (Num distance, Ord distance) => Vector distance -> Bool
isKingsMove (MkVector 0 0)			= False
isKingsMove (MkVector xDistance yDistance)	= abs xDistance <= 1 && abs yDistance <= 1

{- |
	* Whether the specified /vector/ matches a @Pawn@'s initial double-advance move.

	* CAVEAT: passing this test doesn't guarantee that it is a @Pawn@'s double-advance move, since the move may not relate to a @Pawn@, or could be invalid.
-}
matchesPawnDoubleAdvance
	:: (Eq distance, Num distance)
	=> Attribute.LogicalColour.LogicalColour
	-> Vector distance
	-> Bool
matchesPawnDoubleAdvance logicalColour (MkVector xDistance yDistance)	= xDistance == 0 && yDistance == (
	if Attribute.LogicalColour.isBlack logicalColour
		then negate
		else id
 ) 2

-- | Translate the specified /coordinates/ by the specified /vector/.
translate :: (
	Enum		x,
	Enum		y,
	Integral	distance,
	Ord		x,
	Ord		y
 )
	=> Cartesian.Coordinates.Coordinates x y
	-> Vector distance
	-> Cartesian.Coordinates.Coordinates x y
translate coordinates (MkVector xDistance yDistance)	= Cartesian.Coordinates.translate (
	toEnum . (+ fromIntegral xDistance) . fromEnum *** toEnum . (+ fromIntegral yDistance) . fromEnum
 ) coordinates

-- | Where legal, translate the specified /coordinates/ by the specified /vector/.
maybeTranslate :: (
	Enum		x,
	Enum		y,
	Integral	distance,
	Ord		x,
	Ord		y
 )
	=> Cartesian.Coordinates.Coordinates x y
	-> Vector distance
	-> Maybe (Cartesian.Coordinates.Coordinates x y)
{-# INLINE maybeTranslate #-}
maybeTranslate coordinates (MkVector xDistance yDistance)	= Cartesian.Coordinates.maybeTranslate (
	toEnum . (+ fromIntegral xDistance) . fromEnum *** toEnum . (+ fromIntegral yDistance) . fromEnum
 ) coordinates

{- |
	* Where possible, converts the specified /vector/ into a /direction/.

	* @Nothing@ is returned for those /vector/s which don't translate into a legal /direction/ (e.g. a @Knight@'s move).
-}
toMaybeDirection :: (Num distance, Ord distance) => Vector distance -> Maybe Attribute.Direction.Direction
{-# INLINE toMaybeDirection #-}
toMaybeDirection vector@(MkVector xDistance yDistance)
	| isStraight vector	= Just $ Attribute.Direction.mkDirection (compare xDistance 0) (compare yDistance 0)
	| otherwise		= Nothing

