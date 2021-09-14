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

 [@DESCRIPTION@]	Defines one move (actually just a half move AKA "ply") of a /piece/.
-}

module BishBosh.Component.Move(
-- * Types
-- ** Type-synonyms
	Move(
--		MkMove,
		getSource,
		getDestination
	),
-- * Constants
	tag,
	nPliesPerMove,
-- * Functions
	measureDistance,
	interpolate,
-- ** Constructors
	mkMove,
-- ** Predicates
	isPawnDoubleAdvance
) where

import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Vector		as Cartesian.Vector
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	BishBosh.Property.Orientated		as Property.Orientated
import qualified	BishBosh.Property.Reflectable		as Property.Reflectable
import qualified	BishBosh.Types				as T
import qualified	BishBosh.Type.Count			as Type.Count
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Ord

-- | Used to qualify XML.
tag :: String
tag	= "move"

-- | The constant number of plies per move.
nPliesPerMove :: Type.Count.NPlies
nPliesPerMove	= 2

{- |
	* A move of a /piece/.

	* Most modern chess-notations (except Standard Algebraic) start with similar information, but also define ancillary information which is captured in /MoveType/.
-}
data Move x y	= MkMove {
	getSource	:: Cartesian.Coordinates.Coordinates x y,
	getDestination	:: Cartesian.Coordinates.Coordinates x y
} deriving Eq

instance (Ord x, Ord y) => Ord (Move x y) where
	{-# SPECIALISE instance Ord (Move T.X T.Y) #-}
	move@MkMove { getSource = source } `compare` move'@MkMove { getSource = source' }	= case source `compare` source' of
		EQ		-> Data.Ord.comparing getDestination move move'
		ordering	-> ordering

instance (Control.DeepSeq.NFData x, Control.DeepSeq.NFData y) => Control.DeepSeq.NFData (Move x y) where
	rnf MkMove {
		getSource	= source,
		getDestination	= destination
	} = Control.DeepSeq.rnf (source, destination)

instance (Show x, Show y) => Show (Move x y) where
	showsPrec _ MkMove {
		getSource	= source,
		getDestination	= destination
	} = shows (source, destination)

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Read	x,
	Read	y
 ) => Read (Move x y) where
	readsPrec _	= map (Control.Arrow.first $ uncurry mkMove) . reads

instance Property.Opposable.Opposable (Move x y) where
	getOpposite (MkMove source destination)	= MkMove {
		getSource	= destination,
		getDestination	= source
	}

instance (Enum x, Enum y) => Property.Orientated.Orientated (Move x y) where
	isDiagonal	= (Property.Orientated.isDiagonal :: Cartesian.Vector.VectorInt -> Bool) . measureDistance
	isParallel	= (Property.Orientated.isParallel :: Cartesian.Vector.VectorInt -> Bool) . measureDistance

instance Enum y => Property.Reflectable.ReflectableOnX (Move x y) where
	reflectOnX MkMove {
		getSource	= source,
		getDestination	= destination
	} = MkMove {
		getSource	= Property.Reflectable.reflectOnX source,
		getDestination	= Property.Reflectable.reflectOnX destination
	}

instance Enum x => Property.Reflectable.ReflectableOnY (Move x y) where
	reflectOnY MkMove {
		getSource	= source,
		getDestination	= destination
	} = MkMove {
		getSource	= Property.Reflectable.reflectOnY source,
		getDestination	= Property.Reflectable.reflectOnY destination
	}

-- | Smart constructor.
mkMove
	:: (Eq x, Eq y)
	=> Cartesian.Coordinates.Coordinates x y
	-> Cartesian.Coordinates.Coordinates x y
	-> Move x y
{-# INLINE mkMove #-}
mkMove source destination	= Control.Exception.assert (source /= destination) MkMove {
	getSource	= source,
	getDestination	= destination
}

-- | Measures the signed distance between the ends of the move.
measureDistance :: (
	Enum	x,
	Enum	y,
	Num	distance,
	Ord	distance
 ) => Move x y -> Cartesian.Vector.Vector distance
{-# SPECIALISE measureDistance :: Move T.X T.Y -> Cartesian.Vector.VectorInt #-}
measureDistance	MkMove {
	getSource	= source,
	getDestination	= destination
} = Cartesian.Vector.measureDistance source destination

-- | Generates a line of /coordinates/ covering the half open interval @(source, destination]@.
interpolate :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => Move x y -> [Cartesian.Coordinates.Coordinates x y]
{-# SPECIALISE interpolate :: Move T.X T.Y -> [Cartesian.Coordinates.Coordinates T.X T.Y] #-}
interpolate move@MkMove {
	getSource	= source,
	getDestination	= destination
} = Control.Exception.assert (Property.Orientated.isStraight move) $ Cartesian.Coordinates.interpolate source destination

{- |
	* Whether the specified /move/ is a @Pawn@'s initial double-advance.

	* CAVEAT: failing this test guarantees that the move isn't a @Pawn@'s double-advance,
	but passing only guarantees that it is, if it was a @Pawn@ which moved & that the /move/ is valid.
-}
isPawnDoubleAdvance
	:: (Enum x, Enum y, Eq y)
	=> Attribute.LogicalColour.LogicalColour	-- Defines the side whose move is referenced.
	-> Move x y
	-> Bool
isPawnDoubleAdvance logicalColour move	= Cartesian.Coordinates.isPawnsFirstRank logicalColour (
	getSource move
 ) && Cartesian.Vector.matchesPawnDoubleAdvance logicalColour (
	measureDistance move :: Cartesian.Vector.VectorInt
 )

