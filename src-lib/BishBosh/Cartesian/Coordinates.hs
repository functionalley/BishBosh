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

 [@DESCRIPTION@]	The location of a square on the board.
-}

module BishBosh.Cartesian.Coordinates(
-- * Types
-- ** Data-types
	Coordinates(
--		MkCoordinates,
		getX,
		getY
	),
-- ** Type-synonyms
--	Transformation,
	ByCoordinates,
-- * Constants
	topLeft,
	bottomRight,
	nSquares,
--	range,
--	extrapolationsByCoordinatesByDirection,
--	interpolationsBySourceByDestination,
	radiusSquared,
-- * Functions
--	(>||<),
	extrapolate,
--	extrapolateInt,
	interpolate,
--	interpolateInt,
	range,
	getLogicalColourOfSquare,
	kingsStartingCoordinates,
	rooksStartingCoordinates,
	measureDistance,
	translate,
	maybeTranslate,
	translateX,
	maybeTranslateX,
	translateY,
	maybeTranslateY,
	getAdjacents,
	advance,
--	maybeAdvance,
	retreat,
	maybeRetreat,
--	rotate,
-- ** Constructors
	mkCoordinates,
	mkMaybeCoordinates,
	fromIx,
	mkRelativeCoordinates,
	listArrayByCoordinates,
-- ** Predicates
--	inBounds,
	isPawnsFirstRank,
	isEnPassantRank,
	areSquaresIsochromatic
) where

import			Control.Arrow((&&&))
import			Data.Array.IArray((!))
import qualified	BishBosh.Attribute.Direction			as Attribute.Direction
import qualified	BishBosh.Attribute.LogicalColour		as Attribute.LogicalColour
import qualified	BishBosh.Attribute.LogicalColourOfSquare	as Attribute.LogicalColourOfSquare
import qualified	BishBosh.Cartesian.Abscissa			as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Ordinate			as Cartesian.Ordinate
import qualified	BishBosh.Data.Exception				as Data.Exception
import qualified	BishBosh.Property.Opposable			as Property.Opposable
import qualified	BishBosh.Property.Reflectable			as Property.Reflectable
import qualified	BishBosh.Property.Rotatable			as Property.Rotatable
import qualified	BishBosh.Text.ShowList				as Text.ShowList
import qualified	BishBosh.Types					as T
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.Maybe
import qualified	Factory.Math.Power

-- | The /coordinates/ of a square on the board.
data Coordinates x y	= MkCoordinates {
	getX	:: x,	-- ^ Abscissa.
	getY	:: y	-- ^ Ordinate.
} deriving Eq

instance (
	Control.DeepSeq.NFData	x,
	Control.DeepSeq.NFData	y
 ) => Control.DeepSeq.NFData (Coordinates x y) where
	rnf MkCoordinates { getX = x, getY = y }	= Control.DeepSeq.rnf (x, y)

instance (Show x, Show y) => Show (Coordinates x y) where
	showsPrec _ MkCoordinates { getX = x, getY = y }	= shows (x, y)

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Read	x,
	Read	y
 ) => Read (Coordinates x y) where
	readsPrec _ s	= [
		(coordinates, remainder) |
			((x, y), remainder)	<- reads s,
			coordinates		<- Data.Maybe.maybeToList $ mkMaybeCoordinates x y
	 ] -- List-comprehension.

instance (Ord x, Ord y) => Ord (Coordinates x y) where
	{-# SPECIALISE instance Ord (Coordinates T.X T.Y) #-}
	MkCoordinates { getX = x, getY = y } `compare` MkCoordinates { getX = x', getY = y' }	= (y, x) `compare` (y', x')	-- N.B.: x is less significant than y, as required by the implementation of 'Data.Array.IArray.Ix.inRange'.

instance (Enum x, Enum y) => Bounded (Coordinates x y) where
	minBound = MkCoordinates {
		getX	= Cartesian.Abscissa.xMin,
		getY	= Cartesian.Ordinate.yMin
	} -- Bottom Left.
	maxBound = MkCoordinates {
		getX	= Cartesian.Abscissa.xMax,
		getY	= Cartesian.Ordinate.yMax
	} -- Top Right.

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => Data.Array.IArray.Ix (Coordinates x y) where
	{-# SPECIALISE instance Data.Array.IArray.Ix (Coordinates T.X T.Y) #-}
	range (lower, upper)			= Control.Exception.assert (lower == minBound && upper == maxBound) range
	inRange (lower, upper) coordinates	= Control.Exception.assert (coordinates >= lower && coordinates <= upper) True
	index (lower, upper) MkCoordinates {
		getX	= x,
		getY	= y
	} = Control.Exception.assert (
		lower == minBound && upper == maxBound
	 ) $ fromIntegral Cartesian.Abscissa.xLength * (
		fromEnum y - Cartesian.Ordinate.yOrigin
	 ) + (
		fromEnum x - Cartesian.Abscissa.xOrigin
	 )

instance Enum y => Property.Reflectable.ReflectableOnX (Coordinates x y) where
	reflectOnX coordinates@MkCoordinates { getY = y }	= coordinates { getY = Cartesian.Ordinate.reflect y }

instance Enum x => Property.Reflectable.ReflectableOnY (Coordinates x y) where
	reflectOnY coordinates@MkCoordinates { getX = x }	= coordinates { getX = Cartesian.Abscissa.reflect x }

instance (Enum x, Enum y) => Property.Rotatable.Rotatable (Coordinates x y) where
	rotate90	= rotate Attribute.Direction.w
	rotate180	= rotate Attribute.Direction.s
	rotate270	= rotate Attribute.Direction.e

-- | Constant.
topLeft :: (Enum x, Enum y) => Coordinates x y
topLeft = MkCoordinates {
	getX	= Cartesian.Abscissa.xMin,
	getY	= Cartesian.Ordinate.yMax
}

-- | Constant.
bottomRight :: (Enum x, Enum y) => Coordinates x y
bottomRight = MkCoordinates {
	getX	= Cartesian.Abscissa.xMax,
	getY	= Cartesian.Ordinate.yMin
}

-- | The constant number of squares on the board.
nSquares :: Int
nSquares	= fromIntegral $ Cartesian.Abscissa.xLength * Cartesian.Ordinate.yLength

-- | Generates a raster over all the board's /coordinates/.
range :: (Enum x, Enum y) => [Coordinates x y]
{-# SPECIALISE range :: [Coordinates T.X T.Y] #-}
range	= [
	MkCoordinates {
		getX	= x,
		getY	= y
	} |
		y	<- Cartesian.Ordinate.yRange,
		x	<- Cartesian.Abscissa.xRange
 ] -- List-comprehension.

-- | Predicate.
inBounds :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 )
	=> x
	-> y
	-> Bool
{-# INLINE inBounds #-}
inBounds x y	= Cartesian.Abscissa.inBounds x && Cartesian.Ordinate.inBounds y

-- | Constructor.
mkCoordinates :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 )
	=> x	-- ^ Abscissa.
	-> y	-- ^ Ordinate.
	-> Coordinates x y
mkCoordinates x y	= Control.Exception.assert (inBounds x y) $ MkCoordinates x y

-- | Safe constructor.
mkMaybeCoordinates :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 )
	=> x	-- ^ Abscissa.
	-> y	-- ^ Ordinate.
	-> Maybe (Coordinates x y)
mkMaybeCoordinates x y
	| inBounds x y	= Just MkCoordinates { getX = x, getY = y }
	| otherwise	= Nothing

{- |
	* Construct from the specified array-index.

	* CAVEAT: assumes that the array is indexed by the whole range of /coordinates/.
-}
fromIx :: (Enum x, Enum y) => Int -> Coordinates x y
fromIx	= (
	\(y, x) -> MkCoordinates {
		getX	= toEnum $ x + Cartesian.Abscissa.xOrigin,
		getY	= toEnum $ y + Cartesian.Ordinate.yOrigin
	}
 ) . (`divMod` fromIntegral Cartesian.Abscissa.xLength)

-- | Translate the specified /coordinates/.
translate :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => ((x, y) -> (x, y)) -> Coordinates x y -> Coordinates x y
translate transformation MkCoordinates {
	getX	= x,
	getY	= y
} = uncurry mkCoordinates $ transformation (x, y)

-- | Where legal, translate the specified /coordinates/.
maybeTranslate :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 )
	=> ((x, y) -> (x, y))	-- ^ Translation.
	-> Coordinates x y
	-> Maybe (Coordinates x y)
{-# INLINE maybeTranslate #-}
maybeTranslate transformation MkCoordinates {
	getX	= x,
	getY	= y
} = uncurry mkMaybeCoordinates $ transformation (x, y)

-- | Translate the specified abscissa.
translateX :: (Enum x, Ord x) => (x -> x) -> Transformation x y
translateX transformation coordinates@MkCoordinates { getX = x }	= coordinates { getX = Cartesian.Abscissa.translate transformation x }

-- | Where legal, translate the /x/-component of the specified /coordinates/.
maybeTranslateX
	:: (Enum x, Ord x)
	=> (x -> x)	-- ^ Translation.
	-> Coordinates x y
	-> Maybe (Coordinates x y)
maybeTranslateX transformation coordinates@MkCoordinates { getX = x }	= (\x' -> coordinates { getX = x' }) `fmap` Cartesian.Abscissa.maybeTranslate transformation x

-- | Translate the specified ordinate.
translateY :: (Enum y, Ord y) => (y -> y) -> Transformation x y
translateY transformation coordinates@MkCoordinates { getY = y }	= coordinates { getY = Cartesian.Ordinate.translate transformation y }

-- | Where legal, translate the /y/-component of the specified /coordinates/.
maybeTranslateY
	:: (Enum y, Ord y)
	=> (y -> y)	-- ^ Translation.
	-> Coordinates x y
	-> Maybe (Coordinates x y)
maybeTranslateY transformation coordinates@MkCoordinates { getY = y }	= (\y' -> coordinates { getY = y' }) `fmap` Cartesian.Ordinate.maybeTranslate transformation y

-- | Construct /coordinates/ relative to 'minBound'.
mkRelativeCoordinates :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 )
	=> ((x, y) -> (x, y))	-- ^ Translation.
	-> Coordinates x y
mkRelativeCoordinates	= (`translate` minBound)

-- | Move one step towards the opponent.
advance
	:: (Enum y, Ord	y)
	=> Attribute.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the /piece/ which is to advance.
	-> Transformation x y
{-# INLINE advance #-}
advance logicalColour	= translateY $ if Attribute.LogicalColour.isBlack logicalColour
	then pred
	else succ

-- | Where legal, move one step towards the opponent.
maybeAdvance
	:: (Enum y, Ord	y)
	=> Attribute.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the /piece/ which is to advance.
	-> Coordinates x y				-- ^ The location from which to advanced.
	-> Maybe (Coordinates x y)
maybeAdvance logicalColour	= maybeTranslateY $ if Attribute.LogicalColour.isBlack logicalColour
	then pred
	else succ

-- | Move one step away from the opponent.
retreat
	:: (Enum y, Ord	y)
	=> Attribute.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the /piece/ which is to retreat.
	-> Transformation x y
retreat	= advance . Property.Opposable.getOpposite

-- | Where legal, move one step away from the opponent.
maybeRetreat
	:: (Enum y, Ord y)
	=> Attribute.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the /piece/ which is to retreat.
	-> Coordinates x y				-- ^ The location from which to retreat.
	-> Maybe (Coordinates x y)
maybeRetreat	= maybeAdvance . Property.Opposable.getOpposite

-- | Get the /coordinates/ immediately left & right.
getAdjacents :: (Enum x, Eq x) => Coordinates x y -> [Coordinates x y]
getAdjacents coordinates@MkCoordinates { getX = x }	= map (\x' -> coordinates { getX = x' }) $ Cartesian.Abscissa.getAdjacents x

infix 6 >||<	-- Just greater than (:).

-- | Alternative to @ zipWith $ curry MkCoordinates @.
(>||<) :: [x] -> [y] -> [Coordinates x y]
{-# INLINE (>||<) #-}
(x' : xs) >||< (y' : ys)	= MkCoordinates { getX = x', getY = y' } : xs >||< ys	-- Recurse.
_ >||< _			= []	-- To avoid unnecessary evaluation, 'zipWith' encodes two patterns for this, but is slightly slower.

{- |
	* Generates a line of /coordinates/, starting just after the specified source & proceeding in the specified /direction/ to the edge of the board.

	* CAVEAT: this is a performance-hotspot (it's also responsible for the allocation of a third of the application's memory); refactor => re-profile.
	In consequence, it is typically automatically avoided using a rewrite-rule to lookup an array of the results from all possible calls.
-}
extrapolate
	:: (Enum x, Enum y)
	=> Attribute.Direction.Direction	-- ^ The direction in which to proceed.
	-> Coordinates x y			-- ^ The point from which to start.
	-> [Coordinates x y]
{-# NOINLINE extrapolate #-}	-- Ensure the rewrite-rule triggers.
{-# RULES "extrapolate/Int" extrapolate = extrapolateInt #-}	-- CAVEAT: the call-stack leading here must be specialised to ensure this rule triggers.
extrapolate direction MkCoordinates {
	getX	= x,
	getY	= y
} = (
	case Attribute.Direction.getXDirection direction of
		GT	-> [succ x .. Cartesian.Abscissa.xMax]
		LT	-> let startX = pred x in startX `seq` [startX, pred startX .. Cartesian.Abscissa.xMin]
		EQ	-> repeat x
 ) >||< (
	case Attribute.Direction.getYDirection direction of
		GT	-> [succ y .. Cartesian.Ordinate.yMax]
		LT	-> let startY = pred y in startY `seq` [startY, pred startY .. Cartesian.Ordinate.yMin]
		EQ	-> repeat y
 )

-- | A specialisation of 'extrapolate'.
extrapolateInt :: Attribute.Direction.Direction -> Coordinates T.X T.Y -> [Coordinates T.X T.Y]
extrapolateInt direction coordinates	= extrapolationsByCoordinatesByDirection ! coordinates ! direction

-- | The constant lists of /coordinates/, extrapolated from every /coordinate/ in the /board/, in every /direction/.
extrapolationsByCoordinatesByDirection :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => ByCoordinates x y (Attribute.Direction.ByDirection [Coordinates x y])
{-# SPECIALISE extrapolationsByCoordinatesByDirection :: ByCoordinates T.X T.Y (Attribute.Direction.ByDirection [Coordinates T.X T.Y]) #-}	-- To promote memoisation.
extrapolationsByCoordinatesByDirection	= listArrayByCoordinates [
	Attribute.Direction.listArrayByDirection [
		(
			case Attribute.Direction.getXDirection direction of
				GT	-> [succ x .. Cartesian.Abscissa.xMax]
				LT	-> let startX = pred x in startX `seq` [startX, pred startX .. Cartesian.Abscissa.xMin]
				EQ	-> repeat x
		) >||< (
			case Attribute.Direction.getYDirection direction of
				GT	-> [succ y .. Cartesian.Ordinate.yMax]
				LT	-> let startY = pred y in startY `seq` [startY, pred startY .. Cartesian.Ordinate.yMin]
				EQ	-> repeat y
		) | direction	<- Attribute.Direction.range
	] | MkCoordinates { getX = x, getY = y }	<- range
 ] -- List-comprehension.

{- |
	* Generates a line of /coordinates/ covering the half open interval @(source, destination]@.

	* CAVEAT: the destination-/coordinates/ must be a valid @Queen@'s /move/ from the source; so that all intermediate points lie on a square of the board.
-}
interpolate :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 )
	=> Coordinates x y	-- ^ Source.
	-> Coordinates x y	-- ^ Destination.
	-> [Coordinates x y]
{-# NOINLINE interpolate #-}	-- Ensure the rewrite-rule triggers.
{-# RULES "interpolate/Int" interpolate = interpolateInt #-}	-- CAVEAT: the call-stack leading here must be specialised to ensure this rule triggers.
interpolate source@MkCoordinates {
	getX	= x,
	getY	= y
} destination@MkCoordinates {
	getX	= x',
	getY	= y'
}
	| source == destination	= []	-- CAVEAT: an invalid move.
	| otherwise		= (
		case x' `compare` x of
			GT	-> [succ x .. x']
			LT	-> let startX = pred x in startX `seq` [startX, pred startX .. x']
			EQ	-> repeat x
	) >||< (
		case y' `compare` y of
			GT	-> [succ y .. y']
			LT	-> let startY = pred y in startY `seq` [startY, pred startY .. y']
			EQ	-> repeat y
	)

-- | A specialisation of 'interpolate'.
interpolateInt :: Coordinates T.X T.Y -> Coordinates T.X T.Y -> [Coordinates T.X T.Y]
interpolateInt coordinatesSource coordinatesDestination	= interpolationsBySourceByDestination ! coordinatesSource ! coordinatesDestination

-- | The list of /coordinates/, between every permutation of source & destination on the /board/.
interpolationsBySourceByDestination :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => ByCoordinates x y (ByCoordinates x y [Coordinates x y])
{-# SPECIALISE interpolationsBySourceByDestination :: ByCoordinates T.X T.Y (ByCoordinates T.X T.Y [Coordinates T.X T.Y]) #-}	-- To promote memoisation.
interpolationsBySourceByDestination	= listArrayByCoordinates [
	listArrayByCoordinates [
		if source == destination
			then []
			else (
				case x' `compare` x of
					GT	-> [succ x .. x']
					LT	-> let startX = pred x in startX `seq` [startX, pred startX .. x']
					EQ	-> repeat x
			) >||< (
				case y' `compare` y of
					GT	-> [succ y .. y']
					LT	-> let startY = pred y in startY `seq` [startY, pred startY .. y']
					EQ	-> repeat y
			)
		| destination@MkCoordinates { getX = x', getY = y' }	<- range
	] | source@MkCoordinates { getX = x, getY = y }	<- range
 ] -- List-comprehension.

-- | The type of a function which changes one set of /coordinates/ to another.
type Transformation x y	= Coordinates x y -> Coordinates x y

{- |
	* Rotates the specified /coordinates/, so that the @Black@ pieces start on the specified side of the board; a /direction/ of @N@ involves no change.

	* CAVEAT: one can only request an integral multiple of 90 degrees.
-}
rotate :: (Enum x, Enum y) => Attribute.Direction.Direction -> Transformation x y
rotate direction coordinates@MkCoordinates {
	getX	= x,
	getY	= y
} = case Attribute.Direction.getXDirection &&& Attribute.Direction.getYDirection $ direction of
	(EQ, GT)	-> coordinates
	(LT, EQ)	-> MkCoordinates {
		getX	= toEnum $ Cartesian.Abscissa.xOrigin + fromIntegral yDistance',
		getY	= toEnum $ Cartesian.Ordinate.yOrigin + fromIntegral xDistance
	} -- +90 degrees, i.e. anti-clockwise.
	(EQ, LT)	-> MkCoordinates {
		getX	= toEnum $ Cartesian.Abscissa.xOrigin + fromIntegral xDistance',
		getY	= toEnum $ Cartesian.Ordinate.yOrigin + fromIntegral yDistance'
	} -- 180 degrees.
	(GT, EQ)	-> MkCoordinates {
		getX	= toEnum $ Cartesian.Abscissa.xOrigin + fromIntegral yDistance,
		getY	= toEnum $ Cartesian.Ordinate.yOrigin + fromIntegral xDistance'
	} -- -90 degrees, i.e. clockwise.
	_		-> Control.Exception.throw . Data.Exception.mkRequestFailure . showString "BishBosh.Cartesian.Coordinates.rotate:\tunable to rotate to direction" . Text.ShowList.showsAssociation $ shows direction "."
	where
		xDistance, xDistance', yDistance, yDistance'	:: T.Distance
		xDistance	= fromIntegral $ fromEnum x - Cartesian.Abscissa.xOrigin
		yDistance	= fromIntegral $ fromEnum y - Cartesian.Ordinate.yOrigin
		xDistance'	= pred Cartesian.Abscissa.xLength - xDistance
		yDistance'	= pred Cartesian.Ordinate.yLength - yDistance

{- |
	* Measures the signed distance between source & destination /coordinates/.

	* CAVEAT: beware the potential fence-post error.
-}
measureDistance :: (
	Enum	x,
	Enum	y,
	Num	distance
 )
	=> Coordinates x y	-- ^ Source.
	-> Coordinates x y	-- ^ Destination.
	-> (distance, distance)
{-# INLINE measureDistance #-}
measureDistance MkCoordinates {
	getX	= x,
	getY	= y
} MkCoordinates {
	getX	= x',
	getY	= y'
} = (fromIntegral $ fromEnum x' - fromEnum x, fromIntegral $ fromEnum y' - fromEnum y)

-- | The constant square of the radius of all coordinates.
radiusSquared :: (
	Fractional	radiusSquared,
	Integral	x,
	Integral	y
 ) => ByCoordinates x y radiusSquared
{-# SPECIALISE radiusSquared :: ByCoordinates T.X T.Y T.RadiusSquared #-}
radiusSquared	= listArrayByCoordinates [
	Factory.Math.Power.square (
		fromIntegral (x :: T.X) - Cartesian.Abscissa.centre
	) + Factory.Math.Power.square (
		fromIntegral (y :: T.Y) - Cartesian.Ordinate.centre
	) | MkCoordinates {
		getX	= x,
		getY	= y
	} <- range
 ] -- List-comprehension.

-- | The /logical colour/ of the specified square.
getLogicalColourOfSquare :: (Enum x, Enum y) => Coordinates x y -> Attribute.LogicalColourOfSquare.LogicalColourOfSquare
getLogicalColourOfSquare coordinates
	| even xDistance == even yDistance	= Attribute.LogicalColourOfSquare.black
	| otherwise				= Attribute.LogicalColourOfSquare.white
	where
		xDistance, yDistance	:: T.Distance
		(xDistance, yDistance)	= measureDistance minBound coordinates

-- | Whether the specified squares have the same logical colour.
areSquaresIsochromatic :: (Enum x, Enum y) => [Coordinates x y] -> Bool
areSquaresIsochromatic	= uncurry (||) . (all (== minBound) &&& all (== maxBound)) . map getLogicalColourOfSquare

-- | The conventional starting /coordinates/ for both @King@s.
kingsStartingCoordinates :: (Enum x, Enum y) => Attribute.LogicalColour.LogicalColour -> Coordinates x y
kingsStartingCoordinates logicalColour	= MkCoordinates {
	getX	= toEnum $ Cartesian.Abscissa.xOrigin + 4,
	getY	= Cartesian.Ordinate.firstRank logicalColour
}

-- | The conventional starting /coordinates/ for each @Rook@.
rooksStartingCoordinates :: (Enum x, Enum y) => Attribute.LogicalColour.LogicalColour -> [Coordinates x y]
rooksStartingCoordinates Attribute.LogicalColour.Black	= [topLeft, maxBound]
rooksStartingCoordinates _				= [minBound, bottomRight]

-- | Whether the specified /coordinates/ are where a @Pawn@ of the specified /logical colour/ starts.
isPawnsFirstRank
	:: (Enum y, Eq y)
	=> Attribute.LogicalColour.LogicalColour
	-> Coordinates x y
	-> Bool
{-# INLINE isPawnsFirstRank #-}
isPawnsFirstRank logicalColour MkCoordinates { getY = y }	= y == Cartesian.Ordinate.pawnsFirstRank logicalColour

-- | Whether a @Pawn@ is currently on the appropriate /rank/ to take an opponent's @Pawn@ /en-passant/.
isEnPassantRank
	:: (Enum y, Eq y)
	=> Attribute.LogicalColour.LogicalColour
	-> Coordinates x y
	-> Bool
isEnPassantRank logicalColour MkCoordinates { getY = y }	= y == Cartesian.Ordinate.enPassantRank logicalColour

-- | A boxed array indexed by /coordinates/, of arbitrary elements.
type ByCoordinates x y	= Data.Array.IArray.Array (Coordinates x y)

-- | Array-constructor.
listArrayByCoordinates :: (
	Data.Array.IArray.IArray	a e,
	Enum				x,
	Enum				y,
	Ord				x,
	Ord				y
 ) => [e] -> a (Coordinates x y) e
listArrayByCoordinates	= Data.Array.IArray.listArray (minBound, maxBound)

