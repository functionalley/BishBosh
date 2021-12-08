{-# LANGUAGE CPP, MagicHash #-}
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
	ArrayByCoordinates,
#ifdef USE_UNBOXED
	UArrayByCoordinates,
#endif
-- * Constants
	tag,
	topLeft,
	bottomRight,
	nSquares,
--	extrapolationsByDirectionByCoordinates,
--	interpolationsByDestinationBySource,
-- * Functions
--	extrapolate',
	extrapolate,
	applyAlongDirectionsFrom,
	interpolate,
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
-- ** Constructors
	mkCoordinates,
	mkMaybeCoordinates,
	mkRelativeCoordinates,
	listArrayByCoordinates,
	arrayByCoordinates,
-- ** Predicates
--	inBounds,
	isPawnsFirstRank,
	isEnPassantRank,
	areSquaresIsochromatic
) where

import			Control.Arrow((&&&), (***))
import			Data.Array.IArray((!))
import qualified	BishBosh.Cartesian.Abscissa		as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Ordinate		as Cartesian.Ordinate
import qualified	BishBosh.Colour.LogicalColour		as Colour.LogicalColour
import qualified	BishBosh.Colour.LogicalColourOfSquare	as Colour.LogicalColourOfSquare
import qualified	BishBosh.Direction.Direction		as Direction.Direction
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	BishBosh.Property.Orientated		as Property.Orientated
import qualified	BishBosh.Property.Reflectable		as Property.Reflectable
import qualified	BishBosh.Property.Rotatable		as Property.Rotatable
import qualified	BishBosh.Type.Count			as Type.Count
import qualified	BishBosh.Type.Length			as Type.Length
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.Map				as Map
import qualified	Data.Maybe

#ifdef USE_PARALLEL
import qualified	Control.Parallel.Strategies
#endif

#ifdef USE_UNBOXED
#	if !(defined USE_NEWTYPE_WRAPPERS || defined USE_NARROW_NUMBERS)
import			GHC.Exts(Int(I#))
import			GHC.Prim((+#), (*#))
#	endif
import qualified	Data.Array.Unboxed
#endif

-- | Used to qualify XML.
tag :: String
tag	= "coordinates"

-- | The /coordinates/ of a square on the board.
data Coordinates	= MkCoordinates {
	getX	:: ! Type.Length.X,	-- ^ Abscissa.
	getY	:: ! Type.Length.Y	-- ^ Ordinate.
} deriving Eq

instance Bounded Coordinates where
	minBound = MkCoordinates {
		getX	= Cartesian.Abscissa.xMin,
		getY	= Cartesian.Ordinate.yMin
	} -- Bottom Left.
	maxBound = MkCoordinates {
		getX	= Cartesian.Abscissa.xMax,
		getY	= Cartesian.Ordinate.yMax
	} -- Top Right.

instance Control.DeepSeq.NFData Coordinates where
--	rnf MkCoordinates { getX = x, getY = y }	= Control.DeepSeq.rnf (x, y)
	rnf _	= ()	-- N.B.: it's already strict.

instance Data.Array.IArray.Ix Coordinates where
	range (lower, upper)			= Control.Exception.assert (lower == minBound && upper == maxBound) Property.FixedMembership.members
	inRange (lower, upper) coordinates	= Control.Exception.assert (coordinates >= lower && coordinates <= upper) True
	index (lower, upper)			= Control.Exception.assert (lower == minBound && upper == maxBound) . fromEnum

instance Enum Coordinates where
	toEnum = (
		\(y, x) -> MkCoordinates {
			getX	= Cartesian.Abscissa.fromIx x,
			getY	= Cartesian.Ordinate.fromIx y
		}
	 ) . (`divMod` fromIntegral Cartesian.Abscissa.xLength)

#if defined USE_UNBOXED && !(defined USE_NEWTYPE_WRAPPERS || defined USE_NARROW_NUMBERS)
	fromEnum MkCoordinates {
		getX	= I# x,
		getY	= I# y
	} = I# (8# *# y +# x)	-- CAVEAT: bypasses modules 'Cartesian.Abscissa' & 'Cartesian.Ordinate'.
#else
	fromEnum MkCoordinates {
		getX	= x,
		getY	= y
	} = fromIntegral Cartesian.Abscissa.xLength * Cartesian.Ordinate.toIx y + Cartesian.Abscissa.toIx x
#endif

instance Ord Coordinates where
	MkCoordinates { getX = x, getY = y } `compare` MkCoordinates { getX = x', getY = y' }	= (y, x) `compare` (y', x')	-- N.B.: x is less significant than y.

instance Show Coordinates where
	showsPrec precedence MkCoordinates { getX = x, getY = y }	= showsPrec precedence (x, y)

instance Read Coordinates where
	readsPrec precedence s	= [
		(coordinates, remainder) |
			(Just coordinates, remainder)	<- Control.Arrow.first (uncurry mkMaybeCoordinates) `map` readsPrec precedence s
	 ] -- List-comprehension.

instance Property.Opposable.Opposable Coordinates where
	getOpposite MkCoordinates {
		getX	= x,
		getY	= y
	} = MkCoordinates {
		getX	= Cartesian.Abscissa.reflect x,
		getY	= Cartesian.Ordinate.reflect y
	}

instance Property.Reflectable.ReflectableOnX Coordinates where
	reflectOnX coordinates@MkCoordinates { getY = y }	= coordinates { getY = Cartesian.Ordinate.reflect y }

instance Property.Reflectable.ReflectableOnY Coordinates where
	reflectOnY coordinates@MkCoordinates { getX = x }	= coordinates { getX = Cartesian.Abscissa.reflect x }

instance Property.Rotatable.Rotatable Coordinates where
	rotate90 MkCoordinates {
		getX	= x,
		getY	= y
	} = MkCoordinates {
		getX	= fromIntegral $! Cartesian.Ordinate.reflect y,
		getY	= fromIntegral x
	} -- +90 degrees, i.e. anti-clockwise.
	rotate180	= Property.Opposable.getOpposite
	rotate270 MkCoordinates {
		getX	= x,
		getY	= y
	} = MkCoordinates {
		getX	= fromIntegral y,
		getY	= fromIntegral $! Cartesian.Abscissa.reflect x
	} -- -90 degrees, i.e. clockwise.

-- | Constant.
topLeft :: Coordinates
topLeft = MkCoordinates {
	getX	= Cartesian.Abscissa.xMin,
	getY	= Cartesian.Ordinate.yMax
}

-- | Constant.
bottomRight :: Coordinates
bottomRight = MkCoordinates {
	getX	= Cartesian.Abscissa.xMax,
	getY	= Cartesian.Ordinate.yMin
}

-- | The constant number of squares on the board.
nSquares :: Type.Count.NCoordinates
nSquares	= fromIntegral Cartesian.Abscissa.xLength * fromIntegral Cartesian.Ordinate.yLength

instance Property.FixedMembership.FixedMembership Coordinates where
	members	= [
		MkCoordinates {
			getX	= x,
			getY	= y
		} |
			y	<- Cartesian.Ordinate.yRange,
			x	<- Cartesian.Abscissa.xRange
	 ] -- List-comprehension.

-- | Predicate.
inBounds
	:: Type.Length.X	-- ^ Abscissa.
	-> Type.Length.Y	-- ^ Ordinate.
	-> Bool
inBounds x y	= Cartesian.Abscissa.inBounds x && Cartesian.Ordinate.inBounds y

-- | Constructor.
mkCoordinates
	:: Type.Length.X	-- ^ Abscissa.
	-> Type.Length.Y	-- ^ Ordinate.
	-> Coordinates
mkCoordinates x y	= Control.Exception.assert (inBounds x y) $ MkCoordinates x y

-- | Safe constructor.
mkMaybeCoordinates
	:: Type.Length.X	-- ^ Abscissa.
	-> Type.Length.Y	-- ^ Ordinate.
	-> Maybe Coordinates
mkMaybeCoordinates x y
	| inBounds x y	= Just MkCoordinates { getX = x, getY = y }
	| otherwise	= Nothing

-- | The type of a function which changes one set of /coordinates/ to another.
type Transformation	= Coordinates -> Coordinates

{- |
	* Translate the specified /coordinates/ using the specified mapping.

	* CAVEAT: the caller must ensure that the results are legal.
-}
translate :: ((Type.Length.X, Type.Length.Y) -> (Type.Length.X, Type.Length.Y)) -> Transformation
translate transformation MkCoordinates {
	getX	= x,
	getY	= y
} = uncurry mkCoordinates $ transformation (x, y)

-- | Where legal, translate the specified /coordinates/.
maybeTranslate :: ((Type.Length.X, Type.Length.Y) -> (Type.Length.X, Type.Length.Y)) -> Coordinates -> Maybe Coordinates
maybeTranslate transformation MkCoordinates {
	getX	= x,
	getY	= y
} = uncurry mkMaybeCoordinates $ transformation (x, y)

{- |
	* Translate the specified abscissa.

	* CAVEAT: the caller must ensure that the results are legal.
-}
translateX :: (Type.Length.X -> Type.Length.X) -> Transformation
translateX transformation coordinates@MkCoordinates { getX = x }	= coordinates { getX = Cartesian.Abscissa.translate transformation x }

-- | Where legal, translate the /x/-component of the specified /coordinates/.
maybeTranslateX
	:: (Type.Length.X -> Type.Length.X)	-- ^ Translation.
	-> Coordinates
	-> Maybe Coordinates
maybeTranslateX transformation coordinates@MkCoordinates { getX = x }	= (\x' -> coordinates { getX = x' }) <$> Cartesian.Abscissa.maybeTranslate transformation x

{- |
	* Translate the specified ordinate.

	* CAVEAT: the caller must ensure that the results are legal.
-}
translateY :: (Type.Length.Y -> Type.Length.Y) -> Transformation
translateY transformation coordinates@MkCoordinates { getY = y }	= coordinates { getY = Cartesian.Ordinate.translate transformation y }

-- | Where legal, translate the /y/-component of the specified /coordinates/.
maybeTranslateY
	:: (Type.Length.Y -> Type.Length.Y)	-- ^ Translation.
	-> Coordinates
	-> Maybe Coordinates
maybeTranslateY transformation coordinates@MkCoordinates { getY = y }	= (\y' -> coordinates { getY = y' }) <$> Cartesian.Ordinate.maybeTranslate transformation y

{- |
	* Construct /coordinates/ relative to 'minBound'.

	* CAVEAT: the caller must ensure that the results are legal.
-}
mkRelativeCoordinates :: ((Type.Length.X, Type.Length.Y) -> (Type.Length.X, Type.Length.Y)) -> Coordinates
mkRelativeCoordinates	= (`translate` minBound)

{- |
	* Move one step towards the opponent.

	* CAVEAT: the caller must ensure that the results are legal.
-}
advance
	:: Colour.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the /piece/ which is to advance.
	-> Transformation
advance logicalColour	= translateY $ if Colour.LogicalColour.isBlack logicalColour
	then pred
	else succ

-- | Where legal, move one step towards the opponent.
maybeAdvance
	:: Colour.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the /piece/ which is to advance.
	-> Coordinates				-- ^ The location from which to advanced.
	-> Maybe Coordinates
maybeAdvance logicalColour	= maybeTranslateY $ if Colour.LogicalColour.isBlack logicalColour
	then pred
	else succ

{- |
	* Move one step away from the opponent.

	* CAVEAT: the caller must ensure that the results are legal.
-}
retreat
	:: Colour.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the /piece/ which is to retreat.
	-> Transformation
retreat	= advance . Property.Opposable.getOpposite

-- | Where legal, move one step away from the opponent.
maybeRetreat
	:: Colour.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the /piece/ which is to retreat.
	-> Coordinates				-- ^ The location from which to retreat.
	-> Maybe Coordinates
maybeRetreat	= maybeAdvance . Property.Opposable.getOpposite

-- | Get the /coordinates/ immediately left & right.
getAdjacents :: Coordinates -> [Coordinates]
getAdjacents coordinates@MkCoordinates { getX = x }	= map (\x' -> coordinates { getX = x' }) $ Cartesian.Abscissa.getAdjacents x

-- | Generates a line of /coordinates/, starting just after the specified source & proceeding in the specified /direction/ to the edge of the board.
extrapolate'
	:: Coordinates				-- ^ The point from which to start.
	-> Direction.Direction.Direction	-- ^ The direction in which to proceed.
	-> [Coordinates]
extrapolate' MkCoordinates {
	getX	= x,
	getY	= y
} direction = uncurry (zipWith MkCoordinates) $ if Property.Orientated.isParallel direction
	then if Property.Orientated.isVertical direction
		then doVertical
		else doHorizontal
	else doDiagonal
	where
		xIncreasing, xDecreasing :: [Type.Length.X]
		xIncreasing	= [succ x .. Cartesian.Abscissa.xMax]
		xDecreasing	= let startX = pred x in startX `seq` [startX, pred startX .. Cartesian.Abscissa.xMin]

		yIncreasing, yDecreasing :: [Type.Length.Y]
		yIncreasing	= [succ y .. Cartesian.Ordinate.yMax]
		yDecreasing	= let startY = pred y in startY `seq` [startY, pred startY .. Cartesian.Ordinate.yMin]

		doVertical, doHorizontal, doDiagonal :: ([Type.Length.X], [Type.Length.Y])
		doVertical	= (
			repeat x,
			if direction == Direction.Direction.s then yDecreasing else yIncreasing
		 ) -- Pair.
		doHorizontal	= (
			if direction == Direction.Direction.w then xDecreasing else xIncreasing,
			repeat y
		 ) -- Pair.
		doDiagonal
			| direction == Direction.Direction.sw	= (xDecreasing,	yDecreasing)
			| direction == Direction.Direction.se	= (xIncreasing,	yDecreasing)
			| direction == Direction.Direction.nw	= (xDecreasing,	yIncreasing)
			| otherwise {-NE-}			= (xIncreasing,	yIncreasing)

{- |
	* Generates a line of /coordinates/, starting just after the specified source & proceeding in the specified /direction/ to the edge of the board.

	* CAVEAT: this is a performance-hotspot (it's also responsible for the allocation of a third of the application's memory); refactor => re-profile.
	In consequence, it is typically automatically avoided using a rewrite-rule to lookup an array of the results from all possible calls.
-}
extrapolate
	:: Coordinates				-- ^ The point from which to start.
	-> Direction.Direction.Direction	-- ^ The direction in which to proceed.
	-> [Coordinates]
extrapolate coordinates	direction	= extrapolationsByDirectionByCoordinates ! coordinates ! direction

-- | The constant lists of /coordinates/, extrapolated from every /coordinate/ in the /board/, in every /direction/.
extrapolationsByDirectionByCoordinates :: ArrayByCoordinates (Direction.Direction.ArrayByDirection [Coordinates])
extrapolationsByDirectionByCoordinates	= listArrayByCoordinates
#ifdef USE_PARALLEL
	. Control.Parallel.Strategies.withStrategy (Control.Parallel.Strategies.parList Control.Parallel.Strategies.rdeepseq)
#endif
	$ map (
		\coordinates	-> Direction.Direction.listArrayByDirection $ extrapolate' coordinates `map` Property.FixedMembership.members {-direction-}
	) Property.FixedMembership.members {-coordinates-}

-- | Apply the specified function to each line of /coordinates/ extrapolated from the specified central hub, in each of the specified /direction/s.
applyAlongDirectionsFrom
	:: ([Coordinates] -> [a])			-- ^ Individually map each straight line of coordinates extrapolated from the central hub.
	-> Coordinates					-- ^ The central hub from which to extrapolate.
	-> Maybe [Direction.Direction.Direction]	-- ^ The directions in which to extrapolate; 'Nothing' implies omnidirectional.
	-> [a]
applyAlongDirectionsFrom f from	= Data.Maybe.maybe (
	Data.Foldable.foldMap f $ extrapolationsByDirectionByCoordinates ! from -- Traverse all directions from this coordinate, without repeatedly converting direction to an array-index.
 ) (
	concatMap $ f . (extrapolationsByDirectionByCoordinates ! from !)
 )

-- | The constant lists of /coordinates/, between every permutation of source & valid destination on the /board/.
interpolationsByDestinationBySource :: ArrayByCoordinates (Map.Map Coordinates [Coordinates])
interpolationsByDestinationBySource	= Data.Array.IArray.amap (
	Map.fromList . map (
		last {-destination-} &&& id {-interpolation-}
	) . concatMap (
		tail {-remove null list-} . Data.List.inits	-- Generate all possible interpolations from this extrapolation.
	) . Data.Foldable.toList
 ) extrapolationsByDirectionByCoordinates	-- Derive from extrapolations.

{- |
	* Generates a line of /coordinates/ covering the half open interval @(source, destination]@.

	* CAVEAT: the destination-/coordinates/ must be a valid @Queen@'s /move/ from the source; so that all intermediate points lie on a square of the board.
-}
interpolate :: Coordinates -> Coordinates -> [Coordinates]
interpolate coordinatesSource coordinatesDestination	= interpolationsByDestinationBySource ! coordinatesSource Map.! coordinatesDestination

{- |
	* Measures the signed distance between source & destination /coordinates/.

	* N.B.: this isn't the typically /irrational/ distance a rational crow would fly, but rather the integral /x/ & /y/ components of that path.

	* CAVEAT: beware the potential fence-post error.
-}
measureDistance
	:: Coordinates	-- ^ Source.
	-> Coordinates	-- ^ Destination.
	-> (Type.Length.X, Type.Length.Y)
measureDistance MkCoordinates {
	getX	= x,
	getY	= y
} MkCoordinates {
	getX	= x',
	getY	= y'
} = (x' - x, y' - y)

-- | The /logical colour/ of the specified square.
getLogicalColourOfSquare :: Coordinates -> Colour.LogicalColourOfSquare.LogicalColourOfSquare
getLogicalColourOfSquare coordinates
	| uncurry (==) . (
		even *** even
	) $ measureDistance minBound coordinates	= Colour.LogicalColourOfSquare.black
	| otherwise					= Colour.LogicalColourOfSquare.white

-- | Whether the specified squares have the same /logical colour/.
areSquaresIsochromatic :: [Coordinates] -> Bool
areSquaresIsochromatic	= uncurry (||) . (all (== minBound) &&& all (== maxBound)) . map getLogicalColourOfSquare

-- | The conventional starting /coordinates/ for the @King@ of the specified /logical colour/.
kingsStartingCoordinates :: Colour.LogicalColour.LogicalColour -> Coordinates
kingsStartingCoordinates logicalColour	= MkCoordinates {
	getX	= Cartesian.Abscissa.kingsFile,
	getY	= Cartesian.Ordinate.firstRank logicalColour
}

-- | The conventional starting /coordinates/ for each @Rook@.
rooksStartingCoordinates :: Colour.LogicalColour.LogicalColour -> [Coordinates]
rooksStartingCoordinates Colour.LogicalColour.Black	= [topLeft, maxBound]
rooksStartingCoordinates _				= [minBound, bottomRight]

-- | Whether the specified /coordinates/ are where a @Pawn@ of the specified /logical colour/ starts.
isPawnsFirstRank :: Coordinates -> Colour.LogicalColour.LogicalColour -> Bool
isPawnsFirstRank MkCoordinates { getY = y }	= (== y) . Cartesian.Ordinate.pawnsFirstRank

-- | Whether a @Pawn@ is currently on the appropriate /rank/ to take an opponent's @Pawn@ /en-passant/.
isEnPassantRank :: Coordinates -> Colour.LogicalColour.LogicalColour -> Bool
isEnPassantRank MkCoordinates { getY = y }	= (== y) . Cartesian.Ordinate.enPassantRank

-- | A boxed array indexed by /coordinates/, of arbitrary elements.
type ArrayByCoordinates	= Data.Array.IArray.Array Coordinates

#ifdef USE_UNBOXED
-- | An unboxed array indexed by /coordinates/, of fixed-size elements.
type UArrayByCoordinates	= Data.Array.Unboxed.UArray Coordinates
#endif

-- | Array-constructor from an ordered list of elements.
listArrayByCoordinates :: Data.Array.IArray.IArray a e => [e] -> a Coordinates e
listArrayByCoordinates	= Data.Array.IArray.listArray (minBound, maxBound)

-- | Array-constructor from an association-list.
arrayByCoordinates :: Data.Array.IArray.IArray a e => [(Coordinates, e)] -> a Coordinates e
arrayByCoordinates	= Data.Array.IArray.array (minBound, maxBound)

