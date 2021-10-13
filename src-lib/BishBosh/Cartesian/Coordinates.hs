{-# LANGUAGE CPP, FlexibleContexts #-}
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
--	UArrayByCoordinates,
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
--	rotate,
-- ** Constructors
	mkCoordinates,
	mkMaybeCoordinates,
--	toIx,
	fromIx,
	mkRelativeCoordinates,
	listArrayByCoordinates,
	arrayByCoordinates,
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
import qualified	BishBosh.Property.FixedMembership		as Property.FixedMembership
import qualified	BishBosh.Property.Opposable			as Property.Opposable
import qualified	BishBosh.Property.Reflectable			as Property.Reflectable
import qualified	BishBosh.Property.Rotatable			as Property.Rotatable
import qualified	BishBosh.Text.ShowList				as Text.ShowList
import qualified	BishBosh.Type.Length				as Type.Length
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.Array.Unboxed
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.Map					as Map
import qualified	Data.Maybe

#ifdef USE_PARALLEL
import qualified	Control.Parallel.Strategies
#endif

-- | Used to qualify XML.
tag :: String
tag	= "coordinates"

-- | The /coordinates/ of a square on the board.
data Coordinates	= MkCoordinates {
	getX	:: Type.Length.X,	-- ^ Abscissa.
	getY	:: Type.Length.Y	-- ^ Ordinate.
} deriving Eq

instance Control.DeepSeq.NFData Coordinates where
	rnf MkCoordinates { getX = x, getY = y }	= Control.DeepSeq.rnf (x, y)

instance Show Coordinates where
	showsPrec precedence MkCoordinates { getX = x, getY = y }	= showsPrec precedence (x, y)

instance Read Coordinates where
	readsPrec precedence s	= [
		(coordinates, remainder) |
			((x, y), remainder)	<- readsPrec precedence s,
			coordinates		<- Data.Maybe.maybeToList $ mkMaybeCoordinates x y
	 ] -- List-comprehension.

instance Ord Coordinates where
	MkCoordinates { getX = x, getY = y } `compare` MkCoordinates { getX = x', getY = y' }	= (y, x) `compare` (y', x')	-- N.B.: x is less significant than y, as required by the implementation of 'Data.Array.IArray.Ix.inRange'.

instance Bounded Coordinates where
	minBound = MkCoordinates {
		getX	= Cartesian.Abscissa.xMin,
		getY	= Cartesian.Ordinate.yMin
	} -- Bottom Left.
	maxBound = MkCoordinates {
		getX	= Cartesian.Abscissa.xMax,
		getY	= Cartesian.Ordinate.yMax
	} -- Top Right.

instance Data.Array.IArray.Ix Coordinates where
	range (lower, upper)			= Control.Exception.assert (lower == minBound && upper == maxBound) Property.FixedMembership.members
	inRange (lower, upper) coordinates	= Control.Exception.assert (coordinates >= lower && coordinates <= upper) True
	index (lower, upper)			= Control.Exception.assert (lower == minBound && upper == maxBound) . toIx

instance Property.Reflectable.ReflectableOnX Coordinates where
	reflectOnX coordinates@MkCoordinates { getY = y }	= coordinates { getY = Cartesian.Ordinate.reflect y }

instance Property.Reflectable.ReflectableOnY Coordinates where
	reflectOnY coordinates@MkCoordinates { getX = x }	= coordinates { getX = Cartesian.Abscissa.reflect x }

instance Property.Rotatable.Rotatable Coordinates where
	rotate90	= rotate Attribute.Direction.w
	rotate180	= rotate Attribute.Direction.s
	rotate270	= rotate Attribute.Direction.e

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
nSquares :: Int
nSquares	= fromIntegral $ Cartesian.Abscissa.xLength * Cartesian.Ordinate.yLength

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
{-# INLINABLE inBounds #-}
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

-- | Convert to an array-index.
toIx :: Coordinates -> Int
toIx MkCoordinates {
	getX	= x,
	getY	= y
} = fromIntegral Cartesian.Abscissa.xLength * Cartesian.Ordinate.toIx y + Cartesian.Abscissa.toIx x

{- |
	* Construct from the specified array-index.

	* CAVEAT: assumes that the array is indexed by the whole range of /coordinates/.
-}
fromIx :: Int -> Coordinates
fromIx	= (
	\(y, x) -> MkCoordinates {
		getX	= Cartesian.Abscissa.fromIx x,
		getY	= Cartesian.Ordinate.fromIx y
	}
 ) . (`divMod` fromIntegral Cartesian.Abscissa.xLength)

{- |
	* Translate the specified /coordinates/ using the specified mapping.

	* CAVEAT: the caller must ensure that the results are legal.
-}
translate :: ((Type.Length.X, Type.Length.Y) -> (Type.Length.X, Type.Length.Y)) -> Coordinates -> Coordinates
translate transformation MkCoordinates {
	getX	= x,
	getY	= y
} = uncurry mkCoordinates $ transformation (x, y)

-- | Where legal, translate the specified /coordinates/.
maybeTranslate :: ((Type.Length.X, Type.Length.Y) -> (Type.Length.X, Type.Length.Y)) -> Coordinates -> Maybe Coordinates
{-# INLINABLE maybeTranslate #-}
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
maybeTranslateX transformation coordinates@MkCoordinates { getX = x }	= (\x' -> coordinates { getX = x' }) `fmap` Cartesian.Abscissa.maybeTranslate transformation x

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
maybeTranslateY transformation coordinates@MkCoordinates { getY = y }	= (\y' -> coordinates { getY = y' }) `fmap` Cartesian.Ordinate.maybeTranslate transformation y

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
	:: Attribute.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the /piece/ which is to advance.
	-> Transformation
{-# INLINE advance #-}
advance logicalColour	= translateY $ if Attribute.LogicalColour.isBlack logicalColour
	then pred
	else succ

-- | Where legal, move one step towards the opponent.
maybeAdvance
	:: Attribute.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the /piece/ which is to advance.
	-> Coordinates				-- ^ The location from which to advanced.
	-> Maybe Coordinates
maybeAdvance logicalColour	= maybeTranslateY $ if Attribute.LogicalColour.isBlack logicalColour
	then pred
	else succ

{- |
	* Move one step away from the opponent.

	* CAVEAT: the caller must ensure that the results are legal.
-}
retreat
	:: Attribute.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the /piece/ which is to retreat.
	-> Transformation
retreat	= advance . Property.Opposable.getOpposite

-- | Where legal, move one step away from the opponent.
maybeRetreat
	:: Attribute.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the /piece/ which is to retreat.
	-> Coordinates				-- ^ The location from which to retreat.
	-> Maybe Coordinates
maybeRetreat	= maybeAdvance . Property.Opposable.getOpposite

-- | Get the /coordinates/ immediately left & right.
getAdjacents :: Coordinates -> [Coordinates]
getAdjacents coordinates@MkCoordinates { getX = x }	= map (\x' -> coordinates { getX = x' }) $ Cartesian.Abscissa.getAdjacents x

-- | Generates a line of /coordinates/, starting just after the specified source & proceeding in the specified /direction/ to the edge of the board.
extrapolate'
	:: Attribute.Direction.Direction	-- ^ The direction in which to proceed.
	-> Coordinates			-- ^ The point from which to start.
	-> [Coordinates]
extrapolate' direction MkCoordinates {
	getX	= x,
	getY	= y
} = zipWith MkCoordinates (
	case Attribute.Direction.getXDirection direction of
		GT	-> [succ x .. Cartesian.Abscissa.xMax]
		LT	-> let startX = pred x in startX `seq` [startX, pred startX .. Cartesian.Abscissa.xMin]
		EQ	-> repeat x
 ) (
	case Attribute.Direction.getYDirection direction of
		GT	-> [succ y .. Cartesian.Ordinate.yMax]
		LT	-> let startY = pred y in startY `seq` [startY, pred startY .. Cartesian.Ordinate.yMin]
		EQ	-> repeat y
 )

{- |
	* Generates a line of /coordinates/, starting just after the specified source & proceeding in the specified /direction/ to the edge of the board.

	* CAVEAT: this is a performance-hotspot (it's also responsible for the allocation of a third of the application's memory); refactor => re-profile.
	In consequence, it is typically automatically avoided using a rewrite-rule to lookup an array of the results from all possible calls.
-}
extrapolate
	:: Attribute.Direction.Direction	-- ^ The direction in which to proceed.
	-> Coordinates			-- ^ The point from which to start.
	-> [Coordinates]
extrapolate direction coordinates	= extrapolationsByDirectionByCoordinates ! coordinates ! direction

-- | The constant lists of /coordinates/, extrapolated from every /coordinate/ in the /board/, in every /direction/.
extrapolationsByDirectionByCoordinates :: ArrayByCoordinates (Attribute.Direction.ArrayByDirection [Coordinates])
extrapolationsByDirectionByCoordinates	= listArrayByCoordinates
#ifdef USE_PARALLEL
	. Control.Parallel.Strategies.withStrategy (Control.Parallel.Strategies.parList Control.Parallel.Strategies.rdeepseq)
#endif
	$ map (
		\coordinates	-> Attribute.Direction.listArrayByDirection $ map (`extrapolate'` coordinates) Property.FixedMembership.members
	) Property.FixedMembership.members

-- | The list of /coordinates/, between every permutation of source & valid destination on the /board/.
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

-- | The type of a function which changes one set of /coordinates/ to another.
type Transformation	= Coordinates -> Coordinates

{- |
	* Rotates the specified /coordinates/, so that the @Black@ pieces start on the specified side of the board; a /direction/ of @N@ involves no change.

	* CAVEAT: one can only request an integral multiple of 90 degrees.
-}
rotate :: Attribute.Direction.Direction -> Transformation
rotate direction coordinates@MkCoordinates {
	getX	= x,
	getY	= y
} = case Attribute.Direction.getXDirection &&& Attribute.Direction.getYDirection $ direction of
	(EQ, GT)	-> coordinates
	(LT, EQ)	-> MkCoordinates {
		getX	= Cartesian.Abscissa.fromIx $ fromIntegral yDistance',
		getY	= Cartesian.Ordinate.fromIx $ fromIntegral xDistance
	} -- +90 degrees, i.e. anti-clockwise.
	(EQ, LT)	-> MkCoordinates {
		getX	= Cartesian.Abscissa.fromIx $ fromIntegral xDistance',
		getY	= Cartesian.Ordinate.fromIx $ fromIntegral yDistance'
	} -- 180 degrees.
	(GT, EQ)	-> MkCoordinates {
		getX	= Cartesian.Abscissa.fromIx $ fromIntegral yDistance,
		getY	= Cartesian.Ordinate.fromIx $ fromIntegral xDistance'
	} -- -90 degrees, i.e. clockwise.
	_		-> Control.Exception.throw . Data.Exception.mkRequestFailure . showString "BishBosh.Cartesian.Coordinates.rotate:\tunable to rotate to direction" . Text.ShowList.showsAssociation $ shows direction "."
	where
		xDistance, xDistance', yDistance, yDistance'	:: Type.Length.Distance
		xDistance	= fromIntegral $ Cartesian.Abscissa.toIx x
		yDistance	= fromIntegral $ Cartesian.Ordinate.toIx y
		xDistance'	= pred Cartesian.Abscissa.xLength - xDistance
		yDistance'	= pred Cartesian.Ordinate.yLength - yDistance

{- |
	* Measures the signed distance between source & destination /coordinates/.

	* N.B.: this isn't the /irrational/ distance a rational crow would fly, but rather the integral /x/ & /y/ components of that path.

	* CAVEAT: beware the potential fence-post error.
-}
measureDistance
	:: Num distance
	=> Coordinates		-- ^ Source.
	-> Coordinates		-- ^ Destination.
	-> (distance, distance)	-- ^ (X-distance, Y-distance)
{-# INLINE measureDistance #-}
measureDistance MkCoordinates {
	getX	= x,
	getY	= y
} MkCoordinates {
	getX	= x',
	getY	= y'
} = (fromIntegral $ fromEnum x' - fromEnum x, fromIntegral $ fromEnum y' - fromEnum y)

-- | The /logical colour/ of the specified square.
getLogicalColourOfSquare :: Coordinates -> Attribute.LogicalColourOfSquare.LogicalColourOfSquare
getLogicalColourOfSquare coordinates
	| even $ uncurry (+) distance	= Attribute.LogicalColourOfSquare.black
	| otherwise			= Attribute.LogicalColourOfSquare.white
	where
		distance :: (Type.Length.Distance, Type.Length.Distance)
		distance	= measureDistance minBound coordinates

-- | Whether the specified squares have the same /logical colour/.
areSquaresIsochromatic :: [Coordinates] -> Bool
areSquaresIsochromatic	= uncurry (||) . (all (== minBound) &&& all (== maxBound)) . map getLogicalColourOfSquare

-- | The conventional starting /coordinates/ for the @King@ of the specified /logical colour/.
kingsStartingCoordinates :: Attribute.LogicalColour.LogicalColour -> Coordinates
kingsStartingCoordinates logicalColour	= MkCoordinates {
	getX	= Cartesian.Abscissa.kingsFile,
	getY	= Cartesian.Ordinate.firstRank logicalColour
}

-- | The conventional starting /coordinates/ for each @Rook@.
rooksStartingCoordinates :: Attribute.LogicalColour.LogicalColour -> [Coordinates]
rooksStartingCoordinates Attribute.LogicalColour.Black	= [topLeft, maxBound]
rooksStartingCoordinates _				= [minBound, bottomRight]

-- | Whether the specified /coordinates/ are where a @Pawn@ of the specified /logical colour/ starts.
isPawnsFirstRank :: Attribute.LogicalColour.LogicalColour -> Coordinates -> Bool
{-# INLINE isPawnsFirstRank #-}
isPawnsFirstRank logicalColour MkCoordinates { getY = y }	= y == Cartesian.Ordinate.pawnsFirstRank logicalColour

-- | Whether a @Pawn@ is currently on the appropriate /rank/ to take an opponent's @Pawn@ /en-passant/.
isEnPassantRank :: Attribute.LogicalColour.LogicalColour -> Coordinates -> Bool
isEnPassantRank logicalColour MkCoordinates { getY = y }	= y == Cartesian.Ordinate.enPassantRank logicalColour

-- | A boxed array indexed by /coordinates/, of arbitrary elements.
type ArrayByCoordinates	= Data.Array.IArray.Array Coordinates

-- | An unboxed array indexed by /coordinates/, of fixed-size elements.
type UArrayByCoordinates	= Data.Array.Unboxed.UArray Coordinates

-- | Array-constructor from an ordered list of elements.
listArrayByCoordinates :: Data.Array.IArray.IArray a e => [e] -> a Coordinates e
listArrayByCoordinates	= Data.Array.IArray.listArray (minBound, maxBound)

-- | Array-constructor from an association-list.
arrayByCoordinates :: Data.Array.IArray.IArray a e => [(Coordinates, e)] -> a Coordinates e
arrayByCoordinates	= Data.Array.IArray.array (minBound, maxBound)

