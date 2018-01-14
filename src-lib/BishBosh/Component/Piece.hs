{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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

 [@DESCRIPTION@]	Defines the data-type which represents any chess-piece.
-}

module BishBosh.Component.Piece(
-- * Types
-- ** Type-synonyms
	NPieces,
	ByPiece,
	LocatedPiece,
-- ** Data-types
	Piece(
--		MkPiece,
		getLogicalColour,
		getRank
	),
-- * Constants
--	tag,
	nPiecesPerSide,
	range,
--	attackVectorsByPiece,
	attackDirectionsByPiece,
--	attackDestinationsByCoordinatesByRankByLogicalColour,
-- * Functions
	findAttackDestinations,
--	findAttackDestinationsInt,
-- ** Mutators
	promote,
-- ** Constructors
	mkBishop,
	mkKing,
	mkKnight,
	mkPawn,
	mkPiece,
	mkQueen,
	mkRook,
	listArrayByPiece,
-- ** Predicates
	canAttackAlong,
	canMoveBetween,
	isBlack,
	isFriend,
--	isPeer,
	isPawn,
	isKnight,
--	isBishop,
--	isRook,
--	isQueen,
	isKing,
	isPawnPromotion
) where

import			Control.Arrow((&&&), (***))
import			Data.Array.IArray((!))
import qualified	BishBosh.Attribute.Direction		as Attribute.Direction
import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa		as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate		as Cartesian.Ordinate
import qualified	BishBosh.Cartesian.Vector		as Cartesian.Vector
import qualified	BishBosh.Property.ForsythEdwards	as Property.ForsythEdwards
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	BishBosh.Types				as T
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.Char
import qualified	Data.List.Extra
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT
import qualified	Text.XML.HXT.Arrow.Pickle.Schema

-- | Used to qualify XML.
tag :: String
tag	= "piece"

-- | A number of /piece/s.
type NPieces	= Int	-- N.B.: 'Data.Int.Int8' saves neither time nor space.

-- | The initial number of pieces per side in a standard opening position; some of which are duplicates.
nPiecesPerSide :: NPieces
nPiecesPerSide	= fromIntegral Cartesian.Abscissa.xLength * 2 {-rows-}

-- | A Chess-piece has a /logical colour/ & a /rank/.
data Piece	= MkPiece {
	getLogicalColour	:: Attribute.LogicalColour.LogicalColour,
	getRank			:: Attribute.Rank.Rank
} deriving (Bounded, Eq, Ord)

instance Control.DeepSeq.NFData Piece where
	rnf MkPiece {
		getLogicalColour	= logicalColour,
		getRank			= rank
	} = Control.DeepSeq.rnf (logicalColour, rank)

instance Data.Array.IArray.Ix Piece where
	range (lower, upper)		= Control.Exception.assert (lower == minBound && upper == maxBound) range
	inRange (lower, upper) piece	= Control.Exception.assert (piece >= lower && piece <= upper) True
	index (lower, upper) MkPiece {
		getLogicalColour	= logicalColour,
		getRank			= rank
	} = Control.Exception.assert (lower == minBound && upper == maxBound) $ fromEnum logicalColour * Attribute.Rank.nDistinctRanks + fromEnum rank

instance Read Piece where
	readsPrec _	= Property.ForsythEdwards.readsFEN

instance Show Piece where
	showsPrec _	= Property.ForsythEdwards.showsFEN

instance Property.ForsythEdwards.ReadsFEN Piece where
	readsFEN s	= case Data.List.Extra.trimStart s of
		c : remainder	-> (
			MkPiece (
				if Data.Char.isUpper c
					then Attribute.LogicalColour.White
					else Attribute.LogicalColour.Black
			) *** const remainder
		 ) `map` reads [c]
		_		-> []	-- No parse.

instance Property.ForsythEdwards.ShowsFEN Piece where
	showsFEN piece@MkPiece { getRank = rank }	= showString . map (
		if isBlack piece
			then Data.Char.toLower	-- Only required for independence from the specific implementation of Read for Rank.
			else Data.Char.toUpper
	 ) $ show rank

instance HXT.XmlPickler Piece where
	xpickle	= HXT.xpWrap (read, show) . HXT.xpAttr tag . HXT.xpTextDT . Text.XML.HXT.Arrow.Pickle.Schema.scEnum $ map show range

instance Property.Opposable.Opposable Piece where
	getOpposite piece@MkPiece {
		getLogicalColour	= logicalColour
	} = piece {
		getLogicalColour	= Property.Opposable.getOpposite logicalColour
	}

-- | Constructor.
mkPiece :: Attribute.LogicalColour.LogicalColour -> Attribute.Rank.Rank -> Piece
mkPiece	= MkPiece

-- | Constructor.
mkPawn :: Attribute.LogicalColour.LogicalColour -> Piece
mkPawn		= (`MkPiece` Attribute.Rank.Pawn)

-- | Constructor.
mkRook :: Attribute.LogicalColour.LogicalColour -> Piece
mkRook		= (`MkPiece` Attribute.Rank.Rook)

-- | Constructor.
mkKnight :: Attribute.LogicalColour.LogicalColour -> Piece
mkKnight	= (`MkPiece` Attribute.Rank.Knight)

-- | Constructor.
mkBishop:: Attribute.LogicalColour.LogicalColour -> Piece
mkBishop	= (`MkPiece` Attribute.Rank.Bishop)

-- | Constructor.
mkQueen :: Attribute.LogicalColour.LogicalColour -> Piece
mkQueen		= (`MkPiece` Attribute.Rank.Queen)

-- | Constructor.
mkKing :: Attribute.LogicalColour.LogicalColour -> Piece
mkKing		= (`MkPiece` Attribute.Rank.King)

-- | The constant ascending range of /piece/s.
range :: [Piece]
range	= [
	MkPiece {
		getLogicalColour	= logicalColour,
		getRank			= rank
	} |
		logicalColour	<- Attribute.LogicalColour.range,
		rank		<- Attribute.Rank.range
 ] -- List-comprehension.

{- |
	* Changes the /rank/ of the specified /piece/, leaving the /logical colour/ unchanged.

	* CAVEAT: only legal if the /rank/ was a @Pawn@, & becomes neither a @Pawn@ nor a @King@.
-}
promote :: Attribute.Rank.Rank -> Piece -> Piece
promote newRank piece	= piece { getRank = newRank }

{- |
	* The constant /vector/s over which the specified type of /piece/ can attack.

	* CAVEAT: only defined for 'Attribute.Rank.fixedAttackRange'.

	* CAVEAT: it doesn't identify @Pawn@-advances, since these aren't attacks.
-}
attackVectorsByPiece :: (Num distance, Ord distance) => Data.Map.Map Piece [Cartesian.Vector.Vector distance]
attackVectorsByPiece	= Data.Map.fromAscList [
	(piece, vectors) |
		(piece, Just vectors) <- map (
			id &&& (
				\piece -> case getRank piece of
					Attribute.Rank.Pawn	-> Just . Cartesian.Vector.attackVectorsForPawn $ getLogicalColour piece
					Attribute.Rank.Knight	-> Just Cartesian.Vector.attackVectorsForKnight
					Attribute.Rank.King	-> Just Cartesian.Vector.attackVectorsForKing
					_			-> Nothing	-- These ranks attack over any distance.
			)
		) range
 ] -- List-comprehension.

{- |
	* The destinations available to those pieces with attack-vectors; @Pawn@, @Knight@, @King@.

	* CAVEAT: the destinations for a @Pawn@, are only those corresponding to diagonal attacks.

	* CAVEAT: this function has no knowledge of the /board/, & therefore of the position of any other piece.
-}
attackDestinationsByCoordinatesByRankByLogicalColour :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => Cartesian.Coordinates.ByCoordinates x y (Data.Map.Map Piece [Cartesian.Coordinates.Coordinates x y])
{-# SPECIALISE attackDestinationsByCoordinatesByRankByLogicalColour :: Cartesian.Coordinates.ByCoordinates T.X T.Y (Data.Map.Map Piece [Cartesian.Coordinates.Coordinates T.X T.Y]) #-}	-- To promote memoisation.
attackDestinationsByCoordinatesByRankByLogicalColour	= Cartesian.Coordinates.listArrayByCoordinates $ map (
	\source -> Data.Map.fromList [
		(
			piece,
			Data.Maybe.mapMaybe (Cartesian.Vector.maybeTranslate source) (attackVectorsByPiece Data.Map.! piece :: [Cartesian.Vector.VectorInt])
		) |
			logicalColour	<- Attribute.LogicalColour.range,
			rank		<- Attribute.Rank.fixedAttackRange,
			let piece	= mkPiece logicalColour rank
	] -- List-comprehension.
 ) Cartesian.Coordinates.range

-- | Calls 'attackVectorsByPiece' to find the destinations which the specified /piece/ can attack from the specified position.
findAttackDestinations :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 )
	=> Cartesian.Coordinates.Coordinates x y	-- ^ The source from which the attack originates.
	-> Piece
	-> [Cartesian.Coordinates.Coordinates x y]	-- ^ The destinations which can be attacked.
{-# NOINLINE findAttackDestinations #-}	-- Ensure the rewrite-rule triggers.
{-# RULES "findAttackDestinations/Int" findAttackDestinations = findAttackDestinationsInt #-}	-- CAVEAT: the call-stack leading to this function must be specialised to ensure this triggers.
findAttackDestinations source piece	= Data.Maybe.mapMaybe (Cartesian.Vector.maybeTranslate source) (attackVectorsByPiece Data.Map.! piece :: [Cartesian.Vector.VectorInt])

-- | A specialisation of 'findAttackDestinations', more efficiently implemented by calling 'attackDestinationsByCoordinatesByRankByLogicalColour'.
findAttackDestinationsInt :: Cartesian.Coordinates.Coordinates T.X T.Y -> Piece -> [Cartesian.Coordinates.Coordinates T.X T.Y]
findAttackDestinationsInt coordinates piece	= attackDestinationsByCoordinatesByRankByLogicalColour ! coordinates Data.Map.! piece

{- |
	* Find the constant directions of the straight lines along which each type of /piece/ can attack.

	* CAVEAT: not defined for a @Knight@.
-}
attackDirectionsByPiece :: Data.Map.Map Piece [Attribute.Direction.Direction]
attackDirectionsByPiece	= Data.Map.fromAscList [
	(
		piece,
		case getRank piece of
			Attribute.Rank.Pawn	-> Attribute.Direction.attackDirectionsForPawn $ getLogicalColour piece
			Attribute.Rank.Rook	-> Attribute.Direction.parallels
			Attribute.Rank.Bishop	-> Attribute.Direction.diagonals
			_ {-royalty-}		-> Attribute.Direction.range
	) |
		piece	<- range,
		not $ isKnight piece	-- The moves of which have no defined direction.
 ] -- List-comprehension.

{- |
	* Whether a /piece/ at the specified /coordinates/ could attack the target at the specified /coordinates/.

	* N.B.: doesn't require that the specified /piece/ actually exists at the target-location, nor that the victim's /logical colour/ is opposite from the attacker's.

	* N.B.: can't detect any blocking /piece/s which might invalidate the attack.

	* CAVEAT: it won't confirm the ability of a @Pawn@ to advance, since that doesn't constitute an attack.
-}
canAttackAlong
	:: (Enum x, Enum y)
	=> Cartesian.Coordinates.Coordinates x y	-- ^ Source (attacker's location).
	-> Cartesian.Coordinates.Coordinates x y	-- ^ Destination (victim's location).
	-> Piece					-- ^ Attacker.
	-> Bool
canAttackAlong source destination piece@MkPiece { getRank = rank }	= (
	case rank of
		Attribute.Rank.Pawn	-> Cartesian.Vector.isPawnAttack $ getLogicalColour piece
		Attribute.Rank.Knight	-> Cartesian.Vector.isKnightsMove
		Attribute.Rank.Bishop	-> Cartesian.Vector.isDiagonal
		Attribute.Rank.Rook	-> Cartesian.Vector.isParallel
		Attribute.Rank.Queen	-> Cartesian.Vector.isStraight
		Attribute.Rank.King	-> Cartesian.Vector.isKingsMove
 ) (
	Cartesian.Vector.measureDistance source destination	:: Cartesian.Vector.VectorInt
 )

{- |
	* Whether the specified /piece/ can move between the specified /coordinates/.

	* N.B.: can't detect any blocking pieces.
-}
canMoveBetween :: (
	Enum	x,
	Enum	y,
	Eq	y
 )
	=> Cartesian.Coordinates.Coordinates x y	-- ^ Source.
	-> Cartesian.Coordinates.Coordinates x y	-- ^ Destination.
	-> Piece
	-> Bool
{-# SPECIALISE canMoveBetween :: Cartesian.Coordinates.Coordinates T.X T.Y -> Cartesian.Coordinates.Coordinates T.X T.Y -> Piece -> Bool #-}
canMoveBetween source destination piece@MkPiece { getRank = rank }	= (
	case rank of
		Attribute.Rank.Pawn	-> \distance -> let
			logicalColour	= getLogicalColour piece
		 in Cartesian.Vector.isPawnAttack logicalColour distance || Cartesian.Vector.getXDistance distance == 0 && (
			let
				y'	= (
					if Attribute.LogicalColour.isBlack logicalColour
						then negate
						else id
				 ) $ Cartesian.Vector.getYDistance distance
			in y' == 1 || Cartesian.Coordinates.isPawnsFirstRank logicalColour source && y' == 2
		 )
		Attribute.Rank.Knight	-> Cartesian.Vector.isKnightsMove
		Attribute.Rank.Bishop	-> Cartesian.Vector.isDiagonal
		Attribute.Rank.Rook	-> Cartesian.Vector.isParallel
		Attribute.Rank.Queen	-> Cartesian.Vector.isStraight
		Attribute.Rank.King	-> Cartesian.Vector.isKingsMove
 ) (
	Cartesian.Vector.measureDistance source destination	:: Cartesian.Vector.VectorInt
 )

-- | Whether a move qualifies for @Pawn@-promotion.
isPawnPromotion
	:: (Enum y, Eq y)
	=> Cartesian.Coordinates.Coordinates x y	-- ^ Destination.
	-> Piece
	-> Bool
isPawnPromotion destination MkPiece {
	getLogicalColour	= logicalColour,
	getRank			= Attribute.Rank.Pawn
}			= Cartesian.Ordinate.lastRank logicalColour == Cartesian.Coordinates.getY destination
isPawnPromotion _ _	= False

-- | Whether the specified /piece/ is @Black@.
{-# INLINE isBlack #-}
isBlack :: Piece -> Bool
isBlack MkPiece { getLogicalColour = Attribute.LogicalColour.Black }	= True
isBlack _								= False

-- | Whether the specified /piece/s have the same /logical colour/.
{-# INLINE isFriend #-}
isFriend :: Piece -> Piece -> Bool
isFriend MkPiece { getLogicalColour = logicalColour } MkPiece { getLogicalColour = logicalColour' }	= logicalColour == logicalColour'

-- | Whether the specified /piece/s have the same /rank/.
isPeer :: Piece -> Piece -> Bool
isPeer MkPiece { getRank = rank } MkPiece { getRank = rank' }	= rank == rank'

-- | Whether the specified /piece/ is a @Pawn@.
{-# INLINE isPawn #-}
isPawn :: Piece -> Bool
isPawn MkPiece { getRank = Attribute.Rank.Pawn }	= True
isPawn _						= False

-- | Whether the specified /piece/ is a @Knight@.
{-# INLINE isKnight #-}
isKnight :: Piece -> Bool
isKnight MkPiece { getRank = Attribute.Rank.Knight }	= True
isKnight _						= False

-- | Whether the specified /piece/ is a @Bishop@.
isBishop :: Piece -> Bool
isBishop MkPiece { getRank = Attribute.Rank.Bishop }	= True
isBishop _						= False

-- | Whether the specified /piece/ is a @Rook@.
isRook :: Piece -> Bool
isRook MkPiece { getRank = Attribute.Rank.Rook }	= True
isRook _						= False

-- | Whether the specified /piece/ is a @Queen@.
isQueen :: Piece -> Bool
isQueen MkPiece { getRank = Attribute.Rank.Queen }	= True
isQueen _						= False

-- | Whether the specified /piece/ is a @King@.
{-# INLINE isKing #-}
isKing :: Piece -> Bool
isKing MkPiece { getRank = Attribute.Rank.King }	= True
isKing _						= False

-- | A boxed array indexed by /piece/, of unspecified elements.
type ByPiece	= Data.Array.IArray.Array {-Boxed-} Piece

-- | Array-constructor.
listArrayByPiece :: Data.Array.IArray.IArray a e => [e] -> a Piece e
listArrayByPiece	= Data.Array.IArray.listArray (minBound, maxBound)

-- | Self-documentation.
type LocatedPiece x y	= (Cartesian.Coordinates.Coordinates x y, Piece)

