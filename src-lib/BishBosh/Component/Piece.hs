{-# LANGUAGE LambdaCase #-}
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
--	ByRankByLogicalColour,
--	AttackDestinationsByCoordinatesByRankByLogicalColour,
	ArrayByPiece,
	LocatedPiece,
-- ** Data-types
	Piece(
--		MkPiece,
		getLogicalColour,
		getRank
	),
-- * Constants
--	tag,
	range,
	nPiecesPerSide,
	epdCharacterSet,
--	attackVectorsByRankByLogicalColour,
--	attackDirectionsByRankByLogicalColour,
--	attackDestinationsByCoordinatesByRankByLogicalColour,
-- * Functions
--	findAttackDestinations',
	findAttackDestinations,
	showPieces,
-- ** Accessors
	getAttackDirections,
-- ** Mutators
	promote,
-- ** Constructors
--	mkByRankByLogicalColour,
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
	isQueen,
	isKing,
	isPawnPromotion
) where

import			Control.Arrow((&&&), (***))
import			Data.Array.IArray((!))
import qualified	BishBosh.Attribute.Rank				as Attribute.Rank
import qualified	BishBosh.Cartesian.Coordinates			as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate			as Cartesian.Ordinate
import qualified	BishBosh.Cartesian.Vector			as Cartesian.Vector
import qualified	BishBosh.Colour.LogicalColour			as Colour.LogicalColour
import qualified	BishBosh.Data.Exception				as Data.Exception
import qualified	BishBosh.Direction.Direction			as Direction.Direction
import qualified	BishBosh.Property.ExtendedPositionDescription	as Property.ExtendedPositionDescription
import qualified	BishBosh.Property.FixedMembership		as Property.FixedMembership
import qualified	BishBosh.Property.ForsythEdwards		as Property.ForsythEdwards
import qualified	BishBosh.Property.Opposable			as Property.Opposable
import qualified	BishBosh.Property.Orientated			as Property.Orientated
import qualified	BishBosh.Type.Count				as Type.Count
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.Char
import qualified	Data.Foldable
import qualified	Data.List.Extra
import qualified	Data.Map					as Map
import qualified	Data.Maybe
import qualified	Text.XML.HXT.Arrow.Pickle			as HXT
import qualified	Text.XML.HXT.Arrow.Pickle.Schema

-- | Used to qualify XML.
tag :: String
tag	= "piece"

-- | The constant number of pieces per side at the conventional opening position; some of which are duplicates.
nPiecesPerSide :: Type.Count.NPieces
nPiecesPerSide	= Data.Foldable.foldl' (+) 0 Attribute.Rank.initialAllocationByRankPerSide

-- | A Chess-piece has a /logical colour/ & a /rank/.
data Piece	= MkPiece {
	getLogicalColour	:: Colour.LogicalColour.LogicalColour,
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
	} = Control.Exception.assert (lower == minBound && upper == maxBound) $ fromEnum logicalColour * fromIntegral Attribute.Rank.nDistinctRanks + fromEnum rank

instance Read Piece where
	readsPrec _	= Property.ForsythEdwards.readsFEN

instance Show Piece where
	showsPrec _	= Property.ForsythEdwards.showsFEN

-- | The constant set of permissible characters in an EPD.
epdCharacterSet	:: Property.ExtendedPositionDescription.EPD
epdCharacterSet	= concatMap Property.ExtendedPositionDescription.showEPD range

instance Property.ExtendedPositionDescription.ReadsEPD Piece where
	readsEPD s	= case Data.List.Extra.trimStart s of
		c : remainder	-> (
			MkPiece (
				if Data.Char.isUpper c
					then Colour.LogicalColour.White
					else Colour.LogicalColour.Black
			) *** const remainder
		 ) `map` reads [c]
		_		-> []	-- No parse.

instance Property.ExtendedPositionDescription.ShowsEPD Piece where
	showsEPD piece@MkPiece { getRank = rank }	= showString . map (
		if isBlack piece
			then Data.Char.toLower	-- Only required for independence from the specific implementation of Read for Rank.
			else Data.Char.toUpper
	 ) $ show rank

instance Property.ForsythEdwards.ReadsFEN Piece

instance Property.ForsythEdwards.ShowsFEN Piece

instance HXT.XmlPickler Piece where
	xpickle	= HXT.xpWrap (read, show) . HXT.xpAttr tag . HXT.xpTextDT . Text.XML.HXT.Arrow.Pickle.Schema.scEnum $ map show range

instance Property.Opposable.Opposable Piece where
	getOpposite piece@MkPiece {
		getLogicalColour	= logicalColour
	} = piece {
		getLogicalColour	= Property.Opposable.getOpposite logicalColour
	}

-- | The constant ascending range of /piece/s.
range :: [Piece]
range	= [
	MkPiece {
		getLogicalColour	= logicalColour,
		getRank			= rank
	} |
		logicalColour	<- Property.FixedMembership.members,
		rank		<- Property.FixedMembership.members
 ] -- List-comprehension.

-- | Returns a constant string containing all possible pieces.
showPieces :: String
showPieces	= concatMap show range

instance Property.FixedMembership.FixedMembership Piece where
	members	= range

-- | Constructor.
mkPiece :: Colour.LogicalColour.LogicalColour -> Attribute.Rank.Rank -> Piece
mkPiece	= MkPiece

-- | Constructor.
mkPawn :: Colour.LogicalColour.LogicalColour -> Piece
mkPawn	= (`MkPiece` Attribute.Rank.Pawn)

-- | Constructor.
mkRook :: Colour.LogicalColour.LogicalColour -> Piece
mkRook	= (`MkPiece` Attribute.Rank.Rook)

-- | Constructor.
mkKnight :: Colour.LogicalColour.LogicalColour -> Piece
mkKnight	= (`MkPiece` Attribute.Rank.Knight)

-- | Constructor.
mkBishop:: Colour.LogicalColour.LogicalColour -> Piece
mkBishop	= (`MkPiece` Attribute.Rank.Bishop)

-- | Constructor.
mkQueen :: Colour.LogicalColour.LogicalColour -> Piece
mkQueen	= (`MkPiece` Attribute.Rank.Queen)

-- | Constructor.
mkKing :: Colour.LogicalColour.LogicalColour -> Piece
mkKing	= (`MkPiece` Attribute.Rank.King)

-- | Changes the specified /piece/ to the specified /rank/ leaving its /logical colour/ unchanged.
promote :: Attribute.Rank.Rank -> Piece -> Piece
promote newRank piece
	| not $ isPawn piece					= Control.Exception.throw . Data.Exception.mkIncompatibleData . showString "BishBosh.Component.Piece.promote:\tcan't promote a " $ shows piece "."
	| newRank `notElem` Attribute.Rank.promotionProspects	= Control.Exception.throw . Data.Exception.mkIncompatibleData . showString "BishBosh.Component.Piece.promote:\tcan't promote to a " $ shows newRank "."
	| otherwise						= piece { getRank = newRank }

-- | The structure of a container of arbitrary data, indexed by /logicalColour/ & some /rank/s.
type ByRankByLogicalColour element	= Colour.LogicalColour.ArrayByLogicalColour (Map.Map Attribute.Rank.Rank element)

-- | Constructor of a certain shape of container, but with arbitrary contents.
mkByRankByLogicalColour
	:: [Attribute.Rank.Rank]
	-> (Colour.LogicalColour.LogicalColour -> Attribute.Rank.Rank -> element)
	-> ByRankByLogicalColour element
mkByRankByLogicalColour ranks mkElement	= Colour.LogicalColour.listArrayByLogicalColour $ map (
	\logicalColour	-> Map.fromList $ map (id &&& mkElement logicalColour) ranks
 ) Property.FixedMembership.members

{- |
	* The constant /vector/s over which the specified type of /piece/ can attack.

	* CAVEAT: only defined for 'Attribute.Rank.fixedAttackRange'.

	* CAVEAT: it doesn't identify @Pawn@-advances, since these aren't attacks.
-}
attackVectorsByRankByLogicalColour :: ByRankByLogicalColour [Cartesian.Vector.Vector]
attackVectorsByRankByLogicalColour	= mkByRankByLogicalColour Attribute.Rank.fixedAttackRange $ \logicalColour -> \case
	Attribute.Rank.Pawn	-> Cartesian.Vector.attackVectorsForPawn logicalColour
	Attribute.Rank.Knight	-> Cartesian.Vector.attackVectorsForKnight
	Attribute.Rank.King	-> Cartesian.Vector.attackVectorsForKing
	rank			-> error . showString "BishBosh.Component.Piece.attackVectorsByRankByLogicalColour:\trank must attack over fixed range; " $ shows rank "."	-- These ranks attack over any distance.

-- | The destinations available to those pieces with attack-vectors; @Pawn@, @Knight@, @King@.
type AttackDestinationsByCoordinatesByRankByLogicalColour	= ByRankByLogicalColour (Cartesian.Coordinates.ArrayByCoordinates [Cartesian.Coordinates.Coordinates])

-- | Calls 'attackVectorsByRankByLogicalColour' to find the destinations which the specified /piece/ can attack from the specified position.
findAttackDestinations'
	:: Piece
	-> Cartesian.Coordinates.Coordinates	-- ^ The source from which the attack originates.
	-> [Cartesian.Coordinates.Coordinates]	-- ^ The destinations which can be attacked.
findAttackDestinations' MkPiece {
	getLogicalColour	= logicalColour,
	getRank			= rank
} source	= Data.Maybe.mapMaybe (
	`Cartesian.Vector.maybeTranslate` source
 ) (
	attackVectorsByRankByLogicalColour ! logicalColour Map.! rank
 )

{- |
	* The destinations available to those pieces with attack-vectors; @Pawn@, @Knight@, @King@.

	* CAVEAT: the destinations for a @Pawn@, are only those corresponding to diagonal attacks.

	* CAVEAT: this function has no knowledge of the /board/, & therefore of the position of any other piece.
-}
attackDestinationsByCoordinatesByRankByLogicalColour :: AttackDestinationsByCoordinatesByRankByLogicalColour
attackDestinationsByCoordinatesByRankByLogicalColour	= mkByRankByLogicalColour Attribute.Rank.fixedAttackRange $ \logicalColour rank -> Cartesian.Coordinates.listArrayByCoordinates $ map (
	findAttackDestinations' $! mkPiece logicalColour rank
 ) Property.FixedMembership.members

-- | Find the destinations which the specified /piece/ can attack from the specified position.
findAttackDestinations
	:: Piece
	-> Cartesian.Coordinates.Coordinates	-- ^ The source from which the attack originates.
	-> [Cartesian.Coordinates.Coordinates]	-- ^ The destinations which can be attacked.
findAttackDestinations MkPiece {
	getLogicalColour	= logicalColour,
	getRank			= rank
} coordinates	= attackDestinationsByCoordinatesByRankByLogicalColour ! logicalColour Map.! rank ! coordinates

-- The constant /direction/s of the straight lines along which each type of /piece/ can attack.


{- |
	* The constant /direction/s of the straight lines along which each type of /piece/ can attack.

	* CAVEAT: not defined for a @Knight@.
-}
attackDirectionsByRankByLogicalColour :: ByRankByLogicalColour [Direction.Direction.Direction]
attackDirectionsByRankByLogicalColour	= mkByRankByLogicalColour Attribute.Rank.earthBound $ \logicalColour -> \case
	Attribute.Rank.Pawn	-> Direction.Direction.attackDirectionsForPawn logicalColour
	Attribute.Rank.Bishop	-> Direction.Direction.diagonals
	Attribute.Rank.Rook	-> Direction.Direction.parallels
	_ {-royalty-}		-> Property.FixedMembership.members {-directions-}

{- |
	* Get the constant /direction/s of the straight lines along which the specified /piece/ can attack.

	* CAVEAT: not defined for a @Knight@.
-}
getAttackDirections :: Piece -> [Direction.Direction.Direction]
getAttackDirections MkPiece {
	getLogicalColour	= logicalColour,
	getRank			= rank
} = attackDirectionsByRankByLogicalColour ! logicalColour Map.! rank

{- |
	* Whether a /piece/ at the specified /coordinates/ could attack the target at the specified /coordinates/.

	* N.B.: doesn't require that the specified /piece/ actually exists at the target-location, nor that the victim's /logical colour/ is opposite from the attacker's.

	* N.B.: can't detect any blocking /piece/s which might invalidate the attack.

	* CAVEAT: it won't confirm the ability of a @Pawn@ to advance, since that doesn't constitute an attack.
-}
canAttackAlong
	:: Cartesian.Coordinates.Coordinates	-- ^ Source (attacker's location).
	-> Cartesian.Coordinates.Coordinates	-- ^ Destination (victim's location).
	-> Piece				-- ^ Attacker.
	-> Bool
canAttackAlong source destination piece	= (
	case getRank piece of
		Attribute.Rank.Pawn	-> (`Cartesian.Vector.isPawnAttack` getLogicalColour piece)
		Attribute.Rank.Knight	-> Cartesian.Vector.isKnightsMove
		Attribute.Rank.Bishop	-> Property.Orientated.isDiagonal
		Attribute.Rank.Rook	-> Property.Orientated.isParallel
		Attribute.Rank.Queen	-> Property.Orientated.isStraight
		Attribute.Rank.King	-> Cartesian.Vector.isKingsMove
 ) $! Cartesian.Vector.measureDistance source destination

{- |
	* Whether the specified /piece/ can move between the specified /coordinates/.

	* N.B.: can't detect any blocking pieces.
-}
canMoveBetween
	:: Piece
	-> Cartesian.Coordinates.Coordinates	-- ^ Source.
	-> Cartesian.Coordinates.Coordinates	-- ^ Destination.
	-> Bool
canMoveBetween piece source destination	= (
	case getRank piece of
		Attribute.Rank.Pawn	-> let
			logicalColour	= getLogicalColour piece
		 in uncurry (||) . (
			(`Cartesian.Vector.isPawnAttack` logicalColour) &&& (
				uncurry (&&) . (
					(== 0) . Cartesian.Vector.getXDistance &&& (
						\case
							1	-> True
							2	-> Cartesian.Coordinates.isPawnsFirstRank source logicalColour
							_	-> False
					) . (
						if Colour.LogicalColour.isBlack logicalColour
							then negate
							else id
					) . Cartesian.Vector.getYDistance
				)
			)
		 )
		Attribute.Rank.Knight	-> Cartesian.Vector.isKnightsMove
		Attribute.Rank.Bishop	-> Property.Orientated.isDiagonal
		Attribute.Rank.Rook	-> Property.Orientated.isParallel
		Attribute.Rank.Queen	-> Property.Orientated.isStraight
		Attribute.Rank.King	-> Cartesian.Vector.isKingsMove
 ) $! Cartesian.Vector.measureDistance source destination

-- | Whether a move qualifies for @Pawn@-promotion.
isPawnPromotion
	:: Piece
	-> Cartesian.Coordinates.Coordinates	-- ^ Destination.
	-> Bool
isPawnPromotion MkPiece {
	getLogicalColour	= logicalColour,
	getRank			= Attribute.Rank.Pawn
} destination		= Cartesian.Ordinate.lastRank logicalColour == Cartesian.Coordinates.getY destination
isPawnPromotion _ _	= False

-- | Whether the specified /piece/ is @Black@.
{-# INLINE isBlack #-}
isBlack :: Piece -> Bool
isBlack MkPiece { getLogicalColour = Colour.LogicalColour.Black }	= True
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
type ArrayByPiece	= Data.Array.IArray.Array {-Boxed-} Piece

-- | Array-constructor.
listArrayByPiece :: Data.Array.IArray.IArray a e => [e] -> a Piece e
listArrayByPiece	= Data.Array.IArray.listArray (minBound, maxBound)

-- | A /piece/ at specific /coordinates/.
type LocatedPiece	= (Cartesian.Coordinates.Coordinates, Piece)

