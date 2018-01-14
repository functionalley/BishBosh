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

	* Defines the value for each type of piece, of occupying different squares.

	* This metric includes aspects of both control of the centre, & material advantage,
	in that a side's score can increase either by occupying more valuable squares or simply by having more pieces.

	* N.B.: the evaluation of fitness by "material" COULD be entirely built into these tables, so that the average value for a @Queen@ is ~9 times that for a @Pawn@,
	but under these circumstances a non-zero material value for a @King@ must be arbitrarily chosen.

	* N.B. The normal & end-game phases are typically represented by independent instances.
-}

module BishBosh.Input.PieceSquareTable(
-- * Types
-- ** Data-types
	PieceSquareTable(
--		MkPieceSquareTable,
--		getReflectOnY,
		getByRank
	),
-- * Constants
	tag,
	reflectOnYTag,
-- * Functions
--	mirror,
--	unmirror,
	findUndefinedRanks,
	dereference,
-- ** Constructors
	mkPieceSquareTable
) where

import			BishBosh.Data.Bool()	-- HXT.XmlPickler.
import			Control.Arrow((***))
import qualified	BishBosh.Attribute.Rank		as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa	as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates	as Cartesian.Coordinates
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Data.Num		as Data.Num
import qualified	BishBosh.Property.ShowFloat	as Property.ShowFloat
import qualified	BishBosh.Text.ShowList		as Text.ShowList
import qualified	Control.Arrow
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.Default
import qualified	Data.Map
import qualified	Data.Set
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT

-- | Used to qualify XML.
tag :: String
tag		= "pieceSquareTable"

-- | Used to qualify XML.
reflectOnYTag :: String
reflectOnYTag	= "reflectOnY"

-- | Defines the value for each type of piece, of occupying each square.
data PieceSquareTable x y pieceSquareValue	= MkPieceSquareTable {
	getReflectOnY	:: Bool,	-- ^ Whether values for the RHS of the board should be inferred by reflection about the y-axis.
	getByRank	:: Data.Map.Map Attribute.Rank.Rank (
		Cartesian.Coordinates.ByCoordinates x y pieceSquareValue
	)				-- ^ N.B.: on the assumption that the values for Black pieces are the reflection of those for White, merely the /rank/ of each /piece/ need be defined.
} deriving (Eq, Show)

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Real	pieceSquareValue,
	Show	pieceSquareValue
 ) => Property.ShowFloat.ShowFloat (PieceSquareTable x y pieceSquareValue) where
	showsFloat fromDouble MkPieceSquareTable {
		getReflectOnY	= reflectOnY,
		getByRank	= byRank
	} = Text.ShowList.showsAssociationList Text.ShowList.showsSeparator $ (
		reflectOnYTag,
		shows reflectOnY
	 ) : map (
		show {-rank-} *** Text.ShowList.showsFormattedList' (
			fromDouble . realToFrac
		) . (
			if reflectOnY
				then unmirror
				else id
		) . Data.Array.IArray.elems
	 ) (
		Data.Map.assocs byRank
	 )

instance Data.Default.Default (PieceSquareTable x y pieceSquareValue) where
	def = MkPieceSquareTable {
		getReflectOnY	= True,
		getByRank	= Data.Map.empty
	}

instance (
	Enum		x,
	Enum		y,
	Fractional	pieceSquareValue,
	Ord		pieceSquareValue,
	Ord		x,
	Ord		y,
	Real		pieceSquareValue,
	Show		pieceSquareValue
 ) => HXT.XmlPickler (PieceSquareTable x y pieceSquareValue) where
	xpickle	= HXT.xpWrap (
		uncurry mkPieceSquareTable,
		\MkPieceSquareTable {
			getReflectOnY	= reflectOnY,
			getByRank	= byRank
		} -> (
			reflectOnY,
			Data.Map.assocs $ Data.Map.map (
				(
					if reflectOnY
						then unmirror
						else id
				) . Data.Array.IArray.elems
			) byRank
		) -- Pair.
	 ) $ (
		getReflectOnY Data.Default.def `HXT.xpDefault` HXT.xpAttr reflectOnYTag HXT.xpickle {-Bool-}
	 ) `HXT.xpPair` HXT.xpList1 (
		HXT.xpElem "byRank" $ HXT.xpickle {-rank-} `HXT.xpPair` HXT.xpWrap (
			\s -> [
				realToFrac (pieceSquareValue :: Double) |
					word			<- words s,
					(pieceSquareValue, "")	<- reads word
			], -- List-comprehension.
			unwords . map (show . (\pieceSquareValue -> realToFrac pieceSquareValue :: Double))
		 ) (HXT.xpTextAttr "byCoordinates")
	 )

-- | Generates a mirror-symmetric RHS, to build a complete description.
mirror :: Show pieceSquareValue => [pieceSquareValue] -> [pieceSquareValue]
mirror (a : b : c : d : remainder)	= a : b : c : d : d : c : b : a : mirror remainder
mirror []				= []
mirror pieceSquareValues		= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Input.PieceSquareTable.mirror:\tthe number of piece-square values must be a multiple of " . shows (Cartesian.Abscissa.xLength `div` 2) . showString "; " $ shows pieceSquareValues "."

-- | Removes the mirror-symmetric RHS, for a concise description.
unmirror :: Show pieceSquareValue => [pieceSquareValue] -> [pieceSquareValue]
unmirror (a : b : c : d : remainder)	= a : b : c : d : unmirror (drop (fromIntegral Cartesian.Abscissa.xLength `div` 2) remainder)
unmirror []				= []
unmirror pieceSquareValues		= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Input.PieceSquareTable.unmirror:\tthe number of piece-square values must be a multiple of " . shows (Cartesian.Abscissa.xLength `div` 2) . showString "; " $ shows pieceSquareValues "."

-- | Smart constructor.
mkPieceSquareTable :: (
	Enum	x,
	Enum	y,
	Num	pieceSquareValue,
	Ord	pieceSquareValue,
	Ord	x,
	Ord	y,
	Show	pieceSquareValue
 )
	=> Bool	-- ^ Whether values for the RHS of the board are inferred by reflection about the y-axis.
	-> [(Attribute.Rank.Rank, [pieceSquareValue])]
	-> PieceSquareTable x y pieceSquareValue
mkPieceSquareTable reflectOnY assocs
	| any (
		(/= nValuesRequired) . length . snd {-pieceSquareValues-}
	) assocs	= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Input.PieceSquareTable.mkPieceSquareTable:\texactly " . shows nValuesRequired . showString " values must be defined for each type of piece; " $ shows assocs "."
	| any (
		any (
			not . Data.Num.inClosedUnitInterval
		) . snd {-list-}
	) assocs	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.PieceSquareTable.mkPieceSquareTable:\tall values must be within the closed unit-interval [0,1]; " $ shows assocs "."
	| otherwise	= MkPieceSquareTable {
		getReflectOnY	= reflectOnY,
		getByRank	= Data.Map.fromList . map (Control.Arrow.second Cartesian.Coordinates.listArrayByCoordinates) $ (
			if reflectOnY
				then map $ Control.Arrow.second mirror
				else id
		) assocs
	}
	where
		nValuesRequired	= (
			if reflectOnY
				then (`div` 2)
				else id
		 ) Cartesian.Coordinates.nSquares

-- | Identify any /rank/ lacking a definition.
findUndefinedRanks :: PieceSquareTable x y pieceSquareValue -> Data.Set.Set Attribute.Rank.Rank
findUndefinedRanks MkPieceSquareTable { getByRank = byRank }	= Data.Set.fromAscList Attribute.Rank.range `Data.Set.difference` Data.Map.keysSet byRank

-- | Lookup the value for the specified /rank/.
dereference :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => Attribute.Rank.Rank -> PieceSquareTable x y pieceSquareValue -> Maybe [pieceSquareValue]
dereference rank MkPieceSquareTable { getByRank = byRank}	= Data.Array.IArray.elems `fmap` Data.Map.lookup rank byRank

