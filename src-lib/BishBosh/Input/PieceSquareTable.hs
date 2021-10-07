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

	* This module is used to parse the user's configuration, which may involve reflecting their configuration to generate values for the RHS of the board.

	* This metric includes aspects of both control of the centre, & material advantage,
	in that a side's score can increase either by occupying squares of greater value, or simply by having more pieces.

	* N.B.: the evaluation of fitness by material COULD be entirely built into these tables, so that the average value for a @Queen@ is ~9 times that for a @Pawn@,
	but under these circumstances a non-zero material value for a @King@ must be arbitrarily chosen.

	* N.B. The normal & end-game phases are typically represented by independent instances.
	CAVEAT: any normalisation is performed on each of these instances independently, using their respective minimum & maximum values, rather than using the global minimum & maximum.
-}

module BishBosh.Input.PieceSquareTable(
-- * Types
-- ** Type-synonyms
--	Normalise,
--	ReflectOnY,
	Assocs,
-- ** Data-types
	PieceSquareTable(
--		MkPieceSquareTable,
--		getNormalise,
--		getReflectOnY,
		getPieceSquareValueByCoordinatesByRank
	),
-- * Constants
	tag,
	reflectOnYTag,
-- * Functions
	normaliseToUnitInterval,
	mirror,
	unmirror,
	findUndefinedRanks,
	dereference,
-- ** Constructors
	mkPieceSquareTable,
-- ** Predicates
	inClosedUnitInterval
) where

import			BishBosh.Data.Bool()	-- HXT.XmlPickler.
import			Control.Arrow((&&&), (***))
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa		as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Data.Exception			as Data.Exception
import qualified	BishBosh.Data.Foldable			as Data.Foldable
import qualified	BishBosh.Data.Num			as Data.Num
import qualified	BishBosh.Property.Empty			as Property.Empty
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Property.ShowFloat		as Property.ShowFloat
import qualified	BishBosh.Text.Case			as Text.Case
import qualified	BishBosh.Text.ShowList			as Text.ShowList
import qualified	BishBosh.Type.Mass			as Type.Mass
import qualified	Control.Arrow
import qualified	Control.Exception
import qualified	Data.Default
import qualified	Data.Foldable
import qualified	Data.Map.Strict				as Map
import qualified	Data.Set
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT

-- | Used to qualify XML.
tag :: String
tag		= "pieceSquareTable"

-- | Used to qualify XML.
normaliseTag :: String
normaliseTag	= "normalise"

-- | Used to qualify XML.
reflectOnYTag :: String
reflectOnYTag	= "reflectOnY"

-- | Type-synonym.
type Normalise	= Bool

-- | Type-synonym.
type ReflectOnY	= Bool

-- | Defines the value for each type of piece, of occupying each square.
data PieceSquareTable x y pieceSquareValue	= MkPieceSquareTable {
	getNormalise				:: Normalise,	-- ^ Whether to map the specified values into the closed unit-interval.	CAVEAT: incompatible with RelaxNG, the specification for which already constrains values to the unit-interval.
	getReflectOnY				:: ReflectOnY,	-- ^ Whether values for the RHS of the board should be inferred by reflection about the y-axis.
	getPieceSquareValueByCoordinatesByRank	:: Map.Map Attribute.Rank.Rank (
		Cartesian.Coordinates.ArrayByCoordinates x y pieceSquareValue
	)							-- ^ N.B.: on the assumption that the values for Black pieces are the reflection of those for White, merely the /rank/ of each /piece/ need be defined.
} deriving (Eq, Show)

instance (Real pieceSquareValue, Show pieceSquareValue) => Property.ShowFloat.ShowFloat (PieceSquareTable x y pieceSquareValue) where
	showsFloat fromDouble MkPieceSquareTable {
		getNormalise				= normalise,
		getReflectOnY				= reflectOnY,
		getPieceSquareValueByCoordinatesByRank	= byRank
	} = Text.ShowList.showsAssociationList Text.ShowList.showsSeparator $ (
		normaliseTag,
		shows normalise
	 ) : (
		reflectOnYTag,
		shows reflectOnY
	 ) : map (
		show {-rank-} *** Text.ShowList.showsFormattedList' (
			fromDouble . realToFrac
		) . (
			if reflectOnY
				then unmirror
				else id
		) . Data.Foldable.toList
	 ) (
		Map.toList byRank
	 )

instance Data.Default.Default (PieceSquareTable x y pieceSquareValue) where
	def = MkPieceSquareTable {
		getNormalise				= False,
		getReflectOnY				= True,
		getPieceSquareValueByCoordinatesByRank	= Property.Empty.empty
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
		\(a, b, c)	-> mkPieceSquareTable a b c,	-- Construct.
		\MkPieceSquareTable {
			getNormalise				= normalise,
			getReflectOnY				= reflectOnY,
			getPieceSquareValueByCoordinatesByRank	= byRank
		} -> (
			normalise,
			reflectOnY,
			Map.toList $ Map.map (
				(
					if reflectOnY
						then unmirror
						else id
				) . Data.Foldable.toList
			) byRank
		) -- Deconstruct to tuple.
	 ) . HXT.xpTriple (
		getNormalise Data.Default.def `HXT.xpDefault` HXT.xpAttr normaliseTag HXT.xpickle {-Bool-}
	 ) (
		getReflectOnY Data.Default.def `HXT.xpDefault` HXT.xpAttr reflectOnYTag HXT.xpickle {-Bool-}
	 ) $ HXT.xpList1 (
		HXT.xpElem (
			showString "by" $ Text.Case.toUpperInitial Attribute.Rank.tag
		) $ HXT.xpickle {-rank-} `HXT.xpPair` HXT.xpWrap (
			\s -> [
				realToFrac (pieceSquareValue :: Type.Mass.PieceSquareValue) |
					word			<- words s,
					(pieceSquareValue, "")	<- reads word
			], -- List-comprehension.
			unwords . map (show . (\pieceSquareValue -> realToFrac pieceSquareValue :: Type.Mass.PieceSquareValue))
		) (
			HXT.xpTextAttr . showString "by" $ Text.Case.toUpperInitial Cartesian.Coordinates.tag
		)
	 )

-- | Type-synonym.
type Assocs rank pieceSquareValue	= [(rank, [pieceSquareValue])]

-- | Map the range of values onto the Closed Unit Interval.
normaliseToUnitInterval
	:: (Fractional pieceSquareValue, Ord pieceSquareValue)
	=> Assocs rank pieceSquareValue
	-> Assocs rank pieceSquareValue
normaliseToUnitInterval []	= []
normaliseToUnitInterval assocs
	| range == 0	= Control.Exception.throw $ Data.Exception.mkNullDatum "BishBosh.Input.PieceSquareTable.normaliseToUnitInterval:\tthe specified piece-square values are identical."
	| otherwise	= map (
		Control.Arrow.second $ map ((/ range) . subtract minimum')
	) assocs
	where
		bounds@(minimum', _)	= minimum &&& maximum $ concatMap snd assocs	-- Analyse the range of values.
		range			= uncurry subtract bounds

-- | Check that the range of values is in the Closed Unit Interval.
inClosedUnitInterval
	:: (Num pieceSquareValue, Ord pieceSquareValue)
	=> Assocs rank pieceSquareValue
	-> Bool
inClosedUnitInterval	= all $ all Data.Num.inClosedUnitInterval . snd {-[pieceSquareValue]-}

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
	Enum		x,
	Enum		y,
	Fractional	pieceSquareValue,
	Ord		pieceSquareValue,
	Ord		x,
	Ord		y,
	Show		pieceSquareValue
 )
	=> Normalise	-- ^ Whether to normalise the specified values into the closed unit interval.
	-> ReflectOnY	-- ^ Whether values for the RHS of the board are inferred by reflection about the y-axis.
	-> Assocs Attribute.Rank.Rank pieceSquareValue
	-> PieceSquareTable x y pieceSquareValue
mkPieceSquareTable normalise reflectOnY assocs
	| any (
		(/= nValuesRequired) . length . snd {-pieceSquareValues-}
	) assocs						= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Input.PieceSquareTable.mkPieceSquareTable:\texactly " . shows nValuesRequired . showString " values must be defined for each type of piece; " $ shows assocs "."
	| not $ null duplicateRanks				= Control.Exception.throw . Data.Exception.mkDuplicateData . showString "BishBosh.Input.PieceSquareTable.mkPieceSquareTable:\tranks must be distinct; " $ shows duplicateRanks "."
	| not $ normalise || inClosedUnitInterval assocs	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.PieceSquareTable.mkPieceSquareTable:\tall values must be within the closed unit-interval [0,1]; " $ shows assocs "."
	| otherwise						= MkPieceSquareTable {
		getNormalise				= normalise,
		getReflectOnY				= reflectOnY,
		getPieceSquareValueByCoordinatesByRank	= Map.fromList . map (
			Control.Arrow.second Cartesian.Coordinates.listArrayByCoordinates
		) . (
			if reflectOnY
				then map $ Control.Arrow.second mirror
				else id
		) $ (
			if normalise
				then normaliseToUnitInterval
				else id
		) assocs
	}
	where
		duplicateRanks	= Data.Foldable.findDuplicates $ map fst assocs

		nValuesRequired	= (
			if reflectOnY
				then (`div` 2)
				else id
		 ) Cartesian.Coordinates.nSquares

-- | Identify any /rank/ lacking a definition.
findUndefinedRanks :: PieceSquareTable x y pieceSquareValue -> Data.Set.Set Attribute.Rank.Rank
findUndefinedRanks MkPieceSquareTable { getPieceSquareValueByCoordinatesByRank = pieceSquareValueByCoordinatesByRank }	= Data.Set.fromAscList Property.FixedMembership.members `Data.Set.difference` Map.keysSet pieceSquareValueByCoordinatesByRank

-- | Lookup the values for all /coordinates/, corresponding to the specified /rank/.
dereference
	:: Attribute.Rank.Rank
	-> PieceSquareTable x y pieceSquareValue
	-> Maybe (Cartesian.Coordinates.ArrayByCoordinates x y pieceSquareValue)
dereference rank MkPieceSquareTable { getPieceSquareValueByCoordinatesByRank = pieceSquareValueByCoordinatesByRank }	= Map.lookup rank pieceSquareValueByCoordinatesByRank

