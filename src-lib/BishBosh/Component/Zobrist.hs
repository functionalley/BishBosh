{-# LANGUAGE MultiParamTypeClasses #-}
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

	* <https://www.chessprogramming.org/Zobrist_Hashing>.

	* Defines the random-numbers required to construct a hash of a chess-position.

	* Facilitates the construction of a hash from arbitrary data.
-}

module BishBosh.Component.Zobrist(
-- * Type-classes
	Hashable1D(..),
	Hashable2D(..),
-- * Types
-- ** Type-synonyms
--	Index,
-- ** Data-types
	Zobrist(
--		MkZobrist,
		getRandomForBlacksMove
--		getRandomByCoordinatesByRankByLogicalColour,
--		getRandomByCastleableRooksXByLogicalColour,
--		getRandomByEnPassantAbscissa
	),
-- * Constants
--	combiningOp,
-- * Functions
--	measureHammingDistances,
	dereferenceRandomByCoordinatesByRankByLogicalColour,
	dereferenceRandomByCastleableRooksXByLogicalColour,
	dereferenceRandomByEnPassantAbscissa,
	hash2D,
	combine,
-- ** Constructors
	mkZobrist
) where

import			Control.Arrow((***))
import			Data.Array.IArray((!))
import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa		as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Data.Exception			as Data.Exception
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.Bits
import qualified	Data.Default
import qualified	Data.Foldable
import qualified	Data.List
import qualified	System.Random
import qualified	ToolShed.System.Random

-- | Used as an index into 'getRandomByCoordinatesByRankByLogicalColour'.
type Index x y	= (Attribute.LogicalColour.LogicalColour, Attribute.Rank.Rank, Cartesian.Coordinates.Coordinates x y)

-- | The random numbers used to generate a hash, which almost uniquely represent a /position/.
data Zobrist x y positionHash	= MkZobrist {
	getRandomForBlacksMove				:: positionHash,							-- ^ Defines a random number to apply when the next move is @Black@'s.
	getRandomByCoordinatesByRankByLogicalColour	:: Data.Array.IArray.Array {-Boxed-} (Index x y) positionHash,		-- ^ Defines random numbers to represent all combinations of each piece at each coordinate; though @Pawn@s can't exist on the terminal ranks. N.B.: regrettably the array can't be unboxed, because 'Data.Array.Unboxed.UArray' isn't 'Foldable'; cf. 'Data.Array.IArray.Array'.

	getRandomByCastleableRooksXByLogicalColour	:: Attribute.LogicalColour.ArrayByLogicalColour [(x, positionHash)],	-- ^ Defines random numbers to represent all combinations of castleable @Rook@s.
	getRandomByEnPassantAbscissa			:: Cartesian.Abscissa.ArrayByAbscissa x positionHash			-- ^ Defines random numbers to represent any file on which capture en-passant might be available.
} deriving Show

instance Foldable (Zobrist x y) where
	foldr f i MkZobrist {
		getRandomForBlacksMove				= randomForBlacksMove,
		getRandomByCoordinatesByRankByLogicalColour	= randomByCoordinatesByRankByLogicalColour,
		getRandomByCastleableRooksXByLogicalColour	= randomByCastleableRooksXByLogicalColour,
		getRandomByEnPassantAbscissa			= randomByEnPassantAbscissa
	} = Data.Foldable.foldr f (
		Data.Foldable.foldr (
			flip . foldr $ f . snd
		) (
			Data.Foldable.foldr f (
				f randomForBlacksMove i
			) randomByCoordinatesByRankByLogicalColour
		) randomByCastleableRooksXByLogicalColour
	 ) randomByEnPassantAbscissa

instance (
	Data.Array.IArray.Ix	x,
	Data.Bits.FiniteBits	positionHash,
	Enum			x,
	Enum			y,
	Ord			y,
	System.Random.Random	positionHash
 ) => Data.Default.Default (Zobrist x y positionHash) where
	def	= mkZobrist Nothing $ System.Random.mkStdGen 0

{- |
	* <https://www.chessprogramming.org/Population_Count#HammingDistance>.

	* Quantifies the Hamming distance between all combinations of pairs of the random numbers used to compose hashes.

	* CAVEAT: <https://en.wikipedia.org/wiki/Linear_independence> a better measure of the suitability of the selected random numbers
-}
measureHammingDistances :: Data.Bits.Bits positionHash => Zobrist x y positionHash -> [Int]
measureHammingDistances	= map (Data.Bits.popCount . uncurry Data.Bits.xor) . getCombinations . Data.Foldable.toList where
	getCombinations :: [a] -> [(a, a)]
	getCombinations (x : remainder)	= map ((,) x) remainder ++ getCombinations remainder	-- CAVEAT: O(n^2) time-complexity.
	getCombinations _		= []

-- | Smart constructor.
mkZobrist :: (
	Data.Array.IArray.Ix	x,
	Data.Bits.FiniteBits	positionHash,
	Enum			x,
	Enum			y,
	Ord			y,
	System.Random.RandomGen randomGen,
	System.Random.Random	positionHash
 )
	=> Maybe Int	-- ^ The optional minimum acceptable Hamming-distance between any two of the selected random numbers.
	-> randomGen
	-> Zobrist x y positionHash
mkZobrist maybeMinimumHammingDistance randomGen
	| Just minimumHammingDistance <- maybeMinimumHammingDistance
	, let minimumHammingDistance'	= minimum $ measureHammingDistances zobrist
	, minimumHammingDistance' < minimumHammingDistance	= Control.Exception.throw . Data.Exception.mkRequestFailure . showString "BishBosh.Component.Zobrist.mkZobrist:\tthe minimum Hamming-distance between the selected random numbers doesn't reach the configured minimum " . shows (minimumHammingDistance', minimumHammingDistance) . showString " => use more than " $ shows (Data.Bits.finiteBitSize randomForBlacksMove) " bits, or re-seed the generator & hope."
	| otherwise						= zobrist
	where
		((randomForBlacksMove, randomByCoordinatesByRankByLogicalColour), (randomByCastleableRooksXByLogicalColour, randomByEnPassantAbscissa))	= (
			(
				head . System.Random.randoms *** Data.Array.IArray.listArray (minBound, maxBound) . System.Random.randoms
			) . System.Random.split
		 ) *** (
			(
				Attribute.LogicalColour.listArrayByLogicalColour . map (
					zip [Cartesian.Abscissa.xMin, Cartesian.Abscissa.xMax] . System.Random.randoms
				) . ToolShed.System.Random.randomGens *** Cartesian.Abscissa.listArrayByAbscissa . System.Random.randoms
			) . System.Random.split
		 ) $ System.Random.split randomGen

		zobrist	= MkZobrist {
			getRandomForBlacksMove				= randomForBlacksMove,
			getRandomByCoordinatesByRankByLogicalColour	= randomByCoordinatesByRankByLogicalColour,
			getRandomByCastleableRooksXByLogicalColour	= randomByCastleableRooksXByLogicalColour,
			getRandomByEnPassantAbscissa			= randomByEnPassantAbscissa
		}

-- | Dereferences 'getRandomByCoordinatesByRankByLogicalColour' using the specified index.
dereferenceRandomByCoordinatesByRankByLogicalColour :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 )
	=> Index x y
	-> Zobrist x y positionHash
	-> positionHash
dereferenceRandomByCoordinatesByRankByLogicalColour index MkZobrist { getRandomByCoordinatesByRankByLogicalColour = randomByCoordinatesByRankByLogicalColour }	= randomByCoordinatesByRankByLogicalColour ! index

-- | Dereferences 'getRandomByCastleableRooksXByLogicalColour' using the specified abscissa.
dereferenceRandomByCastleableRooksXByLogicalColour
	:: Eq x
	=> Attribute.LogicalColour.LogicalColour
	-> x
	-> Zobrist x y positionHash
	-> Maybe positionHash	-- ^ The existence of a result depends on whether there remain any Rooks which can castle.
dereferenceRandomByCastleableRooksXByLogicalColour logicalColour x MkZobrist { getRandomByCastleableRooksXByLogicalColour = randomByCastleableRooksXByLogicalColour }	= lookup x $ randomByCastleableRooksXByLogicalColour ! logicalColour

-- | Dereferences 'getRandomByEnPassantAbscissa' using the specified abscissa.
dereferenceRandomByEnPassantAbscissa
	:: Data.Array.IArray.Ix x
	=> x
	-> Zobrist x y positionHash
	-> positionHash
dereferenceRandomByEnPassantAbscissa x MkZobrist { getRandomByEnPassantAbscissa = randomByEnPassantAbscissa }	= randomByEnPassantAbscissa ! x

-- | An interface to which 1-D hashable data can conform.
class Hashable1D hashable x {-CAVEAT: MultiParamTypeClasses-} where
	listRandoms1D	:: hashable x -> Zobrist x y positionHash -> [positionHash]

-- | An interface to which 2-D hashable data can conform.
class Hashable2D hashable x y {-CAVEAT: MultiParamTypeClasses-} where
	listRandoms2D	:: hashable x y -> Zobrist x y positionHash -> [positionHash]

-- | The operator used when combining random numbers to compose a hash.
combiningOp :: Data.Bits.Bits positionHash => positionHash -> positionHash -> positionHash
combiningOp	= Data.Bits.xor

-- | Resolve a hashable into a hash.
hash2D :: (
	Data.Bits.Bits	positionHash,
	Hashable2D	hashable x y
 )
	=> hashable x y
	-> Zobrist x y positionHash
	-> positionHash
hash2D hashable	= Data.List.foldl1' combiningOp . listRandoms2D hashable

-- | Include a list of random numbers in the hash.
combine :: Data.Bits.Bits positionHash => positionHash -> [positionHash] -> positionHash
combine	= Data.List.foldl' combiningOp

