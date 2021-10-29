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

 [@DESCRIPTION@]	Defines the data-type which represents the rank of a chess-/piece/.

 [@CAVEAT@]	This term is also commonly used to refer to a row of the board.
-}

module BishBosh.Attribute.Rank(
-- * Type-classes
	Promotable(..),
-- * Types
-- ** Type-synonyms
	EvaluateRank,
	ArrayByRank,
	UArrayByRank,
-- ** Data-types
	Rank(..),
-- * Constants
	tag,
	flank,
	promotionProspects,
	defaultPromotionRank,
	plodders,
	fixedAttackRange,
	individuallySufficientMaterial,
--	royalty
	pieces,
	nobility,
	range,
	earthBound,
	expendable,
	nDistinctRanks,
	initialAllocationByRankPerSide,
-- * Functions
	compareByLVA,
	findUndefinedRanks,
-- ** Constructor
	listArrayByRank,
	arrayByRank
) where

import qualified	BishBosh.Metric.RankValue		as Metric.RankValue
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Type.Count			as Type.Count
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.Array.Unboxed
import qualified	Data.Char
import qualified	Data.List
import qualified	Data.Ord
import qualified	Data.Set
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT
import qualified	Text.XML.HXT.Arrow.Pickle.Schema

-- | Used to qualify XML.
tag :: String
tag	= "rank"

-- | A sum-type which represents the component of a chess-/piece/ other than its colour.
data Rank
	= Pawn
	| Rook
	| Knight
	| Bishop
	| Queen
	| King
	deriving (
		Bounded,
		Enum,
		Eq,
		Ord
	)

instance Control.DeepSeq.NFData Rank where
	rnf _	= ()

instance Data.Array.IArray.Ix Rank where
{-
	range				= uncurry enumFromTo
	inRange (lower, upper) rank	= rank >= lower && rank <= upper
	index (lower, _) rank		= fromEnum rank - fromEnum lower
-}
	range (lower, upper)		= Control.Exception.assert (lower == minBound && upper == maxBound) range
	inRange (lower, upper) rank	= Control.Exception.assert (rank >= lower && rank <= upper) True
	index (lower, upper)		= Control.Exception.assert (lower == minBound && upper == maxBound) . fromEnum

instance Show Rank where
	show Pawn	= "p"
	show Rook	= "r"
	show Knight	= "n"
	show Bishop	= "b"
	show Queen	= "q"
	show King	= "k"

instance Read Rank where
	readsPrec precedence (c : s)
		| Data.Char.isSpace c	= readsPrec precedence s	-- Consume.
		| otherwise		= map (flip (,) s) $ case Data.Char.toLower c of
			'p'	-> [Pawn]
			'r'	-> [Rook]
			'n'	-> [Knight]
			'b'	-> [Bishop]
			'q'	-> [Queen]
			'k'	-> [King]
			_	-> []	-- No parse.
	readsPrec _ _	= []	-- No parse.

instance HXT.XmlPickler Rank where
	xpickle	= HXT.xpAttr tag . HXT.xpWrap (read, show) . HXT.xpTextDT . Text.XML.HXT.Arrow.Pickle.Schema.scEnum $ map show range

-- | The constant ascending list of all /rank/s.
range :: [Rank]
range	= [minBound .. maxBound]

instance Property.FixedMembership.FixedMembership Rank where
	members	= range

-- | The distinct /rank/s of the constant ordered range of those /piece/s of which each side has exactly two.
flank :: [Rank]
flank	= [Rook, Knight, Bishop]

-- | The constant list of distinct /rank/ to which a @Pawn@ may legally be promoted; though there's no point in promotion to other than @Queen@ or @Knight@.
promotionProspects :: [Rank]
promotionProspects	= Queen : flank

-- | The /rank/ to which a @Pawn@ is, in the absence of instruction, promoted.
defaultPromotionRank :: Rank
defaultPromotionRank	= Queen

-- | The subset of /rank/s which can only move in single steps.
plodders :: [Rank]
plodders	= [Pawn, King]

-- | The subset of /rank/s which attack over a fixed range.
fixedAttackRange :: [Rank]
fixedAttackRange	= Knight : plodders

-- | The subset of /rank/s which lacking support, are sufficient to force checkmate.
individuallySufficientMaterial :: [Rank]
individuallySufficientMaterial	= [Pawn, Rook, Queen]

-- | The /rank/s of the back row of pieces, excluding both flanks.
royalty :: [Rank]
royalty	= [Queen, King]

-- | The distinct /rank/s of the pieces from which the back row is composed, i.e. everything except @Pawn@s.
pieces :: [Rank]
pieces	= flank ++ royalty

-- | The ordered /rank/s of the pieces from which the back row is composed, including duplicates.
nobility :: [Rank]
nobility	= pieces ++ reverse flank

-- | Those /rank/s which can't jump.
earthBound :: [Rank]
earthBound	= Data.List.delete Knight range

-- | Those /rank/s which can be taken.
expendable :: [Rank]
expendable	= Data.List.delete King range

-- | The type of a function which returns a /rank/'s value.
type EvaluateRank	= Rank -> Metric.RankValue.RankValue

{- |
	* Given two alternative capture moves, this function compares the rank-value of the aggressors.

	* N.B.: a @King@ is always considered most valuable, regardless of the evaluation-function supplied.
-}
compareByLVA
	:: EvaluateRank
	-> Rank
	-> Rank
	-> Ordering
compareByLVA evaluateRank rankL rankR
	| rankL == rankR	= EQ
	| rankL == King		= GT
	| rankR == King		= LT
	| otherwise		= Data.Ord.comparing evaluateRank rankL rankR

-- | The constant number of distinct /rank/s.
nDistinctRanks :: Type.Count.NRanks
nDistinctRanks	= fromIntegral $ length range

-- | The constant number of each rank per side, at the conventional opening position.
initialAllocationByRankPerSide :: ArrayByRank Type.Count.NPieces
initialAllocationByRankPerSide	= listArrayByRank [8, 2, 2, 2, 1, 1]

-- | A boxed array indexed by /rank/, of arbitrary values.
type ArrayByRank	= Data.Array.IArray.Array Rank

-- | An unboxed array indexed by /rank/, of fixed-size values.
type UArrayByRank	= Data.Array.Unboxed.UArray Rank

-- | Array-constructor from an ordered list of elements.
listArrayByRank :: Data.Array.IArray.IArray a e => [e] -> a Rank e
listArrayByRank	= Data.Array.IArray.listArray (minBound, maxBound)

-- | Array-constructor from an association-list.
arrayByRank :: Data.Array.IArray.IArray a e => [(Rank, e)] -> a Rank e
arrayByRank	= Data.Array.IArray.array (minBound, maxBound)

-- | An interface which data which can represent @Pawn@-promotion, can implement.
class Promotable a where
	getMaybePromotionRank	:: a -> Maybe Rank

-- | Finds any unspecified ranks.
findUndefinedRanks :: [Rank] -> [Rank]
findUndefinedRanks	= Data.Set.toList . (Data.Set.fromAscList range `Data.Set.difference`) . Data.Set.fromList

