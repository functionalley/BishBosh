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

	* The relative values of the various /rank/s of chess-piece.

	* <https://en.wikipedia.org/wiki/Chess_piece_relative_value#Hans_Berliner.27s_system%20Chess-piece%20relative%20values>
-}

module BishBosh.Input.RankValues(
-- * Types
-- ** Data-types
	RankValues(
--		MkRankValues,
--		deconstruct
	),
-- * Constants
	tag,
-- * Functions
	findRankValue,
	calculateMaximumTotalValue,
-- ** Constructor
	fromAssocs
) where

import			Control.Arrow((&&&), (***))
import			Data.Array.IArray((!))
import qualified	BishBosh.Attribute.Rank		as Attribute.Rank
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Data.Foldable		as Data.Foldable
import qualified	BishBosh.Metric.RankValue	as Metric.RankValue
import qualified	BishBosh.Property.ShowFloat	as Property.ShowFloat
import qualified	BishBosh.Text.ShowList		as Text.ShowList
import qualified	BishBosh.Type.Mass		as Type.Mass
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.Default
import qualified	Data.List
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT

-- | Used to qualify XML.
tag :: String
tag	= "rankValues"

{- |
	* The constant value associated with each /rank/; the higher, the more valuable it is considered to be.

	* N.B.: only relative values are significant; the absolute value associated with any /rank/ is irrelevant; typically ranks are valued in /centipawns/.

	* CAVEAT: a @King@ can never be taken, but assigning the value /infinity/ creates problems, so typically it has the value @0@.
-}
newtype RankValues	= MkRankValues {
	deconstruct	:: Attribute.Rank.ArrayByRank Metric.RankValue.RankValue
} deriving (Eq, Read, Show)

instance Property.ShowFloat.ShowFloat RankValues where
	showsFloat fromDouble	= Text.ShowList.showsAssociationList' . map (show *** Property.ShowFloat.showsFloat fromDouble) . Data.Array.IArray.assocs . deconstruct

instance Data.Default.Default RankValues where
	def = MkRankValues . Attribute.Rank.listArrayByRank $ map (
		fromRational . (/ 10)	-- Map into the closed unit-interval.
	 ) [
		1,
		5,
		3,
		3,
		9,
		0	-- N.B.: move-selection is independent of the King's value (since it can't be taken), so it can be defined arbitrarily.
	 ]

instance Control.DeepSeq.NFData RankValues where
	rnf (MkRankValues byRank)	= Control.DeepSeq.rnf byRank

instance HXT.XmlPickler RankValues where
	xpickle	= HXT.xpDefault Data.Default.def . HXT.xpWrap (
		fromAssocs,				-- Construct from an association-list.
		Data.Array.IArray.assocs . deconstruct	-- Deconstruct to an association-list.
	 ) . HXT.xpList1 . HXT.xpElem tag $ HXT.xpickle {-Rank-} `HXT.xpPair` HXT.xpickle {-RankValue-}

-- | Smart constructor.
fromAssocs :: [(Attribute.Rank.Rank, Metric.RankValue.RankValue)] -> RankValues
fromAssocs assocs
	| not $ null undefinedRanks	= Control.Exception.throw . Data.Exception.mkInsufficientData . showString "BishBosh.Input.RankValues.fromAssocs:\tranks" . Text.ShowList.showsAssociation $ shows undefinedRanks " are undefined."
	| not $ null duplicateRanks	= Control.Exception.throw . Data.Exception.mkDuplicateData . showString "BishBosh.Input.RankValues.fromAssocs:\tranks must be distinct; " $ shows duplicateRanks "."
	| all (
		(== 0) . snd {-RankValue-}
	) assocs			= Control.Exception.throw . Data.Exception.mkNullDatum . showString "BishBosh.Input.RankValues.fromAssocs:\tat least one rank should have a non-zero value; " $ shows assocs "."
	| byRank ! Attribute.Rank.Queen /= maximum [
		rankValue |
			(rank, rankValue)	<- assocs,
			rank /= Attribute.Rank.King	-- Whose rank-value is irrelevant.
	] {-list-comprehension-}	= Control.Exception.throw . Data.Exception.mkIncompatibleData . showString "BishBosh.Input.RankValues.fromAssocs:\texcepting possibly the King, the Queen should be the most valuable rank; " $ shows assocs "."
	| otherwise			= MkRankValues byRank
	where
		(undefinedRanks, duplicateRanks)	= Attribute.Rank.findUndefinedRanks &&& Data.Foldable.findDuplicates $ map fst assocs
		byRank					= Attribute.Rank.arrayByRank assocs

-- | Query.
findRankValue :: RankValues -> Attribute.Rank.Rank -> Metric.RankValue.RankValue
findRankValue (MkRankValues byRank)	= (byRank !)

{- |
	* The maximum total rank-value one side can have.

	* CAVEAT: assumes that zero pieces have been captured, all @Pawn@s have been queened, & that this is the most valuable /rank/ of /piece/.
-}
calculateMaximumTotalValue :: RankValues -> Type.Mass.RankValue
calculateMaximumTotalValue (MkRankValues byRank)	= 9 {-accounting for all possible promotions-} * realToFrac (
	byRank ! Attribute.Rank.Queen
 ) + 2 * Data.List.foldl' (
	\acc -> (+ acc) . realToFrac . (byRank !)
 ) 0 Attribute.Rank.flank

