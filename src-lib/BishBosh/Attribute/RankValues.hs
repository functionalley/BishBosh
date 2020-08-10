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

module BishBosh.Attribute.RankValues(
-- * Types
-- ** Data-types
	RankValues(),
-- * Constants
	tag,
-- * Functions
	findRankValue,
	calculateMaximumTotalValue,
-- ** Constructor
	fromAssocs
) where

import			Control.Arrow((***))
import			Data.Array.IArray((!))
import qualified	BishBosh.Attribute.Rank		as Attribute.Rank
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Data.Num		as Data.Num
import qualified	BishBosh.Property.ShowFloat	as Property.ShowFloat
import qualified	BishBosh.Text.ShowList		as Text.ShowList
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.Default
import qualified	Data.List
import qualified	Data.Set
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT

-- | Used to qualify XML.
tag :: String
tag	= "rankValues"

{- |
	* The constant value associated with each /rank/; the higher, the more valuable it is considered to be.

	* N.B.: only relative values are significant; the absolute value associated with any /rank/ is irrelevant, but typically @Pawn = 1@.

	* CAVEAT: a @King@ can never be taken, but assigning the value /infinity/ creates problems, so typically it has the value @0@.
-}
newtype RankValues rankValue	= MkRankValues {
	deconstruct	:: Attribute.Rank.ByRank rankValue
} deriving (Eq, Read, Show)

instance Real rankValue => Property.ShowFloat.ShowFloat (RankValues rankValue) where
	showsFloat fromDouble	= Text.ShowList.showsAssociationList' . map (show *** fromDouble . realToFrac) . Data.Array.IArray.assocs . deconstruct

-- Derived from Larry Kaufman's values; <https://www.chessprogramming.org/Point_Value>.
instance (
	Fractional	rankValue,
	Ord		rankValue,
	Show		rankValue
 ) => Data.Default.Default (RankValues rankValue) where
	def = fromAssocs [
		(
			Attribute.Rank.Pawn,	0.1
		), (
			Attribute.Rank.Rook,	0.525
		), (
			Attribute.Rank.Knight,	0.35
		), (
			Attribute.Rank.Bishop,	0.35
		), (
			Attribute.Rank.Queen,	1
		), (
			Attribute.Rank.King,	0	-- N.B.: move-selection is independent of this value (since it can't be taken), so it can be defined arbitrarily.
		)
	 ]

instance Control.DeepSeq.NFData rankValue => Control.DeepSeq.NFData (RankValues rankValue) where
	rnf (MkRankValues byRank)	= Control.DeepSeq.rnf byRank

instance (
	Fractional	rankValue,
	HXT.XmlPickler	rankValue,
	Ord		rankValue,
	Show		rankValue
 ) => HXT.XmlPickler (RankValues rankValue) where
	xpickle	= HXT.xpDefault Data.Default.def . HXT.xpWrap (
		fromAssocs,				-- Construct from an association-list.
		Data.Array.IArray.assocs . deconstruct	-- Deconstruct to an association-list.
	 ) . HXT.xpList1 . HXT.xpElem tag $ HXT.xpickle {-rank-} `HXT.xpPair` HXT.xpAttr "value" HXT.xpickle

-- | Smart-constructor.
fromAssocs :: (
	Fractional	rankValue,
	Ord		rankValue,
	Show		rankValue
 ) => [(Attribute.Rank.Rank, rankValue)] -> RankValues rankValue
fromAssocs assocs
	| not $ Data.Set.null undefinedRanks	= Control.Exception.throw . Data.Exception.mkInsufficientData . showString "BishBosh.Attribute.RankValues.fromAssocs:\tranks" . Text.ShowList.showsAssociation $ shows (Data.Set.toList undefinedRanks) " are undefined."
	| any (
		not . Data.Num.inClosedUnitInterval . snd {-rank-value-}
	) assocs				= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Attribute.RankValues.fromAssocs:\tall values must be within the closed unit-interval, [0,1]; " $ shows assocs "."
	| otherwise				= MkRankValues byRank
	where
		undefinedRanks	= Data.Set.fromAscList Attribute.Rank.range `Data.Set.difference` Data.Set.fromList (map fst assocs)
		byRank		= Data.Array.IArray.array (minBound, maxBound) assocs

-- | Query.
findRankValue :: Attribute.Rank.Rank -> RankValues rankValue -> rankValue
findRankValue rank (MkRankValues byRank)	= byRank ! rank

{- |
	* The maximum total rank-value one side can have.

	* CAVEAT: assumes that zero pieces have been captured, all @Pawn@s have been queened, & that this is the most valuable /rank/ of /piece/.
-}
calculateMaximumTotalValue :: Num rankValue => RankValues rankValue -> rankValue
calculateMaximumTotalValue (MkRankValues byRank)	= 9 {-accounting for all possible promotions-} * (byRank ! Attribute.Rank.Queen) + 2 * Data.List.foldl' (
	\acc -> (+ acc) . (byRank !)
 ) 0 Attribute.Rank.flank

