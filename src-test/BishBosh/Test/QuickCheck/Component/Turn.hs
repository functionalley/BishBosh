{-# OPTIONS_GHC -fno-warn-orphans #-}
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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties.
-}

module BishBosh.Test.QuickCheck.Component.Turn(
-- * Constants
--	rankValues,
	results
) where

import			BishBosh.Test.QuickCheck.Attribute.MoveType()
import			BishBosh.Test.QuickCheck.Colour.LogicalColour()
import			BishBosh.Test.QuickCheck.Component.Move()
import			BishBosh.Test.QuickCheck.Component.Piece()
import			BishBosh.Test.QuickCheck.Input.RankValues()
import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.MoveType		as Attribute.MoveType
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Component.CastlingMove		as Component.CastlingMove
import qualified	BishBosh.Component.Piece		as Component.Piece
import qualified	BishBosh.Component.QualifiedMove	as Component.QualifiedMove
import qualified	BishBosh.Component.Turn			as Component.Turn
import qualified	BishBosh.Input.RankValues		as Input.RankValues
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Property.Reflectable		as Property.Reflectable
import qualified	Control.Arrow
import qualified	Data.List
import qualified	Data.Maybe
import qualified	Test.QuickCheck
import qualified	ToolShed.Data.List
import qualified	ToolShed.Test.ReversibleIO

instance Test.QuickCheck.Arbitrary Component.Turn.Turn where
	arbitrary	= do
		moveType	<- Test.QuickCheck.arbitrary
		(move, piece)	<- case moveType of
			Attribute.MoveType.Castle _	-> fmap (
				Component.CastlingMove.getKingsMove . Data.Maybe.fromJust . Data.List.find (
					(== moveType) . Component.CastlingMove.getMoveType
				) . Component.CastlingMove.getCastlingMoves &&& Component.Piece.mkKing
			 ) Test.QuickCheck.arbitrary {-logicalColour-}
			Attribute.MoveType.EnPassant	-> (,) <$> Test.QuickCheck.arbitrary {-piece. CAVEAT: very lax-} <*> fmap Component.Piece.mkPawn Test.QuickCheck.arbitrary {-logicalColour-}
			_				-> Test.QuickCheck.arbitrary	-- CAVEAT: the move-vector isn't tailored to the rank of piece.

		return {-to Gen-monad-} . Component.Turn.mkTurn (Component.QualifiedMove.mkQualifiedMove move moveType) $ Component.Piece.getRank piece

-- | Distinct rank-values designed for a predictable sort-order.
rankValues :: Input.RankValues.RankValues
rankValues	= Input.RankValues.fromAssocs . zip Property.FixedMembership.members $ map (fromRational . (/ 10)) [1, 5, 3, 4, 9, 0]

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Component.Turn.Turn -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Turn.prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: String -> Test.QuickCheck.Property
		f garbage	= Test.QuickCheck.label "Turn.prop_read" $ case (reads garbage :: [(Component.Turn.Turn, String)]) of
			[_]	-> True
			_	-> True	-- Unless the read-implementation throws an exception.
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Component.Turn.Turn -> String -> Test.QuickCheck.Property
		f turn	= Test.QuickCheck.label "Turn.prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (const False) turn
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: Component.Turn.Turn -> Test.QuickCheck.Property
		f	= Test.QuickCheck.label "Turn.prop_reflectOnX" . uncurry (==) . (id &&& Property.Reflectable.reflectOnX . Property.Reflectable.reflectOnX)
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 64 } f,
	let
		f :: Input.RankValues.RankValues -> [Component.Turn.Turn] -> Test.QuickCheck.Property
		f rankValues'	= Test.QuickCheck.label "Turn.prop_compareByMVVLVA/quiet" . uncurry (==) . (
			dropWhile Component.Turn.isCapture . Data.List.sortBy (
				Component.Turn.compareByMVVLVA (`Input.RankValues.findRankValue` rankValues')
			) &&& filter (
				not . Component.Turn.isCapture
			)
		 )
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: [Component.Turn.Turn] -> Test.QuickCheck.Property
		f = Test.QuickCheck.label "Turn.prop_compareByMVVLVA/MVV" . (
			\ranks -> Data.List.sortOn (
				negate . toRational . (`Input.RankValues.findRankValue` rankValues) -- Most valuable victim should be first.
			) ranks == ranks
		 ) . map head . Data.List.group . Data.Maybe.mapMaybe (
			Attribute.MoveType.getMaybeImplicitlyTakenRank . Component.QualifiedMove.getMoveType . Component.Turn.getQualifiedMove
		 ) . Data.List.sortBy (
			Component.Turn.compareByMVVLVA (`Input.RankValues.findRankValue` rankValues)
		 )
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: [Component.Turn.Turn] -> Test.QuickCheck.Property
		f = Test.QuickCheck.label "Turn.prop_compareByMVVLVA/LVA" . all (
			(
				\ranks -> uncurry (++) (
					Control.Arrow.first (
						Data.List.sortOn (`Input.RankValues.findRankValue` rankValues)	-- Least valuable aggressor should be first.
					) $ Data.List.partition (/= Attribute.Rank.King) ranks
				) == ranks
			) . map head . Data.List.group . map Component.Turn.getRank {-aggressor's rank-}
		 ) . Data.List.groupBy (
			ToolShed.Data.List.equalityBy $ Attribute.MoveType.getMaybeImplicitlyTakenRank . Component.QualifiedMove.getMoveType . Component.Turn.getQualifiedMove
		 ) . takeWhile Component.Turn.isCapture . Data.List.sortBy (
			Component.Turn.compareByMVVLVA (`Input.RankValues.findRankValue` rankValues)
		 )
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 512 } f
 ]

