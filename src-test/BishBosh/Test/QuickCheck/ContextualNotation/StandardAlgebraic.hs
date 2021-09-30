{-# LANGUAGE CPP #-}
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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties.
-}

module BishBosh.Test.QuickCheck.ContextualNotation.StandardAlgebraic(
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.Attribute.Rank()
import qualified	BishBosh.Attribute.Rank				as Attribute.Rank
import qualified	BishBosh.Component.Turn				as Component.Turn
import qualified	BishBosh.ContextualNotation.StandardAlgebraic	as ContextualNotation.StandardAlgebraic
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.Test.QuickCheck.Model.Game		as Test.QuickCheck.Model.Game
import qualified	Test.QuickCheck

#ifdef USE_POLYPARSE
import			Control.Arrow((***))
#	if USE_POLYPARSE == 1
import qualified	Text.ParserCombinators.Poly.Lazy		as Poly
#	else /* Plain */
import			Control.Arrow((|||))
import qualified	Text.ParserCombinators.Poly.Plain		as Poly
#	endif
#else /* Parsec */
import			Control.Arrow((|||))
import qualified	Text.ParserCombinators.Parsec
#endif

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: Attribute.Rank.Rank -> Test.QuickCheck.Property
		f rank	= Test.QuickCheck.label "StandardAlgebraic.prop_san" $ ContextualNotation.StandardAlgebraic.toRank (ContextualNotation.StandardAlgebraic.fromRank rank) == rank
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 32 } f,
	let
		f :: Test.QuickCheck.Model.Game.Game -> ContextualNotation.StandardAlgebraic.ExplicitEnPassant -> ContextualNotation.StandardAlgebraic.ValidateMoves -> Test.QuickCheck.Property
		f game explicitEnPassant validateMoves	= Test.QuickCheck.label "StandardAlgebraic.prop_sanParser" $ all (
			\(game', turn) -> let
				qualifiedMove	= Component.Turn.getQualifiedMove turn
			in
#ifdef USE_POLYPARSE
			uncurry (&&) . (
#	if USE_POLYPARSE == 1
				(== qualifiedMove) . ContextualNotation.StandardAlgebraic.getQualifiedMove *** null {-unparsed input-}
#	else /* Plain */
				(
					const False ||| (== qualifiedMove) . ContextualNotation.StandardAlgebraic.getQualifiedMove
				) *** null {-unparsed input-}
#	endif
			) . Poly.runParser (
				ContextualNotation.StandardAlgebraic.parser explicitEnPassant validateMoves game'
			)
#else /* Parsec */
			(
				const False ||| (== qualifiedMove) . ContextualNotation.StandardAlgebraic.getQualifiedMove
			) . Text.ParserCombinators.Parsec.parse (
				ContextualNotation.StandardAlgebraic.parser explicitEnPassant validateMoves game'
			) "SAN-parser"
#endif
			$ ContextualNotation.StandardAlgebraic.showTurn explicitEnPassant turn game'
		 ) $ Model.Game.rollBack game
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 512 } f
 ]

