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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary'.
-}

module BishBosh.Test.QuickCheck.Search.SearchState(
-- * Types
-- ** Type-synonyms
	SearchState
) where

import			BishBosh.Test.QuickCheck.Component.Zobrist()
import			BishBosh.Test.QuickCheck.Input.EvaluationOptions()
import			BishBosh.Test.QuickCheck.Input.SearchOptions()
import			BishBosh.Test.QuickCheck.Model.Game()
import qualified	BishBosh.Evaluation.PositionHashQuantifiedGameTree	as Evaluation.PositionHashQuantifiedGameTree
import qualified	BishBosh.Property.Empty					as Property.Empty
import qualified	BishBosh.Search.SearchState				as Search.SearchState
import qualified	BishBosh.Type.Crypto					as Type.Crypto
import qualified	Data.Bits
import qualified	System.Random
import qualified	Test.QuickCheck

-- | A suitable concrete type for testing.
type SearchState	= Search.SearchState.SearchState Type.Crypto.PositionHash

instance (
	Data.Bits.FiniteBits	positionHash,
	Num			positionHash,
	Show			positionHash,
	System.Random.Random	positionHash
 ) => Test.QuickCheck.Arbitrary (Search.SearchState.SearchState positionHash) where
	{-# SPECIALISE instance Test.QuickCheck.Arbitrary SearchState #-}
	arbitrary	= do
		Search.SearchState.initialise <$> (
			Evaluation.PositionHashQuantifiedGameTree.mkPositionHashQuantifiedGameTree <$> Test.QuickCheck.arbitrary {-EvaluationOptions-} <*> Test.QuickCheck.arbitrary {-SearchOptions-} <*> Test.QuickCheck.arbitrary {-Zobrist-} <*> return {-to Gen-monad-} Property.Empty.empty {-MoveFrequency-} <*> Test.QuickCheck.arbitrary {-Game-}
		 )

