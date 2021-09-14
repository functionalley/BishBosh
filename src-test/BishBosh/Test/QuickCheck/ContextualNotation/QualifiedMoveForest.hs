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

module BishBosh.Test.QuickCheck.ContextualNotation.QualifiedMoveForest(
-- * Types
-- ** Type-synonyms
	QualifiedMoveForest,
-- * Constants
	results
) where

import			BishBosh.Test.QuickCheck.ContextualNotation.PGN()
import			Control.Arrow((&&&))
import qualified	BishBosh.ContextualNotation.QualifiedMoveForest	as ContextualNotation.QualifiedMoveForest
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.Model.GameTree				as Model.GameTree
import qualified	BishBosh.Property.Null				as Property.Null
import qualified	BishBosh.State.Board				as State.Board
import qualified	BishBosh.Types					as T
import qualified	Data.Foldable
import qualified	Data.Tree
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Show	x,
	Show	y
 ) => Test.QuickCheck.Arbitrary (ContextualNotation.QualifiedMoveForest.QualifiedMoveForest x y) where
	{-# SPECIALISE instance Test.QuickCheck.Arbitrary QualifiedMoveForest #-}
	arbitrary	= fmap ContextualNotation.QualifiedMoveForest.fromPGNDatabase Test.QuickCheck.arbitrary

-- | Defines a concrete type for testing.
type QualifiedMoveForest	= ContextualNotation.QualifiedMoveForest.QualifiedMoveForest T.X T.Y

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	let
		f :: QualifiedMoveForest -> Test.QuickCheck.Property
		f = Test.QuickCheck.label "QualifiedMoveForest.prop_findMinimumPieces" . uncurry (==) . (
			ContextualNotation.QualifiedMoveForest.findMinimumPieces &&& Data.Foldable.minimum . fmap (
				State.Board.getNPieces . Model.Game.getBoard
			) . Model.GameTree.deconstruct . ContextualNotation.QualifiedMoveForest.toGameTree
		 )
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f,
	let
		f :: QualifiedMoveForest -> Test.QuickCheck.Property
		f qualifiedMoveForest	= not (Property.Null.isNull qualifiedMoveForest) ==> Test.QuickCheck.label "QualifiedMoveForest.prop_count" . uncurry (==) $ (
			snd {-nPositions-} . ContextualNotation.QualifiedMoveForest.count &&& fromIntegral . length . tail {-remove the apex-} . Data.Tree.flatten . Model.GameTree.deconstruct . ContextualNotation.QualifiedMoveForest.toGameTree
		 ) qualifiedMoveForest
	in Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 } f
 ]

