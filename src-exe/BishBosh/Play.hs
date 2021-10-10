{-# LANGUAGE CPP, FlexibleContexts #-}
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

	* Constructs the Zobrist data-type.

	* Analyses standard-opening moves in order to statically sort the move-tree.

	* Reads any persistent game-state from file, to initialise the play-state, which constructs the quantified move tree.

	* Constructs position-hashes for each node in the tree.

	* Calls the selected user-interface.
-}

module BishBosh.Play(
-- * Functions
	play
 ) where

import			Control.Arrow((&&&), (|||))
import qualified	BishBosh.Component.Zobrist					as Component.Zobrist
import qualified	BishBosh.ContextualNotation.PositionHashQualifiedMoveTree	as ContextualNotation.PositionHashQualifiedMoveTree
import qualified	BishBosh.ContextualNotation.QualifiedMoveForest			as ContextualNotation.QualifiedMoveForest
import qualified	BishBosh.Input.EvaluationOptions				as Input.EvaluationOptions
import qualified	BishBosh.Input.IOOptions					as Input.IOOptions
import qualified	BishBosh.Input.Options						as Input.Options
import qualified	BishBosh.Input.SearchOptions					as Input.SearchOptions
import qualified	BishBosh.Input.UIOptions					as Input.UIOptions
import qualified	BishBosh.Input.Verbosity					as Input.Verbosity
import qualified	BishBosh.Model.GameTree						as Model.GameTree
import qualified	BishBosh.Model.MoveFrequency					as Model.MoveFrequency
import qualified	BishBosh.Notation.MoveNotation					as Notation.MoveNotation
import qualified	BishBosh.Property.Arboreal					as Property.Arboreal
import qualified	BishBosh.Property.Empty						as Property.Empty
import qualified	BishBosh.Search.SearchState					as Search.SearchState
import qualified	BishBosh.State.PlayState					as State.PlayState
import qualified	BishBosh.Text.ShowColouredPrefix				as Text.ShowColouredPrefix
import qualified	BishBosh.Type.Crypto						as Type.Crypto
import qualified	BishBosh.Type.Length						as Type.Length
import qualified	BishBosh.Type.Mass						as Type.Mass
import qualified	BishBosh.UI.CECP						as UI.CECP
import qualified	BishBosh.UI.Raw							as UI.Raw
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Control.Monad
import qualified	Data.Array.IArray
import qualified	Data.Bits
import qualified	Data.Default
import qualified	Data.Maybe
import qualified	System.FilePath
import qualified	System.IO
import qualified	System.Random
import			System.FilePath((</>))

#ifdef USE_UNBOXED_ARRAYS
import qualified	Data.Array.Unboxed
#endif

-- | Plays the game according to the specified configuration.
play :: (
	Control.DeepSeq.NFData		pieceSquareValue,
	Control.DeepSeq.NFData		x,
	Control.DeepSeq.NFData		y,
	Data.Array.IArray.Ix		x,
#ifdef USE_UNBOXED_ARRAYS
	Data.Array.Unboxed.IArray	Data.Array.Unboxed.UArray pieceSquareValue,	-- Requires 'FlexibleContexts'. The unboxed representation of the array-element must be defined (& therefore must be of fixed size).
#endif
	Data.Bits.FiniteBits		positionHash,
	Fractional			pieceSquareValue,
	Integral			x,
	Integral			y,
	Ord				positionHash,
	Read				x,
	Read				y,
	Real				pieceSquareValue,
	Show				pieceSquareValue,
	Show				x,
	Show				y,
	System.Random.Random		positionHash,
	System.Random.RandomGen		randomGen
 )
	=> Input.Verbosity.Verbosity
	-> randomGen
	-> Input.Options.Options pieceSquareValue x y
	-> ContextualNotation.QualifiedMoveForest.QualifiedMoveForest x y	-- ^ Standard openings.
	-> IO (State.PlayState.PlayState pieceSquareValue positionHash x y)
{-# SPECIALISE play
	:: System.Random.RandomGen randomGen
	=> Input.Verbosity.Verbosity
	-> randomGen
	-> Input.Options.Options Type.Mass.PieceSquareValue Type.Length.X Type.Length.Y
	-> ContextualNotation.QualifiedMoveForest.QualifiedMoveForest Type.Length.X Type.Length.Y
	-> IO (State.PlayState.PlayState Type.Mass.PieceSquareValue Type.Crypto.PositionHash Type.Length.X Type.Length.Y)
 #-}
play verbosity randomGen options qualifiedMoveForest	= Data.Maybe.maybe (
	return {-to IO-monad-} Data.Default.def {-game-}
 ) (
	\(filePath, _) -> if any (System.FilePath.equalFilePath filePath) [
		showChar System.FilePath.pathSeparator "dev" </> "null", -- On *nix.
		"nul" -- On Windows ?
	]
		then return {-to IO-monad-} Data.Default.def {-game-}
		else Control.Exception.catch (
			do
				s	<- readFile filePath

				return {-to IO-monad-} $! read s	-- Force evaluation, to trigger any exception thrown from within 'catch'.
		) $ \e -> do
			System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixWarning . showString "'readFile' failed; " $ shows (e :: Control.Exception.SomeException) "."

			return {-to IO-monad-} Data.Default.def {-game-}
 ) (
	Input.IOOptions.getMaybePersistence ioOptions
 ) >>= (
	let
		sortOnStandardOpeningMoveFrequency	= Input.SearchOptions.getSortOnStandardOpeningMoveFrequency searchOptions

		moveFrequency
			| sortOnStandardOpeningMoveFrequency	= Model.GameTree.toMoveFrequency $ ContextualNotation.QualifiedMoveForest.toGameTree qualifiedMoveForest
			| otherwise				= Property.Empty.empty
	in (
		\playState -> Data.Maybe.maybe (
			do
				Control.Monad.when (sortOnStandardOpeningMoveFrequency && verbosity == maxBound) . System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixInfo . shows (Model.MoveFrequency.countEntries moveFrequency) . showString " plies are recorded in the tree, of which " $ shows (Model.MoveFrequency.countDistinctEntries moveFrequency) " are distinct."

				return {-to IO-monad-} playState
		) (
			\depth -> do
				System.IO.hPutStrLn System.IO.stderr . Text.ShowColouredPrefix.showsPrefixInfo . showString "Move-tree:\n" $ (
					uncurry Notation.MoveNotation.showsNotationFloatToNDecimals (
						Input.UIOptions.getMoveNotation &&& Input.UIOptions.getNDecimalDigits $ uiOptions
					) . Property.Arboreal.prune depth . Search.SearchState.getPositionHashQuantifiedGameTree $ State.PlayState.getSearchState playState
				 ) ""

				return {-to IO-monad-} playState
		) $ Input.UIOptions.getMaybePrintMoveTree uiOptions
	) . State.PlayState.initialise options zobrist moveFrequency
 ) >>= (
	const UI.Raw.takeTurns ||| const UI.CECP.takeTurns $ Input.UIOptions.getEitherNativeUIOrCECPOptions uiOptions	-- Select a user-interface.
 ) (
	ContextualNotation.PositionHashQualifiedMoveTree.fromQualifiedMoveForest (
		Input.EvaluationOptions.getIncrementalEvaluation $ Input.Options.getEvaluationOptions options
	) zobrist qualifiedMoveForest
 ) randomGen where
	(ioOptions, searchOptions)	= Input.Options.getIOOptions &&& Input.Options.getSearchOptions $ options
	uiOptions			= Input.IOOptions.getUIOptions ioOptions

	zobrist	= Component.Zobrist.mkZobrist (Input.SearchOptions.getMaybeMinimumHammingDistance searchOptions) randomGen

