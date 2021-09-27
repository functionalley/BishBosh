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

 [@DESCRIPTION@]	The data-type threaded through the moves of the game, transporting the changing /game/-state & configuration.
-}

module BishBosh.State.PlayState(
-- * Types
-- ** Type-synonyms
--	Transformation,
-- ** Data-types
	PlayState(
--		MkPlayState,
--		getCriterionValues,
--		getZobrist,
--		getMoveFrequency,
		getSearchState,
		getOptions,
		getMaybeApplicationTerminationReason
	),
-- * Functions
	calculateCriterionValueStatistics,
	suggestCorrections,
-- ** Constructor
	initialise,
-- ** Accessors
	getGame,
-- ** Mutators
	reconstructPositionHashQuantifiedGameTree,
	resetPositionHashQuantifiedGameTree,
--	setPositionHashQuantifiedGameTree,
	updateWithAutomaticMove,
	updateWithManualMove,
	resign,
-- ** Predicates
	hasMorePlies,
	hasApplicationTerminationBeenRequested
 ) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Component.Zobrist				as Component.Zobrist
import qualified	BishBosh.Data.Exception					as Data.Exception
import qualified	BishBosh.Data.List
import qualified	BishBosh.Evaluation.PositionHashQuantifiedGameTree	as Evaluation.PositionHashQuantifiedGameTree
import qualified	BishBosh.Evaluation.QuantifiedGame			as Evaluation.QuantifiedGame
import qualified	BishBosh.Input.IOOptions				as Input.IOOptions
import qualified	BishBosh.Input.Options					as Input.Options
import qualified	BishBosh.Input.UIOptions				as Input.UIOptions
import qualified	BishBosh.Metric.CriterionValue				as Metric.CriterionValue
import qualified	BishBosh.Model.Game					as Model.Game
import qualified	BishBosh.Model.GameTree					as Model.GameTree
import qualified	BishBosh.Notation.MoveNotation				as Notation.MoveNotation
import qualified	BishBosh.Search.SearchState				as Search.SearchState
import qualified	BishBosh.State.ApplicationTerminationReason		as State.ApplicationTerminationReason
import qualified	BishBosh.State.TurnsByLogicalColour			as State.TurnsByLogicalColour
import qualified	BishBosh.Type.Crypto					as Type.Crypto
import qualified	BishBosh.Type.Length					as Type.Length
import qualified	BishBosh.Type.Mass					as Type.Mass
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.Bits
import qualified	Data.Default
import qualified	Data.List
import qualified	Data.Maybe
import qualified	Data.Ord
import qualified	Factory.Math.Statistics

#ifdef USE_UNBOXED_ARRAYS
import qualified	Data.Array.Unboxed
#endif

-- | The type threaded through the sequence of /game/s during play.
data PlayState column pieceSquareValue positionHash row x y	= MkPlayState {
	getCriterionValues			:: [[Metric.CriterionValue.CriterionValue]],					-- ^ The /criterion-value/s accumulated during the game.
	getZobrist				:: Component.Zobrist.Zobrist x y positionHash,					-- ^ The constant hash-codes used construct position-hashes.
	getMoveFrequency			:: Model.GameTree.MoveFrequency x y,						-- ^ The constant frequency of moves extracted from file.
	getSearchState				:: Search.SearchState.SearchState x y positionHash,
	getOptions				:: Input.Options.Options column pieceSquareValue row x y,			-- ^ The constant options by which the game is configured.
	getMaybeApplicationTerminationReason	:: Maybe State.ApplicationTerminationReason.ApplicationTerminationReason	-- ^ Whether the game has terminated.
}

-- | Constructor.
initialise :: (
	Data.Array.IArray.Ix					x,
#ifdef USE_UNBOXED_ARRAYS
	Data.Array.Unboxed.IArray Data.Array.Unboxed.UArray	pieceSquareValue,	-- Requires 'FlexibleContexts'. The unboxed representation of the array-element must be defined (& therefore must be of fixed size).
#endif
	Data.Bits.Bits						positionHash,
	Fractional						pieceSquareValue,
	Integral						x,
	Integral						y,
	Real							pieceSquareValue,
	Show							x,
	Show							y
 )
	=> Input.Options.Options column pieceSquareValue row x y
	-> Component.Zobrist.Zobrist x y positionHash
	-> Model.GameTree.MoveFrequency x y
	-> Model.Game.Game x y
	-> PlayState column pieceSquareValue positionHash row x y
{-# SPECIALISE initialise
	:: Input.Options.Options column Type.Mass.PieceSquareValue row Type.Length.X Type.Length.Y
	-> Component.Zobrist.Zobrist Type.Length.X Type.Length.Y Type.Crypto.PositionHash
	-> Model.GameTree.MoveFrequency Type.Length.X Type.Length.Y
	-> Model.Game.Game Type.Length.X Type.Length.Y
	-> PlayState column Type.Mass.PieceSquareValue Type.Crypto.PositionHash row Type.Length.X Type.Length.Y
 #-}
initialise options zobrist moveFrequency game	= MkPlayState {
	getCriterionValues			= [],
	getZobrist				= zobrist,
	getMoveFrequency			= moveFrequency,
	getSearchState				= Search.SearchState.initialise $ uncurry Evaluation.PositionHashQuantifiedGameTree.mkPositionHashQuantifiedGameTree (
		Input.Options.getEvaluationOptions &&& Input.Options.getSearchOptions $ options
	) zobrist moveFrequency game,
	getOptions				= options,
	getMaybeApplicationTerminationReason	= Nothing
}

-- | Accessor.
getGame :: PlayState column pieceSquareValue positionHash row x y -> Model.Game.Game x y
getGame MkPlayState { getSearchState = searchState }	= Evaluation.QuantifiedGame.getGame . Evaluation.PositionHashQuantifiedGameTree.getRootQuantifiedGame $ Search.SearchState.getPositionHashQuantifiedGameTree searchState

-- | The type of a function used to transform a 'PlayState'.
type Transformation column pieceSquareValue positionHash row x y	= PlayState column pieceSquareValue positionHash row x y -> PlayState column pieceSquareValue positionHash row x y

-- | Mutator.
setPositionHashQuantifiedGameTree :: Evaluation.PositionHashQuantifiedGameTree.PositionHashQuantifiedGameTree x y positionHash -> Transformation column pieceSquareValue positionHash row x y
setPositionHashQuantifiedGameTree positionHashQuantifiedGameTree playState@MkPlayState { getSearchState = searchState }	= playState {
	getSearchState	= searchState { Search.SearchState.getPositionHashQuantifiedGameTree = positionHashQuantifiedGameTree }
}

-- | Reconstruct the /positionHashQuantifiedGameTree/ (in the /searchState/), with the apex set to the specified game.
reconstructPositionHashQuantifiedGameTree :: (
	Data.Array.IArray.Ix					x,
#ifdef USE_UNBOXED_ARRAYS
	Data.Array.Unboxed.IArray Data.Array.Unboxed.UArray	pieceSquareValue,	-- Requires 'FlexibleContexts'. The unboxed representation of the array-element must be defined (& therefore must be of fixed size).
#endif
	Data.Bits.Bits						positionHash,
	Fractional						pieceSquareValue,
	Integral						x,
	Integral						y,
	Real							pieceSquareValue,
	Show							x,
	Show							y
 ) => Model.Game.Game x y -> Transformation column pieceSquareValue positionHash row x y
reconstructPositionHashQuantifiedGameTree game playState@MkPlayState {
	getZobrist		= zobrist,
	getMoveFrequency	= moveFrequency,
	getOptions		= options
} = setPositionHashQuantifiedGameTree (
	uncurry Evaluation.PositionHashQuantifiedGameTree.mkPositionHashQuantifiedGameTree (
		Input.Options.getEvaluationOptions &&& Input.Options.getSearchOptions $ options
	) zobrist moveFrequency game
 ) playState

-- | Reset to the initial state.
resetPositionHashQuantifiedGameTree :: (
	Data.Array.IArray.Ix					x,
#ifdef USE_UNBOXED_ARRAYS
	Data.Array.Unboxed.IArray Data.Array.Unboxed.UArray	pieceSquareValue,	-- Requires 'FlexibleContexts'. The unboxed representation of the array-element must be defined (& therefore must be of fixed size).
#endif
	Data.Bits.Bits						positionHash,
	Fractional						pieceSquareValue,
	Integral						x,
	Integral						y,
	Real							pieceSquareValue,
	Show							x,
	Show							y
 ) => Transformation column pieceSquareValue positionHash row x y
resetPositionHashQuantifiedGameTree playState	= reconstructPositionHashQuantifiedGameTree Data.Default.def playState {
	getCriterionValues			= [],
	getMaybeApplicationTerminationReason	= Nothing
}

-- | Mutator.
updateWithAutomaticMove
	:: [Metric.CriterionValue.CriterionValue]
	-> Search.SearchState.SearchState x y positionHash
	-> Transformation column pieceSquareValue positionHash row x y
updateWithAutomaticMove criterionValues searchState playState	= playState {
	getCriterionValues	= criterionValues : getCriterionValues playState,
	getSearchState		= searchState
}

-- | Mutator.
updateWithManualMove :: (Eq x, Eq y) => Model.Game.Game x y -> Transformation column pieceSquareValue positionHash row x y
updateWithManualMove game playState@MkPlayState { getSearchState = searchState }	= setPositionHashQuantifiedGameTree (
	Data.Maybe.fromMaybe (
		Control.Exception.throw $ Data.Exception.mkIncompatibleData "BishBosh.State.PlayState.updateWithManualMove:\tEvaluation.PositionHashQuantifiedGameTree.reduce failed."
	) . Evaluation.PositionHashQuantifiedGameTree.reduce (
		(
			== Data.Maybe.fromMaybe (
				Control.Exception.throw $ Data.Exception.mkResultUndefined "BishBosh.State.PlayState.updateWithManualMove:\tModel.Game.maybeLastTurn failed."
			) (
				Model.Game.maybeLastTurn game
			)
		) . Evaluation.QuantifiedGame.getLastTurn . Evaluation.PositionHashQuantifiedGameTree.getQuantifiedGame
	) $ Search.SearchState.getPositionHashQuantifiedGameTree searchState
 ) playState

-- | Calculate the /root-mean-square/ & the /standard-deviation/, of the values of each type of /criterion/.
calculateCriterionValueStatistics :: (
	Floating	standardDeviation,
	Fractional	mean
 )
	=> PlayState column pieceSquareValue positionHash row x y
	-> [(mean, standardDeviation)]
calculateCriterionValueStatistics MkPlayState { getCriterionValues = criterionValues }	= map (
	(
		Factory.Math.Statistics.getMean &&& Factory.Math.Statistics.getStandardDeviation
	) . map (
		\criterionValue -> realToFrac criterionValue	:: Type.Mass.CriterionValue
	)
 ) $ Data.List.transpose criterionValues

-- | Resignation by the player who currently holds the choice of move.
resign :: Transformation column pieceSquareValue positionHash row x y
resign playState@MkPlayState { getSearchState = searchState }	= setPositionHashQuantifiedGameTree (
	Evaluation.PositionHashQuantifiedGameTree.resign $ Search.SearchState.getPositionHashQuantifiedGameTree searchState
 ) playState

-- | Given a string from which either a /move/ can't be parsed or the one that can is illegal, returns the closest matches amongst the currently available /move/s.
suggestCorrections :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Show	x,
	Show	y
 )
	=> String	-- ^ Move-string.
	-> PlayState column pieceSquareValue positionHash row x y
	-> [String]	-- ^ Suggested corrections.
suggestCorrections moveString playState@MkPlayState { getOptions = options }
	| Model.Game.isTerminated game	= []
	| otherwise			= BishBosh.Data.List.findClosest moveString . map (
		Notation.MoveNotation.showNotation . Input.UIOptions.getMoveNotation . Input.IOOptions.getUIOptions $ Input.Options.getIOOptions options
	) $ Model.Game.findQualifiedMovesAvailableToNextPlayer game
	where
		game	= getGame playState

-- | Whether the game in the first /play-state/ has more plies than that in the second.
hasMorePlies
	:: PlayState column pieceSquareValue positionHash row x y
	-> PlayState column pieceSquareValue positionHash row x y
	-> Bool
hasMorePlies playState playState'	= Data.Ord.comparing (State.TurnsByLogicalColour.getNPlies . Model.Game.getTurnsByLogicalColour . getGame) playState playState' == GT

-- | Whether the user has requested application-termination, or the configured maximum number of turns has been reached.
hasApplicationTerminationBeenRequested :: PlayState column pieceSquareValue positionHash row x y -> Bool
hasApplicationTerminationBeenRequested MkPlayState { getMaybeApplicationTerminationReason = maybeApplicationTerminationReason }	= Data.Maybe.isJust maybeApplicationTerminationReason
