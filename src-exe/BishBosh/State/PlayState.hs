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
		getMaybeApplicationTerminationReason,
		getNPliesSinceStandardOpeningMatch
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
	rollBackPositionHashQuantifiedGameTree,
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
import qualified	BishBosh.Type.Count					as Type.Count
import qualified	BishBosh.Type.Crypto					as Type.Crypto
import qualified	BishBosh.Type.Mass					as Type.Mass
import qualified	Control.Exception
import qualified	Data.Bits
import qualified	Data.Default
import qualified	Data.List
import qualified	Data.Maybe
import qualified	Data.Ord
import qualified	Factory.Math.Statistics

-- | The type threaded through the sequence of /game/s during play.
data PlayState positionHash	= MkPlayState {
	getCriterionValues			:: [[Metric.CriterionValue.CriterionValue]],					-- ^ The /criterion-value/s accumulated during the game.
	getZobrist				:: Component.Zobrist.Zobrist positionHash,					-- ^ The constant hash-codes used to construct position-hashes.
	getMoveFrequency			:: Model.GameTree.MoveFrequency,						-- ^ The constant frequency of moves extracted from file.
	getSearchState				:: Search.SearchState.SearchState positionHash,
	getOptions				:: Input.Options.Options,							-- ^ The constant options by which the game is configured.
	getMaybeApplicationTerminationReason	:: Maybe State.ApplicationTerminationReason.ApplicationTerminationReason,	-- ^ Whether the game has terminated.
	getNPliesSinceStandardOpeningMatch	:: Type.Count.NPlies								-- ^ The number of plies since matching the position with a standard opening.
}

-- | Constructor.
initialise
	:: Data.Bits.Bits positionHash
	=> Input.Options.Options
	-> Component.Zobrist.Zobrist positionHash
	-> Model.GameTree.MoveFrequency
	-> Model.Game.Game
	-> PlayState positionHash
{-# SPECIALISE initialise
	:: Input.Options.Options
	-> Component.Zobrist.Zobrist Type.Crypto.PositionHash
	-> Model.GameTree.MoveFrequency
	-> Model.Game.Game
	-> PlayState Type.Crypto.PositionHash
 #-}
initialise options zobrist moveFrequency game	= MkPlayState {
	getCriterionValues			= [],
	getZobrist				= zobrist,
	getMoveFrequency			= moveFrequency,
	getSearchState				= Search.SearchState.initialise $ uncurry Evaluation.PositionHashQuantifiedGameTree.mkPositionHashQuantifiedGameTree (
		Input.Options.getEvaluationOptions &&& Input.Options.getSearchOptions $ options
	) zobrist moveFrequency game,
	getOptions				= options,
	getMaybeApplicationTerminationReason	= Nothing,
	getNPliesSinceStandardOpeningMatch	= 0
}

-- | Accessor.
getGame :: PlayState positionHash -> Model.Game.Game
getGame MkPlayState { getSearchState = searchState }	= Evaluation.QuantifiedGame.getGame . Evaluation.PositionHashQuantifiedGameTree.getRootQuantifiedGame $ Search.SearchState.getPositionHashQuantifiedGameTree searchState

-- | The type of a function used to transform a 'PlayState'.
type Transformation positionHash	= PlayState positionHash -> PlayState positionHash

-- | Mutator.
setPositionHashQuantifiedGameTree :: Evaluation.PositionHashQuantifiedGameTree.PositionHashQuantifiedGameTree positionHash -> Transformation positionHash
setPositionHashQuantifiedGameTree positionHashQuantifiedGameTree playState@MkPlayState { getSearchState = searchState }	= playState {
	getSearchState	= searchState { Search.SearchState.getPositionHashQuantifiedGameTree = positionHashQuantifiedGameTree }
}

-- | Reconstruct the /positionHashQuantifiedGameTree/ (in the /searchState/), with the apex set to the specified game.
reconstructPositionHashQuantifiedGameTree
	:: Data.Bits.Bits positionHash
	=> Model.Game.Game
	-> Transformation positionHash
{-# SPECIALISE reconstructPositionHashQuantifiedGameTree :: Model.Game.Game -> Transformation Type.Crypto.PositionHash #-}
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
resetPositionHashQuantifiedGameTree :: Data.Bits.Bits positionHash => Transformation positionHash
{-# SPECIALISE resetPositionHashQuantifiedGameTree :: Transformation Type.Crypto.PositionHash #-}
resetPositionHashQuantifiedGameTree playState	= reconstructPositionHashQuantifiedGameTree Data.Default.def playState {
	getCriterionValues			= [],
	getMaybeApplicationTerminationReason	= Nothing,
	getNPliesSinceStandardOpeningMatch	= 0
}

-- | Roll-back the state by the specified number of plies.
rollBackPositionHashQuantifiedGameTree
	:: Data.Bits.Bits positionHash
	=> Model.Game.Game
	-> Type.Count.NPlies
	-> Transformation positionHash
{-# SPECIALISE rollBackPositionHashQuantifiedGameTree :: Model.Game.Game -> Type.Count.NPlies -> Transformation Type.Crypto.PositionHash #-}
rollBackPositionHashQuantifiedGameTree game nPlies playState@MkPlayState {
	getNPliesSinceStandardOpeningMatch	= nPliesSinceStandardOpeningMatch
} = reconstructPositionHashQuantifiedGameTree game playState {
	getNPliesSinceStandardOpeningMatch	= max 0 $ nPliesSinceStandardOpeningMatch - nPlies
}

-- | Mutator.
updateWithAutomaticMove
	:: [Metric.CriterionValue.CriterionValue]
	-> Search.SearchState.SearchState positionHash
	-> Transformation positionHash
updateWithAutomaticMove criterionValues' searchState playState@MkPlayState {
	getCriterionValues			= criterionValues,
	getNPliesSinceStandardOpeningMatch	= nPliesSinceStandardOpeningMatch
} = playState {
	getCriterionValues			= criterionValues' : criterionValues,
	getSearchState				= searchState,
	getNPliesSinceStandardOpeningMatch	= succ nPliesSinceStandardOpeningMatch
}

-- | Mutator.
updateWithManualMove
	:: Model.Game.Game
	-> Bool	-- ^ Whether the move matches a standard opening.
	-> Transformation positionHash
updateWithManualMove game standardOpeningMatch playState@MkPlayState { getSearchState = searchState }	= setPositionHashQuantifiedGameTree (
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
 ) $ playState {
	getNPliesSinceStandardOpeningMatch	= if standardOpeningMatch
		then 0
		else succ $ getNPliesSinceStandardOpeningMatch playState
 } 

-- | Calculate the /root-mean-square/ & the /standard-deviation/, of the values of each type of /criterion/.
calculateCriterionValueStatistics :: (
	Floating	standardDeviation,
	Fractional	mean
 )
	=> PlayState positionHash
	-> [(mean, standardDeviation)]
calculateCriterionValueStatistics MkPlayState { getCriterionValues = criterionValues }	= map (
	(
		Factory.Math.Statistics.getMean &&& Factory.Math.Statistics.getStandardDeviation
	) . map (
		\criterionValue -> realToFrac criterionValue	:: Type.Mass.CriterionValue
	)
 ) $ Data.List.transpose criterionValues

-- | Resignation by the player who currently holds the choice of move.
resign :: Transformation positionHash
resign playState@MkPlayState { getSearchState = searchState }	= setPositionHashQuantifiedGameTree (
	Evaluation.PositionHashQuantifiedGameTree.resign $ Search.SearchState.getPositionHashQuantifiedGameTree searchState
 ) playState

-- | Given a string from which either a /move/ can't be parsed or the one that can is illegal, returns the closest matches amongst the currently available /move/s.
suggestCorrections
	:: String	-- ^ Move-string.
	-> PlayState positionHash
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
	:: PlayState positionHash
	-> PlayState positionHash
	-> Bool
hasMorePlies playState playState'	= Data.Ord.comparing (State.TurnsByLogicalColour.getNPlies . Model.Game.getTurnsByLogicalColour . getGame) playState playState' == GT

-- | Whether the user has requested application-termination, or the configured maximum number of turns has been reached.
hasApplicationTerminationBeenRequested :: PlayState positionHash -> Bool
hasApplicationTerminationBeenRequested MkPlayState { getMaybeApplicationTerminationReason = maybeApplicationTerminationReason }	= Data.Maybe.isJust maybeApplicationTerminationReason

