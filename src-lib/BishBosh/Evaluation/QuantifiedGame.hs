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

	* Quantifies a /game/, & therefore the sequence of moves applied.

	* The fitness & its breakdown into constituent criterion-values, are also recorded.
-}

module BishBosh.Evaluation.QuantifiedGame(
-- * Types
-- ** Type-synonyms
	OpenInterval,
-- ** Data-types
	QuantifiedGame(
--		MkQuantifiedGame,
		getGame,
		getWeightedMeanAndCriterionValues
	),
-- * Constants
	unboundedInterval,
-- * Functions
	compareFitness,
-- ** Accessors
	getFitness,
-- ** Constructors
	fromGame,
-- ** Accessors
	getLastTurn,
	getLatestTurns,
-- ** Mutators
	negateFitness,
	negateInterval
 ) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Component.Move				as Component.Move
import qualified	BishBosh.Component.Turn				as Component.Turn
import qualified	BishBosh.Data.Exception				as Data.Exception
import qualified	BishBosh.Evaluation.Fitness			as Evaluation.Fitness
import qualified	BishBosh.Input.EvaluationOptions		as Input.EvaluationOptions
import qualified	BishBosh.Metric.WeightedMeanAndCriterionValues	as Metric.WeightedMeanAndCriterionValues
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.Notation.MoveNotation			as Notation.MoveNotation
import qualified	BishBosh.Property.Null				as Property.Null
import qualified	BishBosh.Text.ShowList				as Text.ShowList
import qualified	BishBosh.Type.Count				as Type.Count
import qualified	BishBosh.Type.Mass				as Type.Mass
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Maybe
import qualified	Data.Ord

#ifdef USE_UNBOXED_ARRAYS
import qualified	Data.Array.Unboxed
#endif

-- | The selected /game/ & the criteria against which it was quantified.
data QuantifiedGame	= MkQuantifiedGame {
	getGame					:: Model.Game.Game,	-- ^ The /game/ resulting from a sequence of /turn/s.
	getWeightedMeanAndCriterionValues	:: Metric.WeightedMeanAndCriterionValues.WeightedMeanAndCriterionValues
} deriving (Eq, Show)

instance Control.DeepSeq.NFData QuantifiedGame where
	rnf MkQuantifiedGame { getWeightedMeanAndCriterionValues = weightedMeanAndCriterionValues }	= Control.DeepSeq.rnf weightedMeanAndCriterionValues	-- The other field is a prerequisite.

instance Notation.MoveNotation.ShowNotationFloat QuantifiedGame where
	showsNotationFloat moveNotation showsDouble quantifiedGame	= Text.ShowList.showsAssociationList Text.ShowList.showsSeparator $ map ($ quantifiedGame) [
		(,) Component.Move.tag . Notation.MoveNotation.showsNotation moveNotation . getLastTurn,
		(,) Metric.WeightedMeanAndCriterionValues.weightedMeanTag . showsDouble . realToFrac . getFitness,
		(,) Metric.WeightedMeanAndCriterionValues.criterionValuesTag . Text.ShowList.showsFormattedList' (showsDouble . realToFrac) . Metric.WeightedMeanAndCriterionValues.getCriterionValues . getWeightedMeanAndCriterionValues
	 ]

instance Property.Null.Null QuantifiedGame where
	isNull MkQuantifiedGame { getGame = game }	= Property.Null.isNull game

-- | Accessor.
getFitness :: QuantifiedGame -> Type.Mass.WeightedMean
getFitness MkQuantifiedGame { getWeightedMeanAndCriterionValues = weightedMeanAndCriterionValues }	= Metric.WeightedMeanAndCriterionValues.getWeightedMean weightedMeanAndCriterionValues

-- | Constructor.
fromGame :: (
#ifdef USE_UNBOXED_ARRAYS
	Data.Array.Unboxed.IArray	Data.Array.Unboxed.UArray pieceSquareValue,	-- Requires 'FlexibleContexts'. The unboxed representation of the array-element must be defined (& therefore must be of fixed size).
#endif
	Fractional			pieceSquareValue,
	Real				pieceSquareValue
 )
	=> Maybe pieceSquareValue	-- ^ The value for the specified game.
	-> Model.Game.Game		-- ^ The current state of the /game/.
	-> Input.EvaluationOptions.Reader pieceSquareValue QuantifiedGame
fromGame maybePieceSquareValue game	= MkQuantifiedGame game `fmap` Evaluation.Fitness.evaluateFitness maybePieceSquareValue game

-- | Retrieve the /turn/ used to generate the selected /game/.
getLastTurn :: QuantifiedGame -> Component.Turn.Turn
getLastTurn MkQuantifiedGame { getGame = game }	= Data.Maybe.fromMaybe (
	Control.Exception.throw $ Data.Exception.mkResultUndefined "BishBosh.Evaluation.QuantifiedGame.getLastTurn:\tzero turns have been made."
 ) $ Model.Game.maybeLastTurn game

-- | Drop the specified number of plies from the start of the chronological sequence, leaving the most recent.
getLatestTurns
	:: Type.Count.NPlies	-- ^ The number of plies to drop from the start of the chronological sequence.
	-> QuantifiedGame
	-> [Component.Turn.Turn]
getLatestTurns nPlies MkQuantifiedGame { getGame = game }	= fromIntegral nPlies `drop` Model.Game.listTurnsChronologically game

-- | Represent the /fitness/ of the /game/ resulting from a future /move/ by the opponent, from the perspective of the current player.
negateFitness :: QuantifiedGame -> QuantifiedGame
negateFitness quantifiedGame@MkQuantifiedGame { getWeightedMeanAndCriterionValues = weightedMeanAndCriterionValues }	= quantifiedGame { getWeightedMeanAndCriterionValues = Metric.WeightedMeanAndCriterionValues.negateWeightedMean weightedMeanAndCriterionValues }

-- | Compares fitness.
compareFitness :: QuantifiedGame -> QuantifiedGame -> Ordering
compareFitness	= Data.Ord.comparing getFitness

{- |
	* The open interval in which to search for better solutions.

	* N.B.: 'Nothing' is interpreted as unbounded.
-}
type OpenInterval	= (Maybe QuantifiedGame, Maybe QuantifiedGame)

-- | Constant.
unboundedInterval :: OpenInterval
unboundedInterval	= (Nothing, Nothing)

-- | Reflect the interval about zero.
negateInterval :: OpenInterval -> OpenInterval
negateInterval (maybeAlpha, maybeBeta)	= ($ maybeBeta) &&& ($ maybeAlpha) $ fmap negateFitness

