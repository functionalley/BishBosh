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
import qualified	BishBosh.Attribute.WeightedMeanAndCriterionValues	as Attribute.WeightedMeanAndCriterionValues
import qualified	BishBosh.Component.Move					as Component.Move
import qualified	BishBosh.Component.Turn					as Component.Turn
import qualified	BishBosh.Data.Exception					as Data.Exception
import qualified	BishBosh.Evaluation.Fitness				as Evaluation.Fitness
import qualified	BishBosh.Input.EvaluationOptions			as Input.EvaluationOptions
import qualified	BishBosh.Model.Game					as Model.Game
import qualified	BishBosh.Notation.MoveNotation				as Notation.MoveNotation
import qualified	BishBosh.Property.Null					as Property.Null
import qualified	BishBosh.Text.ShowList					as Text.ShowList
import qualified	BishBosh.Types						as T
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Maybe
import qualified	Data.Ord

#ifdef USE_UNBOXED_ARRAYS
import qualified	Data.Array.Unboxed
#endif

-- | The selected /game/ & the criteria against which it was quantified.
data QuantifiedGame x y criterionValue weightedMean	= MkQuantifiedGame {
	getGame					:: Model.Game.Game x y,	-- ^ The /game/ resulting from a sequence of /turn/s.
	getWeightedMeanAndCriterionValues	:: Attribute.WeightedMeanAndCriterionValues.WeightedMeanAndCriterionValues weightedMean criterionValue
} deriving (Eq, Show)

instance Control.DeepSeq.NFData weightedMean => Control.DeepSeq.NFData (QuantifiedGame x y criterionValue weightedMean) where
	rnf MkQuantifiedGame { getWeightedMeanAndCriterionValues = weightedMeanAndCriterionValues }	= Control.DeepSeq.rnf weightedMeanAndCriterionValues	-- The other field is a prerequisite.

instance (Enum x, Enum y, Real criterionValue, Real weightedMean) => Notation.MoveNotation.ShowNotationFloat (QuantifiedGame x y criterionValue weightedMean) where
	showsNotationFloat moveNotation showsDouble quantifiedGame	= Text.ShowList.showsAssociationList Text.ShowList.showsSeparator $ map ($ quantifiedGame) [
		(,) Component.Move.tag . Notation.MoveNotation.showsNotation moveNotation . getLastTurn,
		(,) Attribute.WeightedMeanAndCriterionValues.weightedMeanTag . showsDouble . realToFrac . getFitness,
		(,) Attribute.WeightedMeanAndCriterionValues.criterionValuesTag . Text.ShowList.showsFormattedList' (showsDouble . realToFrac) . Attribute.WeightedMeanAndCriterionValues.getCriterionValues . getWeightedMeanAndCriterionValues
	 ]

instance Property.Null.Null (QuantifiedGame x y criterionValue weightedMean) where
	isNull MkQuantifiedGame { getGame = game }	= Property.Null.isNull game

-- | Accessor.
getFitness :: QuantifiedGame x y criterionValue weightedMean -> weightedMean
getFitness MkQuantifiedGame { getWeightedMeanAndCriterionValues = weightedMeanAndCriterionValues }	= Attribute.WeightedMeanAndCriterionValues.getWeightedMean weightedMeanAndCriterionValues

-- | Like 'fromGame' except that the caller determines the piece-square value.
fromGame :: (
#ifdef USE_PARALLEL
	Control.DeepSeq.NFData					criterionValue,
#endif
#ifdef USE_UNBOXED_ARRAYS
	Data.Array.Unboxed.IArray Data.Array.Unboxed.UArray	pieceSquareValue,	-- Requires 'FlexibleContexts'. The unboxed representation of the array-element must be defined (& therefore must be of fixed size).
#endif
	Enum							x,
	Enum							y,
	Fractional						criterionValue,
	Fractional						pieceSquareValue,
	Fractional						rankValue,
	Fractional						weightedMean,
	Ord							x,
	Ord							y,
	Real							criterionValue,
	Real							criterionWeight,
	Real							pieceSquareValue,
	Real							rankValue,
	Show							x,
	Show							y
 )
	=> Maybe pieceSquareValue	-- ^ The value for the specified game.
	-> Model.Game.Game x y		-- ^ The current state of the /game/.
	-> Input.EvaluationOptions.Reader criterionWeight pieceSquareValue rankValue x y (QuantifiedGame x y criterionValue weightedMean)
{-# SPECIALISE fromGame :: Maybe T.PieceSquareValue -> Model.Game.Game T.X T.Y -> Input.EvaluationOptions.Reader T.CriterionWeight T.PieceSquareValue T.RankValue T.X T.Y (QuantifiedGame T.X T.Y T.CriterionValue T.WeightedMean) #-}
fromGame maybePieceSquareValue game	= MkQuantifiedGame game `fmap` Evaluation.Fitness.evaluateFitness maybePieceSquareValue game

-- | Retrieve the /turn/ used to generate the selected /game/.
getLastTurn :: QuantifiedGame x y criterionValue weightedMean -> Component.Turn.Turn x y
getLastTurn MkQuantifiedGame { getGame = game }	= Data.Maybe.fromMaybe (
	Control.Exception.throw $ Data.Exception.mkResultUndefined "BishBosh.Evaluation.QuantifiedGame.getLastTurn:\tzero turns have been made."
 ) $ Model.Game.maybeLastTurn game

-- | Drop the specified number of old turns from the start of the chronological sequence, leaving the most recent.
getLatestTurns
	:: Component.Move.NPlies
	-> QuantifiedGame x y criterionValue weightedMean
	-> [Component.Turn.Turn x y]
getLatestTurns nPlies MkQuantifiedGame { getGame = game }	= drop nPlies $ Model.Game.listTurnsChronologically game

-- | Represent the /fitness/ of the /game/ resulting from a future /move/ by the opponent, from the perspective of the current player.
negateFitness :: Num weightedMean => QuantifiedGame x y criterionValue weightedMean -> QuantifiedGame x y criterionValue weightedMean
negateFitness quantifiedGame@MkQuantifiedGame { getWeightedMeanAndCriterionValues = weightedMeanAndCriterionValues }	= quantifiedGame { getWeightedMeanAndCriterionValues = Attribute.WeightedMeanAndCriterionValues.negateWeightedMean weightedMeanAndCriterionValues }

-- | Compares fitness.
compareFitness
	:: Ord weightedMean
	=> QuantifiedGame x y criterionValue weightedMean
	-> QuantifiedGame x y criterionValue weightedMean
	-> Ordering
compareFitness	= Data.Ord.comparing getFitness

{- |
	* The open interval in which to search for better solutions.

	* N.B.: 'Nothing' is interpreted as unbounded.
-}
type OpenInterval x y criterionValue weightedMean	= (Maybe (QuantifiedGame x y criterionValue weightedMean), Maybe (QuantifiedGame x y criterionValue weightedMean))

-- | Constant.
unboundedInterval :: OpenInterval x y criterionValue weightedMean
unboundedInterval	= (Nothing, Nothing)

-- | Reflect the interval about zero.
negateInterval :: Num weightedMean => OpenInterval x y criterionValue weightedMean -> OpenInterval x y criterionValue weightedMean
negateInterval (maybeAlpha, maybeBeta)	= ($ maybeBeta) &&& ($ maybeAlpha) $ fmap negateFitness
