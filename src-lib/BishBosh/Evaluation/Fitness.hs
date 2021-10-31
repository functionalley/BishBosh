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

	* Quantifies the fitness of a game.

	* By measuring the fitness from the perspective of the player who just moved (rather than the next player to move),
	an automated player can test various /move/s & select the fittest.
-}

module BishBosh.Evaluation.Fitness(
-- * Types
-- * Constants
--	maximumDestinations,
	maximumDefended,
-- * Functions
	measurePieceSquareValue,
	measurePieceSquareValueIncrementally,
	measureValueOfMaterial,
--	measureValueOfMobility,
	measureValueOfCastlingPotential,
	measureValueOfDefence,
	measureValueOfDoubledPawns,
	measureValueOfIsolatedPawns,
	measureValueOfPassedPawns,
	evaluateFitness
) where

import			Control.Applicative((<|>))
import			Control.Arrow((&&&))
import			Data.Array.IArray((!))
import qualified	BishBosh.Attribute.LogicalColour			as Attribute.LogicalColour
import qualified	BishBosh.Attribute.MoveType				as Attribute.MoveType
import qualified	BishBosh.Cartesian.Abscissa				as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates				as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate				as Cartesian.Ordinate
import qualified	BishBosh.Component.Move					as Component.Move
import qualified	BishBosh.Component.Piece				as Component.Piece
import qualified	BishBosh.Component.PieceSquareByCoordinatesByRank	as Component.PieceSquareByCoordinatesByRank
import qualified	BishBosh.Component.QualifiedMove			as Component.QualifiedMove
import qualified	BishBosh.Component.Turn					as Component.Turn
import qualified	BishBosh.Input.CriteriaWeights				as Input.CriteriaWeights
import qualified	BishBosh.Input.EvaluationOptions			as Input.EvaluationOptions
import qualified	BishBosh.Input.RankValues				as Input.RankValues
import qualified	BishBosh.Metric.CriterionValue				as Metric.CriterionValue
import qualified	BishBosh.Metric.WeightedMeanAndCriterionValues		as Metric.WeightedMeanAndCriterionValues
import qualified	BishBosh.Model.Game					as Model.Game
import qualified	BishBosh.Property.Opposable				as Property.Opposable
import qualified	BishBosh.Rule.GameTerminationReason			as Rule.GameTerminationReason
import qualified	BishBosh.State.Board					as State.Board
import qualified	BishBosh.State.CastleableRooksByLogicalColour		as State.CastleableRooksByLogicalColour
import qualified	BishBosh.Type.Count					as Type.Count
import qualified	BishBosh.Type.Mass					as Type.Mass
import qualified	Control.Monad.Reader
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.Map.Strict						as Map
import qualified	Data.Maybe

#ifdef USE_UNBOXED_ARRAYS
import qualified	Data.Array.Unboxed
#endif

-- | Measures the piece-square value from the perspective of the last player to move.
measurePieceSquareValue :: (
#ifdef USE_UNBOXED_ARRAYS
	Data.Array.Unboxed.IArray	Data.Array.Unboxed.UArray pieceSquareValue,	-- Requires 'FlexibleContexts'. The unboxed representation of the array-element must be defined (& therefore must be of fixed size).
#endif
	Num				pieceSquareValue
 )
	=> Component.PieceSquareByCoordinatesByRank.PieceSquareByCoordinatesByRank pieceSquareValue
	-> Model.Game.Game
	-> pieceSquareValue
{-# SPECIALISE measurePieceSquareValue :: Component.PieceSquareByCoordinatesByRank.PieceSquareByCoordinatesByRank Type.Mass.PieceSquareValue -> Model.Game.Game -> Type.Mass.PieceSquareValue #-}
measurePieceSquareValue pieceSquareByCoordinatesByRank game	= (
	if Attribute.LogicalColour.isBlack $ Model.Game.getNextLogicalColour game
		then id
		else negate	-- Represent the piece-square value from Black's perspective.
 ) $ whitesPieceSquareValue - blacksPieceSquareValue where
	[blacksPieceSquareValue, whitesPieceSquareValue]	= Data.Array.IArray.elems . State.Board.sumPieceSquareValueByLogicalColour pieceSquareByCoordinatesByRank $ Model.Game.getBoard game

{- |
	* Measures the piece-square value from the perspective of the last player to move.

	* The previous value is provided, to enable calculation by difference.

	* N.B.: because of diminishing returns, the piece-square value for everything but quiet moves is calculated from scratch.
-}
measurePieceSquareValueIncrementally :: (
#ifdef USE_UNBOXED_ARRAYS
	Data.Array.Unboxed.IArray	Data.Array.Unboxed.UArray pieceSquareValue,	-- Requires 'FlexibleContexts'. The unboxed representation of the array-element must be defined (& therefore must be of fixed size).
#endif
	Num				pieceSquareValue
 )
	=> pieceSquareValue	-- ^ The value before the last move was applied, & therefore also from the perspective of the previous player.
	-> Component.PieceSquareByCoordinatesByRank.PieceSquareByCoordinatesByRank pieceSquareValue
	-> Model.Game.Game
	-> pieceSquareValue
{-# SPECIALISE measurePieceSquareValueIncrementally :: Type.Mass.PieceSquareValue -> Component.PieceSquareByCoordinatesByRank.PieceSquareByCoordinatesByRank Type.Mass.PieceSquareValue -> Model.Game.Game -> Type.Mass.PieceSquareValue #-}
measurePieceSquareValueIncrementally previousPieceSquareValue pieceSquareByCoordinatesByRank game
	| Attribute.MoveType.isSimple $ Component.QualifiedMove.getMoveType qualifiedMove	= let
		findPieceSquareValue	= uncurry (
			Component.PieceSquareByCoordinatesByRank.findPieceSquareValue pieceSquareByCoordinatesByRank
		 ) (
			State.Board.getNPieces {- N.B.: no capture occurred-} . Model.Game.getBoard &&& Property.Opposable.getOpposite . Model.Game.getNextLogicalColour $ game	{-the last player to move-}
		 ) (
			Component.Turn.getRank turn	-- N.B.: no promotion occurred.
		 )
	in uncurry (-) (
		findPieceSquareValue . Component.Move.getDestination &&& findPieceSquareValue . Component.Move.getSource $ Component.QualifiedMove.getMove qualifiedMove
	) - previousPieceSquareValue {-from the previous player's perspective-}
	| otherwise	= measurePieceSquareValue pieceSquareByCoordinatesByRank game	-- N.B.: though non-simple (Castling, En-passant, promotion) can be calculated, the returns don't justify the effort.
	where
		Just turn	= Model.Game.maybeLastTurn game
		qualifiedMove	= Component.Turn.getQualifiedMove turn

-- | Measure the arithmetic difference between the total /rank-value/ of the /piece/s currently held by either side; <https://www.chessprogramming.org/Material>.
measureValueOfMaterial
	:: Input.RankValues.RankValues
	-> Type.Mass.RankValue	-- ^ Maximum total rank-value.
	-> Model.Game.Game
	-> Metric.CriterionValue.CriterionValue
measureValueOfMaterial rankValues maximumTotalRankValue game	= realToFrac . (
	/ maximumTotalRankValue	-- Normalise.
 ) . (
	if Attribute.LogicalColour.isBlack $ Model.Game.getNextLogicalColour game
		then id		-- White just moved.
		else negate	-- Black just moved.
 ) . Data.List.foldl' (
	\acc (rank, nPiecesDifference) -> if nPiecesDifference == 0
		then acc	-- Avoid calling 'Input.RankValues.findRankValue'.
		else acc + realToFrac (
			Input.RankValues.findRankValue rank rankValues
		) * fromIntegral nPiecesDifference
 ) 0 . Data.Array.IArray.assocs . State.Board.getNPiecesDifferenceByRank {-which arbitrarily counts White pieces as positive & Black as negative-} $ Model.Game.getBoard game

{- |
	* Count the difference between the reciprocals (cf. <https://www.chessprogramming.org/Mobility>), of the total number of /move/s available to each player.

	* Using the reciprocal facilitates mapping into the /closed unit-interval/, & also emphasises the difference between having just one available move & having zero (i.e. mate).
	In consequence, it is more about restricting the opponent's mobility (particularly the @King@) rather than increasing one's own.
	This metric drives the game towards check-mate, rather than merely fighting a war of attrition.

	* CAVEAT: avoiding a reduction of one's mobility to zero (i.e. mate) must be paramount => losing one's @Queen@ should be preferable.
	measureValueOfMobility = 1 when mobility = 0, whereas loss of a @Queen@ = @ (rankValues ! Queen) / maximumTotalRankValue @,
	=> getWeightOfMobility * 1 > weightOfMaterial * (8.8 / 102.47)
	=> getWeightOfMobility > weightOfMaterial / 11.6

	The corollary is that one probably shouldn't sacrifice even a @Knight@ to temporarily reduce one's opponent's mobility to one.
	measureValueOfMobility = 0.5 when mobility = 1,
	=> getWeightOfMobility * 0.5 < weightOfMaterial * (3.2 / 102.47)
	=> getWeightOfMobility < weightOfMaterial / 16.0
	CAVEAT: the loss of a @Knight@ occurs on the subsequent turn & is therefore downgraded, so even this represents too high a weighting.

	This presents a paradox !
-}
measureValueOfMobility :: Model.Game.Game -> Metric.CriterionValue.CriterionValue
measureValueOfMobility game	= realToFrac . uncurry (-) . (
	measureConstriction &&& measureConstriction . Property.Opposable.getOpposite {-recent mover-}
 ) $ Model.Game.getNextLogicalColour game where
	measureConstriction :: Attribute.LogicalColour.LogicalColour -> Type.Mass.CriterionValue
	measureConstriction logicalColour	= recip . fromIntegral {-NPlies-} . succ {-avoid divide-by-zero-} $ Model.Game.countPliesAvailableTo logicalColour game

-- | Measure the arithmetic difference between the potential to /Castle/, on either side.
measureValueOfCastlingPotential :: Model.Game.Game -> Metric.CriterionValue.CriterionValue
measureValueOfCastlingPotential game	= realToFrac . uncurry (-) . (
	castlingPotential . Property.Opposable.getOpposite {-recent mover-} &&& castlingPotential
 ) $ Model.Game.getNextLogicalColour game where
	castlingPotential :: Attribute.LogicalColour.LogicalColour -> Type.Mass.CriterionValue
	castlingPotential	= Data.Maybe.maybe 1 {-have Castled-} (
		(/ 2) . fromIntegral . length
	 ) . (
		`State.CastleableRooksByLogicalColour.locateForLogicalColour` Model.Game.getCastleableRooksByLogicalColour game
	 )

{- |
	* Measure the arithmetic difference between the number of /doubled/ @Pawn@s on either side; <https://www.chessprogramming.org/Doubled_Pawn>.

	* N.B.: measures tripled @Pawn@s as equivalent to two doubled @Pawn@s.

	* CAVEAT: this is a negative attribute, so the weighted normalised value shouldn't exceed the reduction due to 'measureValueOfMaterial' resulting from a @Pawn@-sacrifice.
-}
measureValueOfDoubledPawns :: Model.Game.Game -> Metric.CriterionValue.CriterionValue
measureValueOfDoubledPawns game	= realToFrac . (
	/ (
		6	:: Type.Mass.CriterionValue	-- Normalise to [-1 .. 1]; the optimal scenario is all files containing one Pawn; the worst scenario is two files each containing four Pawns, all but one per file of which are counted as doubled.
	)
 ) . fromIntegral {-NPieces-} . uncurry (-) . (
	countDoubledPawns &&& countDoubledPawns . Property.Opposable.getOpposite {-recent mover-}
 ) $ Model.Game.getNextLogicalColour game where
	countDoubledPawns :: Attribute.LogicalColour.LogicalColour -> Type.Count.NPieces
	countDoubledPawns logicalColour	= uncurry (-) . (
		Data.Foldable.foldl' (+) 0 &&& fromIntegral . Data.Foldable.length {-one Pawn can't be considered to be doubled, so substract one Pawn per column-}
	 ) $ State.Board.getNPawnsByFileByLogicalColour (Model.Game.getBoard game) ! logicalColour

{- |
	* Measure the arithmetic difference between the number of /isolated/ @Pawn@s on either side; <https://www.chessprogramming.org/Isolated_Pawn>.

	* CAVEAT: this is a negative attribute, so the weighted normalised value shouldn't exceed the reduction due to 'measureValueOfMaterial' resulting from a @Pawn@-sacrifice.
-}
measureValueOfIsolatedPawns :: Model.Game.Game -> Metric.CriterionValue.CriterionValue
measureValueOfIsolatedPawns game	= realToFrac . (
	/ (
		fromIntegral {-X-} Cartesian.Abscissa.xLength	:: Type.Mass.CriterionValue	-- Normalise to [-1 .. 1]; the optimal scenario is eight files each containing one Pawn & the worst scenario is all Pawns isolated (e.g. 4 alternate files of 2, 2 separate files or 4, ...).
	)
 ) . fromIntegral {-NPieces-} . uncurry (-) . (
	countIsolatedPawns &&& countIsolatedPawns . Property.Opposable.getOpposite {-recent mover-}
 ) $ Model.Game.getNextLogicalColour game where
	countIsolatedPawns :: Attribute.LogicalColour.LogicalColour -> Type.Count.NPieces
	countIsolatedPawns logicalColour	= Map.foldlWithKey' (
		\acc x nPawns -> if (`Map.member` nPawnsByFile) `any` Cartesian.Abscissa.getAdjacents x
			then acc		-- This file has at least one neighbouring Pawn which can (if at a suitable rank) be used to protect any of those in this file.
			else acc + nPawns	-- All the Pawns on this file are isolated & thus lack the protection that may be offered by adjacent Pawns.
	 ) 0 nPawnsByFile where
		nPawnsByFile	= State.Board.getNPawnsByFileByLogicalColour (Model.Game.getBoard game) ! logicalColour

-- | Measure the arithmetic difference between the number of /passed/ @Pawn@s on either side; <https://www.chessprogramming.org/Passed_Pawn>.
measureValueOfPassedPawns :: Model.Game.Game -> Metric.CriterionValue.CriterionValue
measureValueOfPassedPawns game	= realToFrac . (
	/ fromIntegral {-X-} Cartesian.Abscissa.xLength	-- Normalise to [-1 .. 1]; the optimal scenario is all files containing exactly one Pawn, of one's own logical colour, on the 7th rank.
 ) . uncurry (-) . (
	valuePassedPawns . Property.Opposable.getOpposite {-recent mover-} &&& valuePassedPawns
 ) $ Model.Game.getNextLogicalColour game where
	valuePassedPawns :: Attribute.LogicalColour.LogicalColour -> Type.Mass.CriterionValue
	valuePassedPawns logicalColour	= Data.List.foldl' (
		\acc -> (acc +) . recip {-value increases exponentially as distance to promotion decreases-} . fromIntegral . abs . subtract (
			Cartesian.Ordinate.lastRank logicalColour
		) . Cartesian.Coordinates.getY	-- Measure the distance to promotion.
	 ) 0 $ State.Board.getPassedPawnCoordinatesByLogicalColour (Model.Game.getBoard game) ! logicalColour

{- |
	* The constant maximum total number of times the /piece/s of either side, can be defended.

	* Assumes all Pawns have been Queened.

	* CAVEAT: assuming the optimal arrangement of pieces:

	RQQB	= 3 + 7 + 3 + 2	= 15
	QQQN	= 4 + 6 + 8 + 4	= 22
	NQQK	= 4 + 8 + 6 + 0	= 18
	BQQR	= 2 + 3 + 7 + 3	= 15
				= 70
-}
maximumDefended :: Type.Count.NPieces
maximumDefended	= 70

{- |
	* Measure the normalised arithmetic difference between the number of /piece/s defending each of one's own, on either side.

	* N.B. the /rank-value/ of the defended /piece/ is irrelevant because; it's the unknown value of the attacker that counts, since that's what the defender has the opportunity to counter-strike.
	CAVEAT: the validity of this depends on the duration of the battle.

	* N.B. defence of the @King@ is irrelevent, because it can't be taken.

	* N.B. it's the total number of defenders which is relevant, rather than whether each piece has some protection, since it's not the individual battles but the war which counts.

	* CAVEAT: this criterion competes with /mobility/, since each defended /piece/ blocks the path of the defender.
-}
measureValueOfDefence :: Model.Game.Game -> Metric.CriterionValue.CriterionValue
measureValueOfDefence game	= realToFrac . (
	/ (
		fromIntegral {-NPieces-} maximumDefended	:: Type.Mass.CriterionValue	-- Normalise.
	)
 ) . fromIntegral {-NPieces-} . uncurry (-) . (
	(! Property.Opposable.getOpposite {-recent mover-} nextLogicalColour) &&& (! nextLogicalColour)
 ) . State.Board.summariseNDefendersByLogicalColour $ Model.Game.getBoard game where
	nextLogicalColour	= Model.Game.getNextLogicalColour game

{- |
	* Evaluates the fitness of the /board/ from the perspective of the last player to move.
	If the game has ended, the fitness is maximum for checkmate or zero for a draw,
	but otherwise is the /weighted mean/ of various criteria; <https://www.chessprogramming.org/Evaluation>.

	* Also returns the break-down of those /criterion-value/s with a non-zero /criterion-weight/.

	* Besides measuring the difference between the total /rank-value/ on either side, other criteria are selected to represent known attributes of a good position.

	* Many possible criteria aren't measured because they're, either currently or imminently, represented by those that are, typically by 'measureValueOfMaterial'.
-}
evaluateFitness :: (
#ifdef USE_UNBOXED_ARRAYS
	Data.Array.Unboxed.IArray	Data.Array.Unboxed.UArray pieceSquareValue,	-- Requires 'FlexibleContexts'. The unboxed representation of the array-element must be defined (& therefore must be of fixed size).
#endif
	Fractional			pieceSquareValue,
	Real				pieceSquareValue
 )
	=> Maybe pieceSquareValue	-- ^ An optional value for the specified game.
	-> Model.Game.Game
	-> Input.EvaluationOptions.Reader pieceSquareValue Metric.WeightedMeanAndCriterionValues.WeightedMeanAndCriterionValues
{-# SPECIALISE evaluateFitness :: Maybe Type.Mass.PieceSquareValue -> Model.Game.Game -> Input.EvaluationOptions.Reader Type.Mass.PieceSquareValue Metric.WeightedMeanAndCriterionValues.WeightedMeanAndCriterionValues #-}
evaluateFitness maybePieceSquareValue game
	| Just gameTerminationReason <- Model.Game.getMaybeTerminationReason game	= return {-to Reader-monad-} $ Metric.WeightedMeanAndCriterionValues.mkWeightedMeanAndCriterionValues (
		if Rule.GameTerminationReason.isCheckMate gameTerminationReason
			then 1	-- The last player to move, has won.
			else 0	-- A draw.
	) []
	| otherwise	= do
		criteriaWeights				<- Control.Monad.Reader.asks Input.EvaluationOptions.getCriteriaWeights
		rankValuePair				<- Control.Monad.Reader.asks $ Input.EvaluationOptions.getRankValues &&& Input.EvaluationOptions.getMaximumTotalRankValue
		maybePieceSquareByCoordinatesByRank	<- Control.Monad.Reader.asks Input.EvaluationOptions.getMaybePieceSquareByCoordinatesByRank

		return {-to Reader-monad-} $ Input.CriteriaWeights.calculateWeightedMean criteriaWeights (
			uncurry measureValueOfMaterial rankValuePair game
		 ) (
			measureValueOfMobility game
		 ) (
			Data.Maybe.maybe 0 (
				realToFrac . (/ fromIntegral Component.Piece.nPiecesPerSide)
			) $ maybePieceSquareValue <|> fmap (`measurePieceSquareValue` game) maybePieceSquareByCoordinatesByRank
		 ) (
			measureValueOfCastlingPotential game
		 ) (
			measureValueOfDefence game
		 ) (
			measureValueOfDoubledPawns game
		 ) (
			measureValueOfIsolatedPawns game
		 ) (
			measureValueOfPassedPawns game
		 )

