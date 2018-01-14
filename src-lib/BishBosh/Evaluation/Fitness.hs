{-# LANGUAGE ScopedTypeVariables #-}
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
-- * Constants
--	maximumDestinations,
	maximumDefended,
-- * Functions
--	mkPieceSquareCriterionValue,
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
import qualified	BishBosh.Attribute.CriterionValue			as Attribute.CriterionValue
import qualified	BishBosh.Attribute.Direction				as Attribute.Direction
import qualified	BishBosh.Attribute.LogicalColour			as Attribute.LogicalColour
import qualified	BishBosh.Attribute.MoveType				as Attribute.MoveType
import qualified	BishBosh.Attribute.RankValues				as Attribute.RankValues
import qualified	BishBosh.Attribute.WeightedMeanAndCriterionValues	as Attribute.WeightedMeanAndCriterionValues
import qualified	BishBosh.Cartesian.Abscissa				as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates				as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate				as Cartesian.Ordinate
import qualified	BishBosh.Component.Move					as Component.Move
import qualified	BishBosh.Component.Piece				as Component.Piece
import qualified	BishBosh.Component.PieceSquareArray			as Component.PieceSquareArray
import qualified	BishBosh.Component.QualifiedMove			as Component.QualifiedMove
import qualified	BishBosh.Component.Turn					as Component.Turn
import qualified	BishBosh.Input.CriteriaWeights				as Input.CriteriaWeights
import qualified	BishBosh.Input.EvaluationOptions			as Input.EvaluationOptions
import qualified	BishBosh.Model.Game					as Model.Game
import qualified	BishBosh.Model.GameTerminationReason			as Model.GameTerminationReason
import qualified	BishBosh.Property.Opposable				as Property.Opposable
import qualified	BishBosh.State.Board					as State.Board
import qualified	BishBosh.State.CastleableRooksByLogicalColour		as State.CastleableRooksByLogicalColour
import qualified	BishBosh.Types						as T
import qualified	Control.Monad.Reader
import qualified	Data.Array.IArray
import qualified	Data.List
import qualified	Data.Map
import qualified	Data.Maybe

-- | Construct a criterion-value from a piece-square value.
mkPieceSquareCriterionValue :: (
	Fractional	criterionValue,
	Ord		criterionValue,
	Real		pieceSquareValue
 ) => pieceSquareValue -> Attribute.CriterionValue.CriterionValue criterionValue
mkPieceSquareCriterionValue	= Attribute.CriterionValue.mkCriterionValue . (
	/ fromIntegral Component.Piece.nPiecesPerSide
 ) . realToFrac

-- | Measures the piece-square value from the perspective of the last player to move.
measurePieceSquareValue :: (
	Enum	x,
	Enum	y,
	Num	pieceSquareValue,
	Ord	x,
	Ord	y
 )
	=> Component.PieceSquareArray.PieceSquareArray x y pieceSquareValue
	-> Model.Game.Game x y
	-> pieceSquareValue
{-# SPECIALISE measurePieceSquareValue :: Component.PieceSquareArray.PieceSquareArray T.X T.Y T.PieceSquareValue -> Model.Game.Game T.X T.Y -> T.PieceSquareValue #-}
measurePieceSquareValue pieceSquareArray game
	| Attribute.LogicalColour.isBlack $ Model.Game.getNextLogicalColour game	= difference
	| otherwise									= negate difference	-- Represent the piece-square value from Black's perspective.
	where
		[blacksPieceSquareValue, whitesPieceSquareValue]	= Data.Array.IArray.elems . State.Board.sumPieceSquareValueByLogicalColour pieceSquareArray $ Model.Game.getBoard game
		difference						= whitesPieceSquareValue - blacksPieceSquareValue

{- |
	* Measures the piece-square value from the perspective of the last player to move.

	* The previous value is provided, to enable calculation by difference.

	* N.B.: because of diminishing returns, the piece-square value for everything but quiet moves is calculated from scratch.
-}
measurePieceSquareValueIncrementally :: (
	Enum	x,
	Enum	y,
	Num	pieceSquareValue,
	Ord	x,
	Ord	y
 )
	=> pieceSquareValue	-- ^ The value before the last move was applied, & therefore also from the perspective of the previous player.
	-> Component.PieceSquareArray.PieceSquareArray x y pieceSquareValue
	-> Model.Game.Game x y
	-> pieceSquareValue
{-# SPECIALISE measurePieceSquareValueIncrementally :: T.PieceSquareValue -> Component.PieceSquareArray.PieceSquareArray T.X T.Y T.PieceSquareValue -> Model.Game.Game T.X T.Y -> T.PieceSquareValue #-}
measurePieceSquareValueIncrementally previousPieceSquareValue pieceSquareArray game
	| Attribute.MoveType.isQuiet $ Component.QualifiedMove.getMoveType qualifiedMove	= let
		findPieceSquareValue coordinates	= Component.PieceSquareArray.findPieceSquareValue (
			State.Board.getNPieces $ Model.Game.getBoard game	-- N.B.: no capture occurred.
		 ) (
			Property.Opposable.getOpposite $ Model.Game.getNextLogicalColour game	-- The last player to move.
		 ) (
			Component.Turn.getRank turn	-- N.B.: no promotion occurred.
		 ) coordinates pieceSquareArray
	in uncurry (-) (
		findPieceSquareValue . Component.Move.getDestination &&& findPieceSquareValue . Component.Move.getSource $ Component.QualifiedMove.getMove qualifiedMove
	) - previousPieceSquareValue {-from the previous player's perspective-}
	| otherwise					= measurePieceSquareValue pieceSquareArray game	-- N.B.: though Castling, En-passant, & promotion, can also be calculated, the returns don't justify the effort.
	where
		Just turn	= Model.Game.maybeLastTurn game
		qualifiedMove	= Component.Turn.getQualifiedMove turn

-- | Measure the arithmetic difference between the total /rank-value/ of the /piece/s currently held by either side; <https://chessprogramming.wikispaces.com/Material>.
measureValueOfMaterial :: (
	Fractional	criterionValue,
	Fractional	rankValue,
	Ord		criterionValue,
	Real		rankValue
 )
	=> Attribute.RankValues.RankValues rankValue
	-> Model.Game.Game x y
	-> Attribute.CriterionValue.CriterionValue criterionValue
-- {-# SPECIALISE measureValueOfMaterial :: Attribute.RankValues.RankValues T.RankValue -> Model.Game.Game T.X T.Y -> Attribute.CriterionValue.CriterionValue T.CriterionValue #-}
measureValueOfMaterial rankValues game	= Attribute.CriterionValue.mkCriterionValue . (
	/ fromIntegral Component.Piece.nPiecesPerSide	-- Normalise.
 ) . realToFrac . (
	if Attribute.LogicalColour.isBlack $ Model.Game.getNextLogicalColour game
		then id		-- White just moved.
		else negate	-- Black just moved.
 ) . Data.List.foldl' (
	\acc (rank, nPieces) -> if nPieces == 0
		then acc	-- Avoid calling 'Attribute.RankValues.findRankValue'.
		else acc + Attribute.RankValues.findRankValue rank rankValues * fromIntegral nPieces
 ) 0 . Data.Array.IArray.assocs . State.Board.getNPiecesDifferenceByRank {-which arbitrarily counts White pieces as positive & Black as negative-} $ Model.Game.getBoard game

{- |
	* Count the difference between the reciprocals (cf. <https://chessprogramming.wikispaces.com/Mobility>), of the total number of /move/s available to each player.

	* Using the reciprocal facilitates mapping into the /closed unit-interval/, & also emphasises the difference between having just one available move & having zero (i.e. mate).
	In consequence, it is more about restricting the opponent's mobility (particularly the @King@) rather than increasing one's own.
	This metric drives the game towards check-mate, rather than merely fighting a war of attrition.

	* CAVEAT: avoiding a reduction of one's mobility to zero (i.e. mate) must be paramount => losing one's @Queen@ should be preferable.
	measureValueOfMobility = 1 when mobility = 0, whereas loss of a @Queen@ = @ (rankValues ! Queen) / maximumTotalRankValue @,
	=> getWeightOfMobility * 1 > weightOfMaterial * (8.8 / 102.47)
	=> getWeightOfMobility > weightOfMaterial / 11.6

	The corollary is that one probably shouldn't sacrifice even a @Knight@ to temporarily reduce one's opponent mobility to one.
	measureValueOfMobility = 0.5 when mobility = 1,
	=> getWeightOfMobility * 0.5 < weightOfMaterial * (3.2 / 102.47)
	=> getWeightOfMobility < weightOfMaterial / 16.0
	CAVEAT: the loss of a @Knight@ occurs on the subsequent turn & is therefore downgraded, so even this represents too high a weighting.

	This presents a paradox !
-}
measureValueOfMobility :: (
	Enum		x,
	Enum		y,
	Fractional	criterionValue,
	Ord		criterionValue,
	Ord		x,
	Ord		y,
	Show		x,
	Show		y
 ) => Model.Game.Game x y -> Attribute.CriterionValue.CriterionValue criterionValue
{-# SPECIALISE measureValueOfMobility :: Model.Game.Game T.X T.Y -> Attribute.CriterionValue.CriterionValue T.CriterionValue #-}
measureValueOfMobility game	= Attribute.CriterionValue.mkCriterionValue . uncurry (-) . (
	measureConstriction &&& measureConstriction . Property.Opposable.getOpposite {-recent mover-}
 ) $ Model.Game.getNextLogicalColour game where
	measureConstriction logicalColour	= recip . fromIntegral . succ {-avoid divide-by-zero-} $ Model.Game.countMovesAvailableTo logicalColour game

-- | Measure the arithmetic difference between the potential to /Castle/, on either side.
measureValueOfCastlingPotential :: (
	Fractional	criterionValue,
	Ord		criterionValue
 ) => Model.Game.Game x y -> Attribute.CriterionValue.CriterionValue criterionValue
-- {-# SPECIALISE measureValueOfCastlingPotential :: Model.Game.Game T.X T.Y -> Attribute.CriterionValue.CriterionValue T.CriterionValue #-}
measureValueOfCastlingPotential game	= Attribute.CriterionValue.mkCriterionValue . uncurry (-) . (
	castlingPotential . Property.Opposable.getOpposite {-recent mover-} &&& castlingPotential
 ) $ Model.Game.getNextLogicalColour game where
{-
	castlingPotential logicalColour	= case State.CastleableRooksByLogicalColour.locateForLogicalColour logicalColour $ Model.Game.getCastleableRooksByLogicalColour game of
		Just []		-> 0		-- Can't castle.
		Just [_]	-> recip 2	-- Have one Rook which can castle.
		_		-> 1		-- Either have castled or can with either Rook.
-}
	castlingPotential	= Data.Maybe.maybe 1 {-have Castled-} (
		(/ 2) . fromIntegral . length
	 ) . (
		`State.CastleableRooksByLogicalColour.locateForLogicalColour` Model.Game.getCastleableRooksByLogicalColour game
	 )

{- |
	* Measure the arithmetic difference between the number of /doubled/ @Pawn@s on either side; <https://chessprogramming.wikispaces.com/Doubled+Pawn>.

	* N.B.: measures tripled @Pawn@s as equivalent to two doubled @Pawn@s.

	* CAVEAT: this is a negative attribute, so the weighted normalised value shouldn't exceed the reduction due to 'measureValueOfMaterial' resulting from a @Pawn@-sacrifice.
-}
measureValueOfDoubledPawns :: (
	Fractional	criterionValue,
	Ord		criterionValue
 ) => Model.Game.Game x y -> Attribute.CriterionValue.CriterionValue criterionValue
-- {-# SPECIALISE measureValueOfDoubledPawns :: Model.Game.Game T.X T.Y -> Attribute.CriterionValue.CriterionValue T.CriterionValue #-}
measureValueOfDoubledPawns game	= Attribute.CriterionValue.mkCriterionValue . (
	/ 6	-- Normalise to [-1 .. 1]; the optimal scenario is eight files each containing one Pawn; the worst scenario is two files each containing four Pawns, all but one per file of which are counted as doubled.
 ) . fromIntegral . uncurry (-) . (
	countDoubledPawns &&& countDoubledPawns . Property.Opposable.getOpposite {-recent mover-}
 ) $ Model.Game.getNextLogicalColour game where
	countDoubledPawns logicalColour	= uncurry (-) . (
		Data.Map.foldl' (+) 0 &&& Data.Map.size {-one Pawn can't be considered to be doubled, so substract one Pawn per column-}
	 ) $ State.Board.getNPawnsByFileByLogicalColour (Model.Game.getBoard game) ! logicalColour

{- |
	* Measure the arithmetic difference between the number of /isolated/ @Pawn@s on either side; <https://chessprogramming.wikispaces.com/Isolated+Pawn>.

	* CAVEAT: this is a negative attribute, so the weighted normalised value shouldn't exceed the reduction due to 'measureValueOfMaterial' resulting from a @Pawn@-sacrifice.
-}
measureValueOfIsolatedPawns :: (
	Enum		x,
	Fractional	criterionValue,
	Ord		criterionValue,
	Ord		x
 ) => Model.Game.Game x y -> Attribute.CriterionValue.CriterionValue criterionValue
{-# SPECIALISE measureValueOfIsolatedPawns :: Model.Game.Game T.X T.Y -> Attribute.CriterionValue.CriterionValue T.CriterionValue #-}
measureValueOfIsolatedPawns game	= Attribute.CriterionValue.mkCriterionValue . (
	/ fromIntegral Cartesian.Abscissa.xLength	-- Normalise to [-1 .. 1]; the optimal scenario is eight files each containing one Pawn & the worst scenario is all Pawns isolated (e.g. 4 alternate files of 2, 2 separate files or 4, ...).
 ) . fromIntegral . uncurry (-) . (
	countIsolatedPawns &&& countIsolatedPawns . Property.Opposable.getOpposite {-recent mover-}
 ) $ Model.Game.getNextLogicalColour game where
	countIsolatedPawns :: Attribute.LogicalColour.LogicalColour -> Component.Piece.NPieces
	countIsolatedPawns logicalColour	= Data.Map.foldlWithKey' (
		\acc x nPawns -> (
			if (`Data.Map.notMember` nPawnsByFile) `all` Cartesian.Abscissa.getAdjacents x
				then (+ nPawns)	-- All the Pawns on this file are isolated & thus lack the protection that may be offered by adjacent Pawns.
				else id		-- This file has at least one neighbouring Pawn which can (if at a suitable rank) be used to protect any of those in this file.
		) acc
	 ) 0 nPawnsByFile where
		nPawnsByFile	= State.Board.getNPawnsByFileByLogicalColour (Model.Game.getBoard game) ! logicalColour

-- | Measure the arithmetic difference between the number of /passed/ @Pawn@s on either side; <https://chessprogramming.wikispaces.com/Passed+Pawn>.
measureValueOfPassedPawns :: forall x y criterionValue. (
	Enum		y,
	Fractional	criterionValue,
	Ord		criterionValue
 ) => Model.Game.Game x y -> Attribute.CriterionValue.CriterionValue criterionValue
{-# SPECIALISE measureValueOfPassedPawns :: Model.Game.Game T.X T.Y -> Attribute.CriterionValue.CriterionValue T.CriterionValue #-}
measureValueOfPassedPawns game	= Attribute.CriterionValue.mkCriterionValue . (
	/ fromIntegral Cartesian.Abscissa.xLength	-- Normalise to [-1 .. 1].
 ) . uncurry (-) . (
	valuePassedPawns . Property.Opposable.getOpposite {-recent mover-} &&& valuePassedPawns
 ) $ Model.Game.getNextLogicalColour game where
	valuePassedPawns :: Attribute.LogicalColour.LogicalColour -> criterionValue
	valuePassedPawns logicalColour	= Data.List.foldl' (
		\acc -> (acc +) . recip {-low distance has high value-} . fromIntegral . abs . (
			+ fromEnum (
				Cartesian.Ordinate.lastRank logicalColour	:: y
			)
		) . negate . fromEnum . Cartesian.Coordinates.getY	-- Measure the distance to promotion.
	 ) 0 $ State.Board.getPassedPawnCoordinatesByLogicalColour (Model.Game.getBoard game) ! logicalColour

{- |
	* The constant maximum total number of times the /piece/s of either side, can be defended.

	* This calculation assumes that:
		every /piece/ can defend another in every /direction/ it can attack,
		which is impossible, since in a 2-D board one can always draw a perimeter around the /piece/s, beyond which there're zero /pieces/ to defend, so the outer /piece/s can never be fully utilised;
		all @Pawn@s have been /queened/, which is unrealistic.
-}
maximumDefended :: Component.Piece.NPieces
maximumDefended	= (9 {-Queens-} + 1 {-King-} + 2 {-Knights-} + 2 {-Rooks + Bishops-}) * Attribute.Direction.nDistinctDirections

{- |
	* Measure the normalised arithmetic difference between the number of /piece/s defending each of one's own, on either side.

	* N.B. the /rank-value/ of the defended /piece/ is irrelevant because; it's the unknown value of the attacker that counts, since that's what the defender has the opportunity to counter-strike.

	* N.B. defence of the @King@ is irrelevent, because it can't be taken.

	* N.B. it's the total number of defenders which is relevant, rather than whether each piece has some protection, since it's the individual battles but the war which counts.

	* CAVEAT: this criterion competes with /mobility/, since each defended /piece/ blocks the path of the defender.
-}
measureValueOfDefence :: (
	Fractional	criterionValue,
	Ord		criterionValue
 ) => Model.Game.Game x y -> Attribute.CriterionValue.CriterionValue criterionValue
-- {-# SPECIALISE measureValueOfDefence :: Model.Game.Game T.X T.Y -> Attribute.CriterionValue.CriterionValue T.CriterionValue #-}
measureValueOfDefence game	= Attribute.CriterionValue.mkCriterionValue . (
	/ fromIntegral maximumDefended	-- Normalise.
 ) . fromIntegral . uncurry (-) . (
	(! Property.Opposable.getOpposite {-recent mover-} nextLogicalColour) &&& (! nextLogicalColour)
 ) . State.Board.summariseNDefendersByLogicalColour $ Model.Game.getBoard game where
	nextLogicalColour	= Model.Game.getNextLogicalColour game

{- |
	* Evaluates the fitness of the /board/ from the perspective of the last player to move.
	If the game has ended, the fitness is maximum for checkmate or zero for a draw,
	but otherwise is the /weighted mean/ of various criteria; <https://chessprogramming.wikispaces.com/Evaluation>.

	* Also returns the break-down of those /criterion-value/s with a non-zero /criterion-weight/.

	* Besides measuring the difference between the total /rank-value/ on either side,
	other criteria are selected to represent known attributes of a good position,
	but which won't be pay dividends any time soon, & therefore won't be represented by 'measureValueOfMaterial' within the limited future predicted.

	* Many possible criteria aren't measured because they're, either currently or soon, represented by those that are, typically 'measureValueOfMaterial'.
-}
evaluateFitness :: (
	Enum		x,
	Enum		y,
	Fractional	criterionValue,
	Fractional	pieceSquareValue,
	Fractional	rankValue,
	Fractional	weightedMean,
	Ord		x,
	Ord		y,
	Real		criterionValue,
	Real		criterionWeight,
	Real		pieceSquareValue,
	Real		rankValue,
	Show		x,
	Show		y
 )
	=> Maybe pieceSquareValue	-- ^ An optional value for the specified game.
	-> Model.Game.Game x y
	-> Input.EvaluationOptions.Reader criterionWeight pieceSquareValue rankValue x y (
		Attribute.WeightedMeanAndCriterionValues.WeightedMeanAndCriterionValues weightedMean criterionValue
	)
{-# SPECIALISE evaluateFitness :: Maybe T.PieceSquareValue -> Model.Game.Game T.X T.Y -> Input.EvaluationOptions.Reader T.CriterionWeight T.PieceSquareValue T.RankValue T.X T.Y (Attribute.WeightedMeanAndCriterionValues.WeightedMeanAndCriterionValues T.WeightedMean T.CriterionValue) #-}
evaluateFitness maybePieceSquareValue game
	| Just gameTerminationReason <- Model.Game.getMaybeTerminationReason game	= return {-to Reader-monad-} $ Attribute.WeightedMeanAndCriterionValues.mkWeightedMeanAndCriterionValues (
		if Model.GameTerminationReason.isCheckMate gameTerminationReason
			then 1	-- The last player to move, has won.
			else 0	-- A draw.
	) []
	| otherwise	= do
		criteriaWeights		<- Control.Monad.Reader.asks Input.EvaluationOptions.getCriteriaWeights
		rankValues		<- Control.Monad.Reader.asks Input.EvaluationOptions.getRankValues
		maybePieceSquareArray	<- Control.Monad.Reader.asks Input.EvaluationOptions.getMaybePieceSquareArray

		return {-to Reader-monad-} $ Input.CriteriaWeights.calculateWeightedMean criteriaWeights (
			measureValueOfMaterial rankValues game
		 ) (
			measureValueOfMobility game
		 ) (
			Data.Maybe.maybe Attribute.CriterionValue.zero mkPieceSquareCriterionValue $ maybePieceSquareValue <|> fmap (
				`measurePieceSquareValue` game
			) maybePieceSquareArray
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

