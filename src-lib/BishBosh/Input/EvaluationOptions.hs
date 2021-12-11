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

	* Defines configurable options related to the evaluation of the game at any instance.

	* N.B.: /evaluation/ is distinct from /search/:
		evaluation => how one assesses the fitness of candidate moves;
		search => the order in which one evaluates candidates before selecting on the basis of their fitness.
-}

module BishBosh.Input.EvaluationOptions(
-- * Types
-- ** Type-synonyms
	IncrementalEvaluation,
--	PieceSquareTablePair,
	Reader,
-- ** Data-types
	EvaluationOptions(
--		MkEvaluationOptions,
		getRankValues,
		getMaximumTotalRankValue,
		getCriteriaWeights,
		getIncrementalEvaluation,
--		getMaybePieceSquareTablePair,
		getMaybePieceSquareByCoordinatesByRank
	),
-- * Constants
	tag,
--	incrementalEvaluationTag,
--	pieceSquareTablesTag,
--	pieceSquareTableEndGameTag,
	nPiecesBounds,
-- * Functions
--	interpolatePieceSquareValues,
--	fromPieceSquareTablePair,
-- ** Constructor
	mkEvaluationOptions
) where

import			BishBosh.Data.Bool()
import			Control.Arrow((&&&), (***))
import			Data.Array.IArray((!))
import qualified	BishBosh.Cartesian.Coordinates				as Cartesian.Coordinates
import qualified	BishBosh.Component.PieceSquareByCoordinatesByRank	as Component.PieceSquareByCoordinatesByRank
import qualified	BishBosh.Colour.LogicalColour				as Colour.LogicalColour
import qualified	BishBosh.Component.Piece				as Component.Piece
import qualified	BishBosh.Data.Exception					as Data.Exception
import qualified	BishBosh.Input.CriteriaWeights				as Input.CriteriaWeights
import qualified	BishBosh.Input.PieceSquareTable				as Input.PieceSquareTable
import qualified	BishBosh.Input.RankValues				as Input.RankValues
import qualified	BishBosh.Property.FixedMembership			as Property.FixedMembership
import qualified	BishBosh.Property.ShowFloat				as Property.ShowFloat
import qualified	BishBosh.Text.ShowList					as Text.ShowList
import qualified	BishBosh.Type.Count					as Type.Count
import qualified	BishBosh.Type.Mass					as Type.Mass
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Control.Monad.Reader
import qualified	Data.Array.IArray
import qualified	Data.Default
import qualified	Data.Foldable
import qualified	Data.Maybe
import qualified	Text.XML.HXT.Arrow.Pickle				as HXT

-- | Used to qualify XML.
tag :: String
tag	= "evaluationOptions"

-- | Used to qualify XML.
incrementalEvaluationTag :: String
incrementalEvaluationTag	= "incrementalEvaluation"

-- | Used to qualify XML.
pieceSquareTablesTag :: String
pieceSquareTablesTag	= showString Input.PieceSquareTable.tag "s"

-- | Used to qualify XML.
pieceSquareTableEndGameTag :: String
pieceSquareTableEndGameTag	= showString Input.PieceSquareTable.tag "EndGame"

-- | Whether to generate position-hashes incrementally from the hash of the position prior to the last move.
type IncrementalEvaluation	= Bool

-- | A pair of piece-square tables representing the opening & end-games respectively.
type PieceSquareTablePair	= (Input.PieceSquareTable.PieceSquareTable, Input.PieceSquareTable.PieceSquareTable)

-- | Defines the options related to the automatic selection of /move/s.
data EvaluationOptions	= MkEvaluationOptions {
	getRankValues				:: Input.RankValues.RankValues,								-- ^ The static value associated with each /piece/'s /rank/.
	getMaximumTotalRankValue		:: Type.Mass.RankValue,									-- ^ Used to normalise the total value of pieces. Derived from 'getRankValues'.
	getCriteriaWeights			:: Input.CriteriaWeights.CriteriaWeights,						-- ^ The weights applied to each of the heterogeneous criterion-values used to select a /move/.
	getIncrementalEvaluation		:: IncrementalEvaluation,								-- ^ Whether to generate position-hashes & evaluate the piece-square value, from the previous value or from scratch.
	getMaybePieceSquareTablePair		:: Maybe PieceSquareTablePair,								-- ^ A optional pair of piece-square tables representing the opening & end-games respectively.
	getMaybePieceSquareByCoordinatesByRank	:: Maybe Component.PieceSquareByCoordinatesByRank.PieceSquareByCoordinatesByRank	-- ^ The optional value for each rank of /piece/, when occupying each coordinate, at each phase of the game.
} deriving (Eq, Show)

instance Control.DeepSeq.NFData EvaluationOptions where
	rnf MkEvaluationOptions {
--		getRankValues				= rankValues,
		getMaximumTotalRankValue		= maximumTotalValue,
		getCriteriaWeights			= criteriaWeights,
		getIncrementalEvaluation		= incrementalEvaluation,
--		getMaybePieceSquareTablePair		= maybePieceSquareTablePair,
		getMaybePieceSquareByCoordinatesByRank	= maybePieceSquareByCoordinatesByRank
	} = Control.DeepSeq.rnf (maximumTotalValue, criteriaWeights, incrementalEvaluation, maybePieceSquareByCoordinatesByRank)

instance Property.ShowFloat.ShowFloat EvaluationOptions where
	showsFloat fromDouble MkEvaluationOptions {
		getRankValues				= rankValues,
--		getMaximumTotalRankValue		= maximumTotalValue,
		getCriteriaWeights			= criteriaWeights,
		getIncrementalEvaluation		= incrementalEvaluation,
		getMaybePieceSquareTablePair		= maybePieceSquareTablePair
--		getMaybePieceSquareByCoordinatesByRank	= maybePieceSquareByCoordinatesByRank
	} = Text.ShowList.showsAssociationList' $ [
		(
			Input.RankValues.tag,		Property.ShowFloat.showsFloat fromDouble rankValues
		), (
			incrementalEvaluationTag,	shows incrementalEvaluation
		), (
			Input.CriteriaWeights.tag,	Property.ShowFloat.showsFloat fromDouble criteriaWeights
		)
	 ] ++ Data.Maybe.maybe [] (
		\(t, t')	-> [
			(
				Input.PieceSquareTable.tag,
				Property.ShowFloat.showsFloat fromDouble t
			), (
				pieceSquareTableEndGameTag,
				Property.ShowFloat.showsFloat fromDouble t'
			)
		]
	 ) maybePieceSquareTablePair

instance Data.Default.Default EvaluationOptions where
	def = MkEvaluationOptions {
		getRankValues				= rankValues,
		getMaximumTotalRankValue		= Input.RankValues.calculateMaximumTotalValue rankValues,
		getCriteriaWeights			= Data.Default.def,
		getIncrementalEvaluation		= True,
		getMaybePieceSquareTablePair		= Nothing,
		getMaybePieceSquareByCoordinatesByRank	= Nothing
	} where
		rankValues	= Data.Default.def

instance HXT.XmlPickler EvaluationOptions where
	xpickle	= HXT.xpDefault def . HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d) -> mkEvaluationOptions a b c d,	-- Construct.
		\MkEvaluationOptions {
			getRankValues				= rankValues,
--			getMaximumTotalRankValue		= maximumTotalRankValue,
			getCriteriaWeights			= criteriaWeights,
			getIncrementalEvaluation		= incrementalEvaluation,
			getMaybePieceSquareTablePair		= maybePieceSquareTablePair
--			getMaybePieceSquareByCoordinatesByRank	= maybePieceSquareByCoordinatesByRank
		} -> (
			rankValues,
			criteriaWeights,
			incrementalEvaluation,
			maybePieceSquareTablePair
		) -- Deconstruct.
	 ) . HXT.xp4Tuple HXT.xpickle {-RankValues-} HXT.xpickle {-CriteriaWeights-} (
		getIncrementalEvaluation def `HXT.xpDefault` HXT.xpAttr incrementalEvaluationTag HXT.xpickle {-Bool-}
	 ) . HXT.xpOption . HXT.xpElem pieceSquareTablesTag $ HXT.xpElem Input.PieceSquareTable.tag HXT.xpickle `HXT.xpPair` HXT.xpElem pieceSquareTableEndGameTag HXT.xpickle where
		def	= Data.Default.def

-- | The constant bounds of the number of pieces on the board, at the end-game & opening-game respectively.
nPiecesBounds :: (Type.Count.NPieces, Type.Count.NPieces)
nPiecesBounds	= (
	3 {-minimum sufficient material-},
	fromIntegral Colour.LogicalColour.nDistinctLogicalColours * Component.Piece.nPiecesPerSide
 )

-- | Given the bounds over which a piece-square value varies, as the game progresses from opening to end, return the linearly interpolated value for the specified stage.
interpolatePieceSquareValues
	:: Type.Count.NPieces						-- ^ The value to interpolate.
	-> (Type.Mass.PieceSquareValue, Type.Mass.PieceSquareValue)	-- ^ (Opening-game, End-game) values.
	-> Type.Mass.PieceSquareValue
interpolatePieceSquareValues nPieces (openingGame, endGame)	= realToFrac . uncurry (+) . (
	(* toRational openingGame) &&& (* toRational endGame) . (1 -)	-- N.B.: arithmetic must be conducted in an unbounded type, instead of 'PieceSquareValue'.
 ) $ fromIntegral (
	nPieces - fst {-minimum-} nPiecesBounds
 ) / fromIntegral (
	uncurry subtract nPiecesBounds	-- N.B.: this can't reasonably be zero.
 )

-- | Convert a /PieceSquareTablePair/ to a single linearly interpolated array.
fromPieceSquareTablePair :: PieceSquareTablePair -> Component.PieceSquareByCoordinatesByRank.PieceSquareByCoordinatesByRank
fromPieceSquareTablePair pieceSquareTablePair	= Component.PieceSquareByCoordinatesByRank.mkPieceSquareByCoordinatesByRank $ \rank -> (
	\(openingGamePieceSquareValueByCoordinates, maybeEndGamePieceSquareValueByCoordinates) -> Data.Maybe.maybe (
		Left openingGamePieceSquareValueByCoordinates	-- There's only one value for this rank, so no interpolation is required.
	) (
		\endGamePieceSquareValueByCoordinates -> Right . Data.Array.IArray.listArray nPiecesBounds . map (
			\nPieces -> Cartesian.Coordinates.listArrayByCoordinates $ map (
				interpolatePieceSquareValues nPieces . (
					(openingGamePieceSquareValueByCoordinates !) &&& (endGamePieceSquareValueByCoordinates !)
				)
			) Property.FixedMembership.members	-- Coordinates.
		) $ uncurry enumFromTo nPiecesBounds
	) maybeEndGamePieceSquareValueByCoordinates
 ) $ (
	uncurry (***) . (
		(.) Data.Maybe.fromJust {-all ranks must be defined for the openingGame-} &&& id
	) $ Input.PieceSquareTable.dereference rank
 ) pieceSquareTablePair

-- | Smart constructor.
mkEvaluationOptions
	:: Input.RankValues.RankValues			-- ^ The static value associated with each /piece/'s /rank/.
	-> Input.CriteriaWeights.CriteriaWeights	-- ^ The weights applied to the values of the criteria used to select a /move/.
	-> IncrementalEvaluation
	-> Maybe PieceSquareTablePair			-- ^ The value to each type of piece, of each square, during normal play & the end-game.
	-> EvaluationOptions
mkEvaluationOptions rankValues criteriaWeights incrementalEvaluation maybePieceSquareTablePair
	| Just (pieceSquareTable, _)	<- maybePieceSquareTablePair
	, let undefinedRanks	= Input.PieceSquareTable.findUndefinedRanks pieceSquareTable
	, not $ Data.Foldable.null undefinedRanks
	= Control.Exception.throw . Data.Exception.mkInsufficientData . showString "BishBosh.Input.EvaluationOptions.mkEvaluationOptions:\tranks" . Text.ShowList.showsAssociation $ shows (Data.Foldable.toList undefinedRanks) " are undefined."
	| Input.CriteriaWeights.getWeightOfPieceSquareValue criteriaWeights /= minBound
	, Data.Maybe.isNothing maybePieceSquareTablePair
	= Control.Exception.throw . Data.Exception.mkIncompatibleData . showString "BishBosh.Input.EvaluationOptions.mkEvaluationOptions:\tweight of " . shows Input.CriteriaWeights.weightOfPieceSquareValueTag . showString " is defined but " $ shows Input.PieceSquareTable.tag " isn't."
	| maximumTotalRankValue == 0 && Input.CriteriaWeights.getWeightOfMaterial criteriaWeights /= 0	= Control.Exception.throw . Data.Exception.mkIncompatibleData . showString "BishBosh.Input.EvaluationOptions.mkEvaluationOptions:\tweight of " . shows Input.CriteriaWeights.weightOfMaterialTag . showString " is non-zero, but the maximum total rank-value is " $ shows rankValues "."
	| otherwise		= MkEvaluationOptions {
		getRankValues				= rankValues,
		getMaximumTotalRankValue		= maximumTotalRankValue,
		getCriteriaWeights			= criteriaWeights,
		getIncrementalEvaluation		= incrementalEvaluation,
		getMaybePieceSquareTablePair		= maybePieceSquareTablePair,
		getMaybePieceSquareByCoordinatesByRank	= fromPieceSquareTablePair <$> maybePieceSquareTablePair	-- Infer.
	} where
		maximumTotalRankValue	= Input.RankValues.calculateMaximumTotalValue rankValues

-- | Self-documentation.
type Reader	= Control.Monad.Reader.Reader EvaluationOptions

