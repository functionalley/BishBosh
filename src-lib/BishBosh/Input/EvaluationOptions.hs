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

	* N.B.: 'evaluation' is distinct from 'search':
		evaluation => how one assesses the fitness of candidate moves;
		search => the order in which one evaluates candidates before selecting on the basis of their fitness.
-}

module BishBosh.Input.EvaluationOptions(
-- * Types
-- ** Type-synonyms
	IncrementalEvaluation,
	Reader,
-- ** Data-types
	EvaluationOptions(
--		MkEvaluationOptions,
		getRankValues,
		getCriteriaWeights,
		getIncrementalEvaluation,
--		getMaybePieceSquareTables,
		getMaybePieceSquareArray
	),
-- * Constants
	tag,
--	incrementalEvaluationTag,
--	pieceSquareTablesTag,
--	pieceSquareTableEndGameTag,
-- * Functions
-- ** Constructor
	mkEvaluationOptions
) where

import			BishBosh.Data.Bool()
import			Control.Arrow((&&&), (***))
import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Attribute.RankValues		as Attribute.RankValues
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Component.Piece		as Component.Piece
import qualified	BishBosh.Component.PieceSquareArray	as Component.PieceSquareArray
import qualified	BishBosh.Data.Exception			as Data.Exception
import qualified	BishBosh.Input.CriteriaWeights		as Input.CriteriaWeights
import qualified	BishBosh.Input.PieceSquareTable		as Input.PieceSquareTable
import qualified	BishBosh.Property.ShowFloat		as Property.ShowFloat
import qualified	BishBosh.Text.ShowList			as Text.ShowList
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Control.Monad.Reader
import qualified	Data.Array.IArray
import qualified	Data.Default
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT

-- | Used to qualify XML.
tag :: String
tag				= "evaluationOptions"

-- | Used to qualify XML.
incrementalEvaluationTag :: String
incrementalEvaluationTag	= "incrementalEvaluation"

-- | Used to qualify XML.
pieceSquareTablesTag :: String
pieceSquareTablesTag		= showString Input.PieceSquareTable.tag "s"

-- | Used to qualify XML.
pieceSquareTableEndGameTag :: String
pieceSquareTableEndGameTag	= showString Input.PieceSquareTable.tag "EndGame"

-- | Whether to generate position-hashes incrementally from the hash of the position prior to the last move.
type IncrementalEvaluation	= Bool

-- | Defines the options related to the automatic selection of /move/s.
data EvaluationOptions criterionWeight pieceSquareValue rankValue x y	= MkEvaluationOptions {
	getRankValues			:: Attribute.RankValues.RankValues rankValue,			-- ^ The static value associated with each /piece/'s /rank/.
	getCriteriaWeights		:: Input.CriteriaWeights.CriteriaWeights criterionWeight,	-- ^ The weights applied to each of the heterogeneous criterion-values used to select a /move/.
	getIncrementalEvaluation	:: IncrementalEvaluation,					-- ^ Whether to generate position-hashes & evaluate the piece-square value, from the previous value or from scratch.
	getMaybePieceSquareTables	:: Maybe (
		Input.PieceSquareTable.PieceSquareTable x y pieceSquareValue,
		Input.PieceSquareTable.PieceSquareTable x y pieceSquareValue
	),												-- ^ Optional piece-square tables; the first governs normal play & the second governs the end-game.
	getMaybePieceSquareArray	:: Maybe (
		Component.PieceSquareArray.PieceSquareArray x y pieceSquareValue
	)												-- ^ The optional value for each type of /piece/ of occupying each coordinate, at each stage in the lifetime of the game.
} deriving (Eq, Show)

instance (
	Control.DeepSeq.NFData	criterionWeight,
	Control.DeepSeq.NFData	pieceSquareValue,
	Control.DeepSeq.NFData	rankValue,
	Control.DeepSeq.NFData	x,
	Control.DeepSeq.NFData	y
 ) => Control.DeepSeq.NFData (EvaluationOptions criterionWeight pieceSquareValue rankValue x y) where
	rnf MkEvaluationOptions {
		getRankValues			= rankValues,
		getCriteriaWeights		= criteriaWeights,
		getIncrementalEvaluation	= incrementalEvaluation,
--		getMaybePieceSquareTables	= maybePieceSquareTables,
		getMaybePieceSquareArray	= maybePieceSquareArray
	} = Control.DeepSeq.rnf (rankValues, criteriaWeights, incrementalEvaluation, maybePieceSquareArray)

instance (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Real	criterionWeight,
	Real	pieceSquareValue,
	Real	rankValue,
	Show	pieceSquareValue
 ) => Property.ShowFloat.ShowFloat (EvaluationOptions criterionWeight pieceSquareValue rankValue x y) where
	showsFloat fromDouble MkEvaluationOptions {
		getRankValues			= rankValues,
		getCriteriaWeights		= criteriaWeights,
		getIncrementalEvaluation	= incrementalEvaluation,
		getMaybePieceSquareTables	= maybePieceSquareTables
--		getMaybePieceSquareArray	= maybePieceSquareArray
	} = Text.ShowList.showsAssociationList' $ [
		(
			Attribute.RankValues.tag,	Property.ShowFloat.showsFloat fromDouble rankValues
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
	 ) maybePieceSquareTables

instance (
	Fractional	rankValue,
	Num		criterionWeight,
	Ord		rankValue,
	Show		rankValue
 ) => Data.Default.Default (EvaluationOptions criterionWeight pieceSquareValue rankValue x y) where
	def = MkEvaluationOptions {
		getRankValues			= Data.Default.def,
		getCriteriaWeights		= Data.Default.def,
		getIncrementalEvaluation	= True,
		getMaybePieceSquareTables	= Nothing,
		getMaybePieceSquareArray	= Nothing
	}

instance (
	Enum		x,
	Enum		y,
	Fractional	pieceSquareValue,
	Fractional	rankValue,
	HXT.XmlPickler	criterionWeight,
	HXT.XmlPickler	rankValue,
	Num		criterionWeight,
	Ord		criterionWeight,
	Ord		pieceSquareValue,
	Ord		rankValue,
	Ord		x,
	Ord		y,
	Real		pieceSquareValue,
	Show		pieceSquareValue,
	Show		criterionWeight,
	Show		rankValue
 ) => HXT.XmlPickler (EvaluationOptions criterionWeight pieceSquareValue rankValue x y) where
	xpickle	= HXT.xpDefault def . HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d) -> mkEvaluationOptions a b c d,	-- Construct.
		\MkEvaluationOptions {
			getRankValues			= rankValues,
			getCriteriaWeights		= criteriaWeights,
			getIncrementalEvaluation	= incrementalEvaluation,
			getMaybePieceSquareTables	= maybePieceSquareTables
--			getMaybePieceSquareArray	= maybePieceSquareArray
		} -> (
			rankValues,
			criteriaWeights,
			incrementalEvaluation,
			maybePieceSquareTables
		) -- Deconstruct.
	 ) . HXT.xp4Tuple HXT.xpickle {-RankValues-} HXT.xpickle {-CriteriaWeights-} (
		getIncrementalEvaluation def `HXT.xpDefault` HXT.xpAttr incrementalEvaluationTag HXT.xpickle {-Bool-}
	 ) . HXT.xpOption . HXT.xpElem pieceSquareTablesTag $ HXT.xpElem Input.PieceSquareTable.tag HXT.xpickle `HXT.xpPair` HXT.xpElem pieceSquareTableEndGameTag HXT.xpickle where
		def	= Data.Default.def

-- | Smart constructor.
mkEvaluationOptions :: (
	Enum		x,
	Enum		y,
	Eq		pieceSquareValue,
	Eq		criterionWeight,
	Fractional	pieceSquareValue,
	Num		criterionWeight,
	Ord		x,
	Ord		y
 )
	=> Attribute.RankValues.RankValues rankValue												-- ^ The static value associated with each /piece/'s /rank/.
	-> Input.CriteriaWeights.CriteriaWeights criterionWeight										-- ^ The weights applied to the values of the criteria used to select a /move/.
	-> IncrementalEvaluation
	-> Maybe (Input.PieceSquareTable.PieceSquareTable x y pieceSquareValue, Input.PieceSquareTable.PieceSquareTable x y pieceSquareValue)	-- ^ The value to each type of piece, of each square, during normal play & the end-game.
	-> EvaluationOptions criterionWeight pieceSquareValue rankValue x y
mkEvaluationOptions rankValues criteriaWeights incrementalEvaluation maybePieceSquareTables
	| Just (pieceSquareTable, _)	<- maybePieceSquareTables
	, let undefinedRanks	= Input.PieceSquareTable.findUndefinedRanks pieceSquareTable
	, not $ Data.Set.null undefinedRanks
	= Control.Exception.throw . Data.Exception.mkInsufficientData . showString "BishBosh.Input.EvaluationOptions.mkEvaluationOptions:\tranks" . Text.ShowList.showsAssociation $ shows (Data.Set.toList undefinedRanks) " are undefined."
	| Input.CriteriaWeights.getWeightOfPieceSquareValue criteriaWeights /= minBound
	, Data.Maybe.isNothing maybePieceSquareTables
	= Control.Exception.throw . Data.Exception.mkIncompatibleData . showString "BishBosh.Input.EvaluationOptions.mkEvaluationOptions:\tweight of " . shows Input.CriteriaWeights.weightOfPieceSquareValueTag . showString " is defined but " $ shows Input.PieceSquareTable.tag " isn't."
	| otherwise		= MkEvaluationOptions {
		getRankValues			= rankValues,
		getCriteriaWeights		= criteriaWeights,
		getIncrementalEvaluation	= incrementalEvaluation,
		getMaybePieceSquareTables	= maybePieceSquareTables,
		getMaybePieceSquareArray	= (
			\pieceSquareTablePair -> Component.PieceSquareArray.mkPieceSquareArray (
				\rank -> Cartesian.Coordinates.listArrayByCoordinates . (
					\(normal, maybeEndGame) -> Data.Maybe.maybe (
						map Left normal
					) (
						zipWith interpolatePieceSquareValues normal
					) maybeEndGame
				) $ (
					 Data.Maybe.fromJust . Input.PieceSquareTable.dereference rank *** Input.PieceSquareTable.dereference rank
				) pieceSquareTablePair
			)
		) `fmap` maybePieceSquareTables
	} where
		nPiecesBounds@(minNPieces, maxNPieces)	= (3 {-minimum sufficient material-}, Attribute.LogicalColour.nDistinctLogicalColours * Component.Piece.nPiecesPerSide)

		interpolatePieceSquareValues :: (
			Eq		pieceSquareValue,
			Fractional	pieceSquareValue
		 ) => pieceSquareValue -> pieceSquareValue -> Component.PieceSquareArray.InterpolatedPieceSquareValues pieceSquareValue
		interpolatePieceSquareValues normal endGame
			| endGame /= normal	= Right . Data.Array.IArray.listArray nPiecesBounds . map (
				uncurry (+) . (
					(* normal) &&& (* endGame) . (1 -)
				) . (
					/ fromIntegral (
						maxNPieces - minNPieces
					) -- Normalise into the closed unit-interval [0,1].
				) . fromIntegral . subtract minNPieces
			) $ uncurry enumFromTo nPiecesBounds
			| otherwise		= Left normal	-- Interpolation is unnecessary.

-- | Self-documentation.
type Reader criterionWeight pieceSquareValue rankValue x y	= Control.Monad.Reader.Reader (EvaluationOptions criterionWeight pieceSquareValue rankValue x y)

