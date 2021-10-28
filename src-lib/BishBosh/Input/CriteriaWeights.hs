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

 [@DESCRIPTION@]	Defines the weight associated with each criterion.
-}

module BishBosh.Input.CriteriaWeights(
-- * Types
-- ** Type-synonyms
--	Transformation,
-- ** Data-types
	CriteriaWeights(
--		MkCriteriaWeights,
		getWeightOfMaterial,
		getWeightOfMobility,
		getWeightOfPieceSquareValue,
		getWeightOfCastlingPotential,
		getWeightOfDefence,
		getWeightOfDoubledPawns,
		getWeightOfIsolatedPawns,
		getWeightOfPassedPawns
	),
-- * Constants
	tag,
	weightOfMaterialTag,
--	weightOfMobilityTag,
	weightOfPieceSquareValueTag,
--	weightOfCastlingPotentialTag,
--	weightOfDefenceTag,
--	weightOfDoubledPawnsTag,
--	weightOfIsolatedPawnsTag,
--	weightOfPassedPawnsTag,
-- * Functions
	calculateWeightedMean,
	normalise,
--	perturbWeights,
-- ** Constructor
	mkCriteriaWeights
) where

import qualified	BishBosh.Data.Exception				as Data.Exception
import qualified	BishBosh.Metric.CriterionValue			as Metric.CriterionValue
import qualified	BishBosh.Metric.CriterionWeight			as Metric.CriterionWeight
import qualified	BishBosh.Metric.WeightedMeanAndCriterionValues	as Metric.WeightedMeanAndCriterionValues
import qualified	BishBosh.Property.ShowFloat			as Property.ShowFloat
import qualified	BishBosh.Text.ShowList				as Text.ShowList
import qualified	BishBosh.Type.Mass				as Type.Mass
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Default
import qualified	System.Random
import qualified	Text.XML.HXT.Arrow.Pickle			as HXT

-- | Used to qualify the XML.
tag :: String
tag				= "criteriaWeights"

-- | Used to qualify the XML.
weightOfMaterialTag :: String
weightOfMaterialTag		= "material"

-- | Used to qualify the XML.
weightOfMobilityTag :: String
weightOfMobilityTag		= "mobility"

-- | Used to qualify the XML.
weightOfPieceSquareValueTag :: String
weightOfPieceSquareValueTag	= "pieceSquareValue"

-- | Used to qualify the XML.
weightOfCastlingPotentialTag :: String
weightOfCastlingPotentialTag	= "castlingPotential"

-- | Used to qualify the XML.
weightOfDefenceTag :: String
weightOfDefenceTag		= "defence"

-- | Used to qualify the XML.
weightOfDoubledPawnsTag :: String
weightOfDoubledPawnsTag		= "doubledPawns"

-- | Used to qualify the XML.
weightOfIsolatedPawnsTag :: String
weightOfIsolatedPawnsTag	= "isolatedPawns"

-- | Used to qualify the XML.
weightOfPassedPawnsTag :: String
weightOfPassedPawnsTag		= "passedPawns"

{- |
	* The weight of various criteria used to select a /move/ from alternatives, at a specific point in the game.

	* CAVEAT: these weights determine the effective value of /isolated/ or /doubled/ @Pawn@s,
	& this value shouldn't be less than their weighted normalised /rank-value/, otherwise sacrifice would be beneficial.
-}
data CriteriaWeights	= MkCriteriaWeights {
	getWeightOfMaterial		:: Metric.CriterionWeight.CriterionWeight,	-- ^ The arithmetic difference between the total /rank-value/ of the /piece/s currently in play on either side; <https://www.chessprogramming.org/Material>.
	getWeightOfMobility		:: Metric.CriterionWeight.CriterionWeight,	-- ^ The arithmetic difference between the reciprocal of the number of destinations available to the /piece/s of either side. N.B. this weight can be derived from 'getWeightOfMaterial' since losing a @Queen@ is less significant than reducing mobility to zero; <https://www.chessprogramming.org/Mobility>.
	getWeightOfPieceSquareValue	:: Metric.CriterionWeight.CriterionWeight,	-- ^ The arithmetic difference in the values of the squares occupied by the pieces of either side.
	getWeightOfCastlingPotential	:: Metric.CriterionWeight.CriterionWeight,	-- ^ Whether each player has been permanently prevented from /Castling/.
	getWeightOfDefence		:: Metric.CriterionWeight.CriterionWeight,	-- ^ The arithmetic difference between the number of /piece/s defending each of one's own, compared with the opponent.
	getWeightOfDoubledPawns		:: Metric.CriterionWeight.CriterionWeight,	-- ^ The arithmetic difference between the total number of /doubled/ @Pawn@s on either side; <https://www.chessprogramming.org/Doubled_Pawn>.
	getWeightOfIsolatedPawns	:: Metric.CriterionWeight.CriterionWeight,	-- ^ The arithmetic difference between the total number of /isolated/ @Pawn@s on either side; <https://www.chessprogramming.org/Isolated_Pawn>.
	getWeightOfPassedPawns		:: Metric.CriterionWeight.CriterionWeight	-- ^ The arithmetic difference between the total number of /passed/ @Pawn@s on either side; <https://www.chessprogramming.org/Passed_Pawn>.
} deriving (Eq, Show)

-- | Smart constructor.
mkCriteriaWeights
	:: Metric.CriterionWeight.CriterionWeight	-- ^ /material/.
	-> Metric.CriterionWeight.CriterionWeight	-- ^ /mobility/.
	-> Metric.CriterionWeight.CriterionWeight	-- ^ /pieceSquareValue/.
	-> Metric.CriterionWeight.CriterionWeight	-- ^ /castlingPotential/.
	-> Metric.CriterionWeight.CriterionWeight	-- ^ /defence/.
	-> Metric.CriterionWeight.CriterionWeight	-- ^ /doubledPawns/.
	-> Metric.CriterionWeight.CriterionWeight	-- ^ /isolatedPawns/.
	-> Metric.CriterionWeight.CriterionWeight	-- ^ /passedPawns/.
	-> CriteriaWeights
mkCriteriaWeights a b c d e f g h
	| criteriaWeights == minBound	= Control.Exception.throw $ Data.Exception.mkInvalidDatum "BishBosh.Input.CriteriaWeights.mkCriteriaWeights:\tall weights are zero."
	| otherwise			= criteriaWeights
	where
		criteriaWeights	= MkCriteriaWeights a b c d e f g h

instance Property.ShowFloat.ShowFloat CriteriaWeights where
	showsFloat fromDouble MkCriteriaWeights {
		getWeightOfMaterial		= weightOfMaterial,
		getWeightOfMobility		= weightOfMobility,
		getWeightOfPieceSquareValue	= weightOfPieceSquareValue,
		getWeightOfCastlingPotential	= weightOfCastlingPotential,
		getWeightOfDefence		= weightOfDefence,
		getWeightOfDoubledPawns		= weightOfDoubledPawns,
		getWeightOfIsolatedPawns	= weightOfIsolatedPawns,
		getWeightOfPassedPawns		= weightOfPassedPawns
	} = Text.ShowList.showsAssociationList' $ map (
		Control.Arrow.second $ fromDouble . realToFrac
	 ) [
		(
			weightOfMaterialTag,		weightOfMaterial
		), (
			weightOfMobilityTag,		weightOfMobility
		), (
			weightOfPieceSquareValueTag,	weightOfPieceSquareValue
		), (
			weightOfCastlingPotentialTag,	weightOfCastlingPotential
		), (
			weightOfDefenceTag,		weightOfDefence
		), (
			weightOfDoubledPawnsTag,	weightOfDoubledPawns
		), (
			weightOfIsolatedPawnsTag,	weightOfIsolatedPawns
		), (
			weightOfPassedPawnsTag,		weightOfPassedPawns
		)
	 ]

instance Data.Default.Default CriteriaWeights where
	def = MkCriteriaWeights {
		getWeightOfMaterial		= maxBound,
		getWeightOfMobility		= Data.Default.def,
		getWeightOfPieceSquareValue	= Data.Default.def,
		getWeightOfCastlingPotential	= Data.Default.def,
		getWeightOfDefence		= Data.Default.def,
		getWeightOfDoubledPawns		= Data.Default.def,
		getWeightOfIsolatedPawns	= Data.Default.def,
		getWeightOfPassedPawns		= Data.Default.def
	}

instance Control.DeepSeq.NFData CriteriaWeights where
	rnf (MkCriteriaWeights a b c d e f g h)	= Control.DeepSeq.rnf [a, b, c, d, e, f, g, h]

instance Bounded CriteriaWeights where
	maxBound	= MkCriteriaWeights maxBound maxBound maxBound maxBound maxBound maxBound maxBound maxBound
	minBound	= MkCriteriaWeights minBound minBound minBound minBound minBound minBound minBound minBound

instance HXT.XmlPickler CriteriaWeights where
	xpickle	= HXT.xpDefault Data.Default.def . HXT.xpElem tag $ HXT.xpWrap (
		\(a, b, c, d, e, f, g, h) -> mkCriteriaWeights a b c d e f g h,	-- Construct.
		\MkCriteriaWeights {
			getWeightOfMaterial		= weightOfMaterial,
			getWeightOfMobility		= weightOfMobility,
			getWeightOfPieceSquareValue	= weightOfPieceSquareValue,
			getWeightOfCastlingPotential	= weightOfCastlingPotential,
			getWeightOfDefence		= weightOfDefence,
			getWeightOfDoubledPawns		= weightOfDoubledPawns,
			getWeightOfIsolatedPawns	= weightOfIsolatedPawns,
			getWeightOfPassedPawns		= weightOfPassedPawns
		} -> (
			weightOfMaterial,
			weightOfMobility,
			weightOfPieceSquareValue,
			weightOfCastlingPotential,
			weightOfDefence,
			weightOfDoubledPawns,
			weightOfIsolatedPawns,
			weightOfPassedPawns
		) -- Deconstruct.
	 ) $ HXT.xp8Tuple (
		xpickle' weightOfMaterialTag
	 ) (
		xpickle' weightOfMobilityTag
	 ) (
		xpickle' weightOfPieceSquareValueTag
	 ) (
		xpickle' weightOfCastlingPotentialTag
	 ) (
		xpickle' weightOfDefenceTag
	 ) (
		xpickle' weightOfDoubledPawnsTag
	 ) (
		xpickle' weightOfIsolatedPawnsTag
	 ) (
		xpickle' weightOfPassedPawnsTag
	 ) where
		xpickle'	= HXT.xpDefault Data.Default.def . (`HXT.xpAttr` HXT.xpickle)

{- |
	* Returns the weighted sum of the specified criteria, divided by the sum of the weights; <https://www.chessprogramming.org/evaluation>

	* Each criterion increases in proportion to some desirable attribute of the proposed /game/.

	* Each criterion should be in the same range of magnitudes, so that none dominates the total, thus making the total a clear measure of the value attributed to each.

	* Also writes individual unweighted /criterion-value/s, to facilitate post-analysis; if the corresponding weight is zero, for efficiency evaluation of the criterion is avoided.
-}
calculateWeightedMean
	:: CriteriaWeights
	-> Metric.CriterionValue.CriterionValue					-- ^ /material/:	maximum if a player's /move/ equals the maximum total piece value (including /queened/ @Pawn@s), while their opponent has just a @King@.
	-> Metric.CriterionValue.CriterionValue					-- ^ /mobility/:	maximum when the opponent is check-mated.
	-> Metric.CriterionValue.CriterionValue					-- ^ /pieceSquareValue/:	maximum when this player occupies all the strategically important squares & the opponent none.
	-> Metric.CriterionValue.CriterionValue					-- ^ /castlingPotential/:	maximum when this player either has /castled/ or can, but the opponent has been permanently prevented.
	-> Metric.CriterionValue.CriterionValue					-- ^ /defence/:	maximum when this player's /piece/s are fully utilised in defence, but none of the opponent's are.
	-> Metric.CriterionValue.CriterionValue					-- ^ /doubledPawns/:	maximum when this player hasn't any /doubled/ @Pawn@s & the opponent has two files of four @Pawn@s.
	-> Metric.CriterionValue.CriterionValue					-- ^ /isolatedPawns/:	maximum when this player hasn't any /isolated/ @Pawn@s & all the opponent's @Pawn@s are /isolated/.
	-> Metric.CriterionValue.CriterionValue					-- ^ /passedPawns/:	maximum when this player has 8 /passed/ @Pawn@s & the opponent has none.
	-> Metric.WeightedMeanAndCriterionValues.WeightedMeanAndCriterionValues	-- ^ The individual /criteria/ values, & their /weighted mean/.
calculateWeightedMean MkCriteriaWeights {
	getWeightOfMaterial		= weightOfMaterial,
	getWeightOfMobility		= weightOfMobility,
	getWeightOfPieceSquareValue	= weightOfPieceSquareValue,
	getWeightOfCastlingPotential	= weightOfCastlingPotential,
	getWeightOfDefence		= weightOfDefence,
	getWeightOfDoubledPawns		= weightOfDoubledPawns,
	getWeightOfIsolatedPawns	= weightOfIsolatedPawns,
	getWeightOfPassedPawns		= weightOfPassedPawns
} material mobility pieceSquareValue castlingPotential defence doubledPawns isolatedPawns passedPawns	= Metric.WeightedMeanAndCriterionValues.calculateWeightedMean [
	(
		material,		weightOfMaterial
	), (
		mobility,		weightOfMobility
	), (
		pieceSquareValue,	weightOfPieceSquareValue
	), (
		castlingPotential,	weightOfCastlingPotential
	), (
		defence,		weightOfDefence
	), (
		doubledPawns,		weightOfDoubledPawns
	), (
		isolatedPawns,		weightOfIsolatedPawns
	), (
		passedPawns,		weightOfPassedPawns
	)
 ]

-- | The type of a function which mutates 'CriteriaWeights'.
type Transformation	= CriteriaWeights -> CriteriaWeights

-- | Adjust the mean weight, so that the maximum weight is @1@.
normalise :: Transformation
normalise criteriaWeights@MkCriteriaWeights {
	getWeightOfMaterial		= weightOfMaterial,
	getWeightOfMobility		= weightOfMobility,
	getWeightOfPieceSquareValue	= weightOfPieceSquareValue,
	getWeightOfCastlingPotential	= weightOfCastlingPotential,
	getWeightOfDefence		= weightOfDefence,
	getWeightOfDoubledPawns		= weightOfDoubledPawns,
	getWeightOfIsolatedPawns	= weightOfIsolatedPawns,
	getWeightOfPassedPawns		= weightOfPassedPawns
} = Control.Exception.assert (
	criteriaWeights /= minBound	-- Guard against divide-by-zero.
 ) MkCriteriaWeights {
	getWeightOfMaterial		= normaliseCriterionWeight weightOfMaterial,
	getWeightOfMobility		= normaliseCriterionWeight weightOfMobility,
	getWeightOfPieceSquareValue	= normaliseCriterionWeight weightOfPieceSquareValue,
	getWeightOfCastlingPotential	= normaliseCriterionWeight weightOfCastlingPotential,
	getWeightOfDefence		= normaliseCriterionWeight weightOfDefence,
	getWeightOfDoubledPawns		= normaliseCriterionWeight weightOfDoubledPawns,
	getWeightOfIsolatedPawns	= normaliseCriterionWeight weightOfIsolatedPawns,
	getWeightOfPassedPawns		= normaliseCriterionWeight weightOfPassedPawns
 } where
	normaliseCriterionWeight	= (
		/ maximum [
			weightOfMaterial,
			weightOfMobility,
			weightOfPieceSquareValue,
			weightOfCastlingPotential,
			weightOfDefence,
			weightOfDoubledPawns,
			weightOfIsolatedPawns,
			weightOfPassedPawns
		]
	 ) -- Section.

{- |
	* Independently perturbs each /criterion-weight/ by a random value, of configurable magnitude.

	* Under this transformation, /criterion-weight/s of @0@, will remain unchanged, thus those criteria deemed irrelevant remain irrelevant.

	* The identity transform results from specification of a change-magnitude of zero.
-}
perturbWeights
	:: System.Random.RandomGen randomGen
	=> randomGen
	-> Type.Mass.CriterionWeight	-- ^ Positive change-magnitude; unbounded to permit values greater than 1.
	-> Transformation
perturbWeights _ 0 criteriaWeights	= criteriaWeights	-- The indentity transform.
perturbWeights randomGen changeMagnitude MkCriteriaWeights {
	getWeightOfMaterial		= weightOfMaterial,
	getWeightOfMobility		= weightOfMobility,
	getWeightOfPieceSquareValue	= weightOfPieceSquareValue,
	getWeightOfCastlingPotential	= weightOfCastlingPotential,
	getWeightOfDefence		= weightOfDefence,
	getWeightOfDoubledPawns		= weightOfDoubledPawns,
	getWeightOfIsolatedPawns	= weightOfIsolatedPawns,
	getWeightOfPassedPawns		= weightOfPassedPawns
}
	| changeMagnitude < 0	= Control.Exception.throw $ Data.Exception.mkInvalidDatum "BishBosh.Input.CriteriaWeights.perturbWeights:\tchange-magnitude can't be negative."
	| otherwise		= normalise MkCriteriaWeights {
		getWeightOfMaterial		= a weightOfMaterial,
		getWeightOfMobility		= b weightOfMobility,
		getWeightOfPieceSquareValue	= c weightOfPieceSquareValue,
		getWeightOfCastlingPotential	= d weightOfCastlingPotential,
		getWeightOfDefence		= e weightOfDefence,
		getWeightOfDoubledPawns		= f weightOfDoubledPawns,
		getWeightOfIsolatedPawns	= g weightOfIsolatedPawns,
		getWeightOfPassedPawns		= h weightOfPassedPawns
	}
	where
		(a : b : c : d : e : f : g : h : _)	= map (
			\r -> realToFrac . (/ r) . realToFrac	-- N.B. this always reduces the weight, & therefore can't breach Metric.CriterionWeight.CriterionWeight's permissible bounds.
		 ) $ System.Random.randomRs (1, succ changeMagnitude) randomGen

