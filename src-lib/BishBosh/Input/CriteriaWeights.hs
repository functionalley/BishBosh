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
	onymousOperators,
-- * Functions
	calculateWeightedMean,
	normalise,
	perturbWeights,
-- ** Constructor
	mkCriteriaWeights
) where

import qualified	BishBosh.Attribute.CriterionValue			as Attribute.CriterionValue
import qualified	BishBosh.Attribute.CriterionWeight			as Attribute.CriterionWeight
import qualified	BishBosh.Attribute.WeightedMeanAndCriterionValues	as Attribute.WeightedMeanAndCriterionValues
import qualified	BishBosh.Data.Exception					as Data.Exception
import qualified	BishBosh.Property.ShowFloat				as Property.ShowFloat
import qualified	BishBosh.Text.ShowList					as Text.ShowList
import qualified	BishBosh.Types						as T
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Default
import qualified	System.Random
import qualified	Text.XML.HXT.Arrow.Pickle				as HXT

-- | Used to qualify the XML.
tag :: String
tag					= "criteriaWeights"

-- | Used to qualify the XML.
weightOfMaterialTag :: String
weightOfMaterialTag			= "material"

-- | Used to qualify the XML.
weightOfMobilityTag :: String
weightOfMobilityTag			= "mobility"

-- | Used to qualify the XML.
weightOfPieceSquareValueTag :: String
weightOfPieceSquareValueTag		= "pieceSquareValue"

-- | Used to qualify the XML.
weightOfCastlingPotentialTag :: String
weightOfCastlingPotentialTag		= "castlingPotential"

-- | Used to qualify the XML.
weightOfDefenceTag :: String
weightOfDefenceTag			= "defence"

-- | Used to qualify the XML.
weightOfDoubledPawnsTag :: String
weightOfDoubledPawnsTag			= "doubledPawns"

-- | Used to qualify the XML.
weightOfIsolatedPawnsTag :: String
weightOfIsolatedPawnsTag		= "isolatedPawns"

-- | Used to qualify the XML.
weightOfPassedPawnsTag :: String
weightOfPassedPawnsTag			= "passedPawns"

{- |
	* The weight of various criteria used to select a move from alternatives, at specific point in the game.

	* CAVEAT: these weights determine the effective value of /isolated/ or /doubled/ @Pawn@s,
	& this value shouldn't be less than their weighted normalised /rank-value/, otherwise sacrifice would be beneficial.
-}
data CriteriaWeights criterionWeight	= MkCriteriaWeights {
	getWeightOfMaterial		:: Attribute.CriterionWeight.CriterionWeight criterionWeight,	-- ^ The arithmetic difference between the total /rank-value/ of the /piece/s currently in play on either side; <https://chessprogramming.wikispaces.com/Material>.
	getWeightOfMobility		:: Attribute.CriterionWeight.CriterionWeight criterionWeight,	-- ^ The arithmetic difference between the reciprocal of the number of destinations available to the /piece/s of either side. N.B. this weight can be derived from 'getWeightOfMaterial' since losing a @Queen@ is less significant than reducing mobility to zero; <https://chessprogramming.wikispaces.com/Mobility>.
	getWeightOfPieceSquareValue	:: Attribute.CriterionWeight.CriterionWeight criterionWeight,	-- ^ The arithmetic difference in the values of the squares occupied by the pieces of either side.
	getWeightOfCastlingPotential	:: Attribute.CriterionWeight.CriterionWeight criterionWeight,	-- ^ Whether each player has been permanently prevented from /Castling/.
	getWeightOfDefence		:: Attribute.CriterionWeight.CriterionWeight criterionWeight,	-- ^ The arithmetic difference between the number of /piece/s defending each of one's own, compared with the opponent.
	getWeightOfDoubledPawns		:: Attribute.CriterionWeight.CriterionWeight criterionWeight,	-- ^ The arithmetic difference between the total number of /doubled/ @Pawn@s on either side; <https://chessprogramming.wikispaces.com/Doubled+Pawn>.
	getWeightOfIsolatedPawns	:: Attribute.CriterionWeight.CriterionWeight criterionWeight,	-- ^ The arithmetic difference between the total number of /isolated/ @Pawn@s on either side; <https://chessprogramming.wikispaces.com/Isolated+Pawn>.
	getWeightOfPassedPawns		:: Attribute.CriterionWeight.CriterionWeight criterionWeight	-- ^ The arithmetic difference between the total number of /passed/ @Pawn@s on either side; <https://chessprogramming.wikispaces.com/Passed+Pawn>.
} deriving (Eq, Show)

-- | Smart-constructor.
mkCriteriaWeights
	:: (Eq criterionWeight, Num criterionWeight)
	=> Attribute.CriterionWeight.CriterionWeight criterionWeight	-- ^ /material/.
	-> Attribute.CriterionWeight.CriterionWeight criterionWeight	-- ^ /mobility/.
	-> Attribute.CriterionWeight.CriterionWeight criterionWeight	-- ^ /pieceSquareValue/.
	-> Attribute.CriterionWeight.CriterionWeight criterionWeight	-- ^ /castlingPotential/.
	-> Attribute.CriterionWeight.CriterionWeight criterionWeight	-- ^ /defence/.
	-> Attribute.CriterionWeight.CriterionWeight criterionWeight	-- ^ /doubledPawns/.
	-> Attribute.CriterionWeight.CriterionWeight criterionWeight	-- ^ /isolatedPawns/.
	-> Attribute.CriterionWeight.CriterionWeight criterionWeight	-- ^ /passedPawns/.
	-> CriteriaWeights criterionWeight
mkCriteriaWeights a b c d e f g h
	| criteriaWeights == minBound	= Control.Exception.throw $ Data.Exception.mkInvalidDatum "BishBosh.Input.CriteriaWeights.mkCriteriaWeights:\tall weights are zero."
	| otherwise			= criteriaWeights
	where
		criteriaWeights	= MkCriteriaWeights a b c d e f g h

instance Real criterionWeight => Property.ShowFloat.ShowFloat (CriteriaWeights criterionWeight) where
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
		Control.Arrow.second $ fromDouble . realToFrac . Attribute.CriterionWeight.deconstruct
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

instance Num criterionWeight => Data.Default.Default (CriteriaWeights criterionWeight) where
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

instance Control.DeepSeq.NFData criterionWeight => Control.DeepSeq.NFData (CriteriaWeights criterionWeight) where
	rnf (MkCriteriaWeights a b c d e f g h)	= Control.DeepSeq.rnf [a, b, c, d, e, f, g, h]

instance Num criterionWeight => Bounded (CriteriaWeights criterionWeight) where
	maxBound	= MkCriteriaWeights maxBound maxBound maxBound maxBound maxBound maxBound maxBound maxBound
	minBound	= MkCriteriaWeights minBound minBound minBound minBound minBound minBound minBound minBound

instance (
	HXT.XmlPickler	criterionWeight,
	Num		criterionWeight,
	Ord		criterionWeight,
	Show		criterionWeight
 ) => HXT.XmlPickler (CriteriaWeights criterionWeight) where
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
	* Returns the weighted sum of the specified criteria, divided by the sum of the weights; <https://chessprogramming.wikispaces.com/evaluation>

	* Each criterion increases in proportion to some desirable attribute of the proposed /game/.

	* Each criterion should be in the same range of magnitudes, so that none dominates the total, thus making the total a clear measure of the value attributed to each.

	* Also writes individual unweighted /criterion-value/s, to facilitate post-analysis; if the corresponding weight is zero, for efficiency evaluation of the criterion is avoided.
-}
calculateWeightedMean :: (
	Fractional	weightedMean,
	Real		criterionValue,
	Real		criterionWeight
 )
	=> CriteriaWeights criterionWeight
	-> Attribute.CriterionValue.CriterionValue criterionValue						-- ^ /material/:	maximum if a player's /move/ equals the maximum total piece value (including /queened/ @Pawn@s), while their opponent has just a @King@.
	-> Attribute.CriterionValue.CriterionValue criterionValue						-- ^ /mobility/:	maximum when the opponent is check-mated.
	-> Attribute.CriterionValue.CriterionValue criterionValue						-- ^ /pieceSquareValue/:	maximum when this player occupies all the strategically important squares & the opponent none.
	-> Attribute.CriterionValue.CriterionValue criterionValue						-- ^ /castlingPotential/:	maximum when this player either has /castled/ or can, but the opponent has been permanently prevented.
	-> Attribute.CriterionValue.CriterionValue criterionValue						-- ^ /defence/:	maximum when this player's /piece/s are fully utilised in defence, but none of the opponent's are.
	-> Attribute.CriterionValue.CriterionValue criterionValue						-- ^ /doubledPawns/:	maximum when this player hasn't any /doubled/ @Pawn@s & the opponent has two files of four @Pawn@s.
	-> Attribute.CriterionValue.CriterionValue criterionValue						-- ^ /isolatedPawns/:	maximum when this player hasn't any /isolated/ @Pawn@s & all the opponent's @Pawn@s are /isolated/.
	-> Attribute.CriterionValue.CriterionValue criterionValue						-- ^ /passedPawns/:	maximum when this player has 8 /passed/ @Pawn@s & the opponent has none.
	-> Attribute.WeightedMeanAndCriterionValues.WeightedMeanAndCriterionValues weightedMean criterionValue	-- ^ The individual /criteria/ values, & their /weighted mean/.
{-# SPECIALISE calculateWeightedMean
	:: CriteriaWeights T.CriterionWeight
	-> Attribute.CriterionValue.CriterionValue T.CriterionValue
	-> Attribute.CriterionValue.CriterionValue T.CriterionValue
	-> Attribute.CriterionValue.CriterionValue T.CriterionValue
	-> Attribute.CriterionValue.CriterionValue T.CriterionValue
	-> Attribute.CriterionValue.CriterionValue T.CriterionValue
	-> Attribute.CriterionValue.CriterionValue T.CriterionValue
	-> Attribute.CriterionValue.CriterionValue T.CriterionValue
	-> Attribute.CriterionValue.CriterionValue T.CriterionValue
	-> Attribute.WeightedMeanAndCriterionValues.WeightedMeanAndCriterionValues T.WeightedMean T.CriterionValue
 #-}
calculateWeightedMean MkCriteriaWeights {
	getWeightOfMaterial		= weightOfMaterial,
	getWeightOfMobility		= weightOfMobility,
	getWeightOfPieceSquareValue	= weightOfPieceSquareValue,
	getWeightOfCastlingPotential	= weightOfCastlingPotential,
	getWeightOfDefence		= weightOfDefence,
	getWeightOfDoubledPawns		= weightOfDoubledPawns,
	getWeightOfIsolatedPawns	= weightOfIsolatedPawns,
	getWeightOfPassedPawns		= weightOfPassedPawns
} material mobility pieceSquareValue castlingPotential defence doubledPawns isolatedPawns passedPawns	= Attribute.CriterionValue.calculateWeightedMean [
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
type Transformation criterionWeight	= CriteriaWeights criterionWeight -> CriteriaWeights criterionWeight

-- | Adjust the mean weight, so that the maximum weight is @1@.
normalise :: (
	Fractional	criterionWeight,
	Ord		criterionWeight,
	Show		criterionWeight
 ) => Transformation criterionWeight
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
	normaliseCriterionWeight	= Attribute.CriterionWeight.mkCriterionWeight . (
		/ Attribute.CriterionWeight.deconstruct (
			maximum [
				weightOfMaterial,
				weightOfMobility,
				weightOfPieceSquareValue,
				weightOfCastlingPotential,
				weightOfDefence,
				weightOfDoubledPawns,
				weightOfIsolatedPawns,
				weightOfPassedPawns
			]
		)
	 ) . Attribute.CriterionWeight.deconstruct

{- |
	* Independently perturbs each /criterion-weight/ by a random value, of configurable magnitude.

	* Under this transformation, /criterion-weight/s of @0@, will remain unchanged, thus irrelevant criteria remain irrelevant.
-}
perturbWeights :: (
	Fractional		criterionWeight,
	Real			criterionWeight,
	Show			criterionWeight,
	System.Random.RandomGen	randomGen
 )
	=> randomGen
	-> criterionWeight	-- ^ Change-magnitude.
	-> Transformation criterionWeight
perturbWeights _ 0 criteriaWeights	= criteriaWeights
perturbWeights randomGen changeMagnitude MkCriteriaWeights {
	getWeightOfMaterial		= weightOfMaterial,
	getWeightOfMobility		= weightOfMobility,
	getWeightOfPieceSquareValue	= weightOfPieceSquareValue,
	getWeightOfCastlingPotential	= weightOfCastlingPotential,
	getWeightOfDefence		= weightOfDefence,
	getWeightOfDoubledPawns		= weightOfDoubledPawns,
	getWeightOfIsolatedPawns	= weightOfIsolatedPawns,
	getWeightOfPassedPawns		= weightOfPassedPawns
} = Control.Exception.assert (changeMagnitude > 0) $ normalise MkCriteriaWeights {
	getWeightOfMaterial		= reduceBy a weightOfMaterial,
	getWeightOfMobility		= reduceBy b weightOfMobility,
	getWeightOfPieceSquareValue	= reduceBy c weightOfPieceSquareValue,
	getWeightOfCastlingPotential	= reduceBy d weightOfCastlingPotential,
	getWeightOfDefence		= reduceBy e weightOfDefence,
	getWeightOfDoubledPawns		= reduceBy f weightOfDoubledPawns,
	getWeightOfIsolatedPawns	= reduceBy g weightOfIsolatedPawns,
	getWeightOfPassedPawns		= reduceBy h weightOfPassedPawns
} where
	(a : b : c : d : e : f : g : h : _)	= System.Random.randomRs (1 :: Double, succ $ realToFrac changeMagnitude) randomGen
	reduceBy randomValue			= Attribute.CriterionWeight.mkCriterionWeight . (/ realToFrac randomValue) . Attribute.CriterionWeight.deconstruct	-- N.B. this always reduces the weight, leaving 'normalise' to correct this.

-- | A list of named accessors & mutators.
onymousOperators :: [
	(
		String,												-- Tag.
		CriteriaWeights criterionWeight -> Attribute.CriterionWeight.CriterionWeight criterionWeight,	-- Accessor.
		Attribute.CriterionWeight.CriterionWeight criterionWeight -> Transformation criterionWeight	-- Mutator.
	) -- Triple.
 ]
onymousOperators	= [
	(
		weightOfMaterialTag,
		getWeightOfMaterial,
		\w criteriaWeights -> criteriaWeights { getWeightOfMaterial = w }
	), (
		weightOfMobilityTag,
		getWeightOfMobility,
		\w criteriaWeights -> criteriaWeights { getWeightOfMobility = w }
	), (
		weightOfPieceSquareValueTag,
		getWeightOfPieceSquareValue,
		\w criteriaWeights -> criteriaWeights { getWeightOfPieceSquareValue = w }
	), (
		weightOfCastlingPotentialTag,
		getWeightOfCastlingPotential,
		\w criteriaWeights -> criteriaWeights { getWeightOfCastlingPotential = w }
	), (
		weightOfDefenceTag,
		getWeightOfDefence,
		\w criteriaWeights -> criteriaWeights { getWeightOfDefence = w }
	), (
		weightOfDoubledPawnsTag,
		getWeightOfDoubledPawns,
		\w criteriaWeights -> criteriaWeights { getWeightOfDoubledPawns = w }
	), (
		weightOfIsolatedPawnsTag,
		getWeightOfIsolatedPawns,
		\w criteriaWeights -> criteriaWeights { getWeightOfIsolatedPawns = w }
	), (
		weightOfPassedPawnsTag,
		getWeightOfPassedPawns,
		\w criteriaWeights -> criteriaWeights { getWeightOfPassedPawns = w }
	)
 ]

