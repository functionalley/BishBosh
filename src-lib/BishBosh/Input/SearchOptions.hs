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

 [@DESCRIPTION@]	Defines configurable options related to the process of searching for the optimal move.
-}

module BishBosh.Input.SearchOptions(
-- * Types
-- ** Type-synonyms
	SortOnStandardOpeningMoveFrequency,
	HammingDistance,
--	Transformation,
--	RecordKillerMoves,
	MaybeRetireAfterNMoves,
	TrapRepeatedPositions,
--	UsePondering,
	MaybeUseTranspositions,
--	StandardOpeningOptions,
--	SearchDepthByLogicalColour,
	Reader,
-- ** Data-types
	SearchOptions(
--		MkSearchOptions,
		getSortOnStandardOpeningMoveFrequency,
		getMaybeCaptureMoveSortAlgorithm,
		getMaybeMinimumHammingDistance,
		getMaybeRetireKillerMovesAfter,
		getTrapRepeatedPositions,
		getUsePondering,
		getMaybeUseTranspositions,
		getStandardOpeningOptions,
		getSearchDepthByLogicalColour
	),
-- * Constants
	tag,
	sortOnStandardOpeningMoveFrequencyTag,
--	minimumHammingDistanceTag,
--	retireKillerMovesAfterTag,
--	trapRepeatedPositionsTag,
--	usePonderingTag,
--	retireTranspositionsAfterTag,
--	minimumTranspositionSearchDepthTag,
--	standardOpeningOptionsTag,
	searchDepthTag,
--	searchDepthByLogicalColourTag
	minimumSearchDepth,
	defaultSearchDepth,
-- * Functions
-- ** Constructor
	mkSearchOptions,
-- ** Accessors
	getSearchDepth,
	maybeRetireTranspositionsAfter,
	maybeMinimumTranspositionSearchDepth,
	identifyAutomatedPlayers,
-- ** Mutators
	setSearchDepth,
	swapSearchDepth,
-- ** Predicates
	recordKillerMoves
) where

import			BishBosh.Data.Bool()		-- For 'HXT.xpickle'.
import			Control.Arrow((***))
import qualified	BishBosh.Attribute.CaptureMoveSortAlgorithm	as Attribute.CaptureMoveSortAlgorithm
import qualified	BishBosh.Attribute.LogicalColour		as Attribute.LogicalColour
import qualified	BishBosh.Data.Exception				as Data.Exception
import qualified	BishBosh.Data.Foldable
import qualified	BishBosh.Input.StandardOpeningOptions		as Input.StandardOpeningOptions
import qualified	BishBosh.Property.Empty				as Property.Empty
import qualified	BishBosh.Property.Opposable			as Property.Opposable
import qualified	BishBosh.Text.Case				as Text.Case
import qualified	BishBosh.Text.ShowList				as Text.ShowList
import qualified	BishBosh.Type.Count				as Type.Count
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Control.Monad.Reader
import qualified	Data.Default
import qualified	Data.Foldable
import qualified	Data.Map.Strict					as Map
import qualified	Data.Maybe
import qualified	Text.XML.HXT.Arrow.Pickle			as HXT

-- | Used to qualify XML.
tag :: String
tag					= "searchOptions"

-- | Used to qualify XML.
sortOnStandardOpeningMoveFrequencyTag :: String
sortOnStandardOpeningMoveFrequencyTag	= "sortOnStandardOpeningMoveFrequency"

-- | Used to qualify XML.
minimumHammingDistanceTag :: String
minimumHammingDistanceTag		= "minimumHammingDistance"

-- | Used to qualify XML.
retireKillerMovesAfterTag :: String
retireKillerMovesAfterTag		= "retireKillerMovesAfter"

-- | Used to qualify XML.
trapRepeatedPositionsTag :: String
trapRepeatedPositionsTag		= "trapRepeatedPositions"

-- | Used to qualify XML.
usePonderingTag :: String
usePonderingTag				= "usePondering"

-- | Used to qualify XML.
retireTranspositionsAfterTag :: String
retireTranspositionsAfterTag		= "retireTranspositionsAfter"

-- | Used to qualify XML.
minimumTranspositionSearchDepthTag :: String
minimumTranspositionSearchDepthTag	= "minimumTranspositionSearchDepth"

-- | Used to qualify XML.
searchDepthTag :: String
searchDepthTag				= "searchDepth"

-- | Used to qualify XML.
searchDepthByLogicalColourTag :: String
searchDepthByLogicalColourTag		= showString searchDepthTag . showString "By" $ Text.Case.toUpperInitial Attribute.LogicalColour.tag

-- | The constant minimum permissible search-depth.
minimumSearchDepth :: Type.Count.NPlies
minimumSearchDepth	= 1

{- |
	* The constant default search-depth.

	* CAVEAT: this is rather arbitrary.
-}
defaultSearchDepth :: Type.Count.NPlies
defaultSearchDepth	= 4

-- | Sort moves on the decreasing frequency of occurrence in standard openings.
type SortOnStandardOpeningMoveFrequency	= Bool

-- | The optional minimum Hamming-distance between the random numbers from which Zobrist-hashes are composed.
type HammingDistance			= Int

-- | The number of full moves (one for each player) after which to retire killer moves.
type MaybeRetireAfterNMoves		= Maybe Type.Count.NMoves

-- | Whether to short-circuit the fitness-evaluation of /position/s which have been visited before in the current /game/.
type TrapRepeatedPositions		= Bool

-- | Whether to ponder about one's next move while the opponent is thinking.
type UsePondering			= Bool

-- | The number of full moves (one for each player) after which to retire transpositions & the search-depth beneath which they aren't recorded at all.
type MaybeUseTranspositions		= Maybe (Type.Count.NMoves, Type.Count.NPlies)

-- | The depth to search for each /logical colour/.
type SearchDepthByLogicalColour		= Map.Map Attribute.LogicalColour.LogicalColour Type.Count.NPlies

-- | Defines options related to searching for a move.
data SearchOptions	= MkSearchOptions {
	getSortOnStandardOpeningMoveFrequency	:: SortOnStandardOpeningMoveFrequency,	-- ^ Whether to sort moves on the decreasing frequency of occurrence in standard openings.
	getMaybeCaptureMoveSortAlgorithm	:: Maybe Attribute.CaptureMoveSortAlgorithm.CaptureMoveSortAlgorithm,
	getMaybeMinimumHammingDistance		:: Maybe HammingDistance,		-- ^ The optional minimum Hamming-distance between the random numbers from which Zobrist-hashes are composed.
	getMaybeRetireKillerMovesAfter		:: MaybeRetireAfterNMoves,		-- ^ The number of full moves (one for each player) after which to retire killer moves.
	getTrapRepeatedPositions		:: TrapRepeatedPositions,		-- ^ Whether to short-circuit the fitness-evaluation of /position/s which have been visited before in the current /game/.
	getUsePondering				:: UsePondering,			-- ^ Whether to ponder about one's next move while the opponent is thinking.
	getMaybeUseTranspositions		:: MaybeUseTranspositions,
	getStandardOpeningOptions		:: Input.StandardOpeningOptions.StandardOpeningOptions,
	getSearchDepthByLogicalColour		:: SearchDepthByLogicalColour		-- ^ The depth to search for each /logical colour/.
} deriving Eq

instance Control.DeepSeq.NFData SearchOptions where
	rnf MkSearchOptions {
		getSortOnStandardOpeningMoveFrequency	= sortOnStandardOpeningMoveFrequency,
		getMaybeCaptureMoveSortAlgorithm	= maybeCaptureMoveSortAlgorithm,
		getMaybeMinimumHammingDistance		= maybeMinimumHammingDistance,
		getMaybeRetireKillerMovesAfter		= maybeRetireKillerMovesAfter,
		getTrapRepeatedPositions		= trapRepeatedPositions,
		getUsePondering				= usePondering,
		getMaybeUseTranspositions		= maybeUseTranspositions,
		getStandardOpeningOptions		= standardOpeningOptions,
		getSearchDepthByLogicalColour		= searchDepthByLogicalColour
	} = Control.DeepSeq.rnf (
		(
			sortOnStandardOpeningMoveFrequency, maybeCaptureMoveSortAlgorithm, maybeMinimumHammingDistance, maybeRetireKillerMovesAfter
		), (
			trapRepeatedPositions, usePondering, maybeUseTranspositions, standardOpeningOptions, searchDepthByLogicalColour
		)
	 )

instance Show SearchOptions where
	showsPrec _ MkSearchOptions {
		getSortOnStandardOpeningMoveFrequency	= sortOnStandardOpeningMoveFrequency,
		getMaybeCaptureMoveSortAlgorithm	= maybeCaptureMoveSortAlgorithm,
		getMaybeMinimumHammingDistance		= maybeMinimumHammingDistance,
		getMaybeRetireKillerMovesAfter		= maybeRetireKillerMovesAfter,
		getTrapRepeatedPositions		= trapRepeatedPositions,
		getUsePondering				= usePondering,
		getMaybeUseTranspositions		= maybeUseTranspositions,
		getStandardOpeningOptions		= standardOpeningOptions,
		getSearchDepthByLogicalColour		= searchDepthByLogicalColour
	} = Text.ShowList.showsAssociationList' . Data.Maybe.maybe id (
		(:) . (,) Attribute.CaptureMoveSortAlgorithm.tag . shows
	 ) maybeCaptureMoveSortAlgorithm . Data.Maybe.maybe id (
		(:) . (,) minimumHammingDistanceTag . shows
	 ) maybeMinimumHammingDistance . Data.Maybe.maybe id (
		(:) . (,) retireKillerMovesAfterTag . shows
	 ) maybeRetireKillerMovesAfter $ Data.Maybe.maybe id (
		\(retireTranspositionsAfter, minimumTranspositionSearchDepth)	-> (++) [
			(
				sortOnStandardOpeningMoveFrequencyTag,
				shows sortOnStandardOpeningMoveFrequency
			), (
				retireTranspositionsAfterTag,
				shows retireTranspositionsAfter
			), (
				minimumTranspositionSearchDepthTag,
				shows minimumTranspositionSearchDepth
			)
		]
	 ) maybeUseTranspositions [
		(
			trapRepeatedPositionsTag,
			shows trapRepeatedPositions
		), (
			usePonderingTag,
			shows usePondering
		), (
			Input.StandardOpeningOptions.tag,
			shows standardOpeningOptions
		), (
			searchDepthByLogicalColourTag,
			Text.ShowList.showsAssociationList' . map (show *** shows) $ Map.toList searchDepthByLogicalColour
		)
	 ]

instance Data.Default.Default SearchOptions where
	def = MkSearchOptions {
		getSortOnStandardOpeningMoveFrequency	= True,
		getMaybeCaptureMoveSortAlgorithm	= Nothing,
		getMaybeMinimumHammingDistance		= Nothing,
		getMaybeRetireKillerMovesAfter		= Nothing,
		getTrapRepeatedPositions		= True,
		getUsePondering				= False,
		getMaybeUseTranspositions		= Nothing,
		getStandardOpeningOptions		= Data.Default.def,
		getSearchDepthByLogicalColour		= Property.Empty.empty	-- Manual play.
	}

instance HXT.XmlPickler SearchOptions where
	xpickle	= HXT.xpDefault Data.Default.def . HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d, e, f, g, h, i) -> mkSearchOptions a b c d e f g h i,	-- Construct.
		\MkSearchOptions {
			getSortOnStandardOpeningMoveFrequency	= sortOnStandardOpeningMoveFrequency,
			getMaybeCaptureMoveSortAlgorithm	= maybeCaptureMoveSortAlgorithm,
			getMaybeMinimumHammingDistance		= maybeMinimumHammingDistance,
			getMaybeRetireKillerMovesAfter		= maybeRetireKillerMovesAfter,
			getTrapRepeatedPositions		= trapRepeatedPositions,
			getUsePondering				= usePondering,
			getMaybeUseTranspositions		= maybeUseTranspositions,
			getStandardOpeningOptions		= standardOpeningOptions,
			getSearchDepthByLogicalColour		= searchDepthByLogicalColour
		} -> (
			sortOnStandardOpeningMoveFrequency,
			maybeCaptureMoveSortAlgorithm,
			maybeMinimumHammingDistance,
			maybeRetireKillerMovesAfter,
			trapRepeatedPositions,
			usePondering,
			maybeUseTranspositions,
			standardOpeningOptions,
			searchDepthByLogicalColour
		) -- Deconstruct.
	 ) $ HXT.xp9Tuple (
		getSortOnStandardOpeningMoveFrequency def `HXT.xpDefault` HXT.xpAttr sortOnStandardOpeningMoveFrequencyTag HXT.xpickle
	 ) (
		HXT.xpOption HXT.xpickle {-CaptureMoveSortAlgorithm-}
	 ) (
		HXT.xpAttrImplied minimumHammingDistanceTag HXT.xpInt
	 ) (
		HXT.xpAttrImplied retireKillerMovesAfterTag HXT.xpickle {-NMoves-}
	 ) (
		getTrapRepeatedPositions def `HXT.xpDefault` HXT.xpAttr trapRepeatedPositionsTag HXT.xpickle {-Bool-}
	 ) (
		getUsePondering def `HXT.xpDefault` HXT.xpAttr usePonderingTag HXT.xpickle {-Bool-}
	 ) (
		HXT.xpOption . HXT.xpElem "transpositions" $ HXT.xpAttr retireTranspositionsAfterTag HXT.xpickle {-NMoves-} `HXT.xpPair` HXT.xpAttr minimumTranspositionSearchDepthTag HXT.xpickle {-NPlies-}
	 ) HXT.xpickle {-standardOpeningOptions-} (
		HXT.xpElem searchDepthByLogicalColourTag . HXT.xpWrap (
			Map.fromList . (
				\l	-> let
					duplicateLogicalColours	= BishBosh.Data.Foldable.findDuplicates $ map fst {-logicalColour-} l
				in if null duplicateLogicalColours
					then l
					else Control.Exception.throw . Data.Exception.mkDuplicateData . showString "BishBosh.Input.SearchOptions.xpickle:\t" . showString Attribute.LogicalColour.tag . showString "s must be distinct; " $ shows duplicateLogicalColours "."
			),	-- Construct from a List.
			Map.toList		-- Deconstruct to an association-list.
		) . HXT.xpList {-potentially null-} . HXT.xpElem (
			showString "by" $ Text.Case.toUpperInitial Attribute.LogicalColour.tag
		) $ HXT.xpickle {-LogicalColour-} `HXT.xpPair` HXT.xpAttr searchDepthTag HXT.xpickle {-NPlies-}
	 ) where
		def	= Data.Default.def

-- | Smart constructor.
mkSearchOptions
	:: SortOnStandardOpeningMoveFrequency
	-> Maybe Attribute.CaptureMoveSortAlgorithm.CaptureMoveSortAlgorithm
	-> Maybe HammingDistance	-- ^ The optional lower bound on the Hamming-distance between the random numbers used to compose Zobrist hashes from /position/s.
	-> MaybeRetireAfterNMoves	-- ^ The number of full moves back from the current position, after which to retire killer-moves.
	-> TrapRepeatedPositions
	-> UsePondering
	-> MaybeUseTranspositions	-- ^ The number of full moves after which to retire transpositions, & the search-depth beneath which transpositions aren't recorded.
	-> Input.StandardOpeningOptions.StandardOpeningOptions
	-> SearchDepthByLogicalColour
	-> SearchOptions
mkSearchOptions sortOnStandardOpeningMoveFrequency maybeCaptureMoveSortAlgorithm maybeMinimumHammingDistance maybeRetireKillerMovesAfter trapRepeatedPositions usePondering maybeUseTranspositions standardOpeningOptions searchDepthByLogicalColour
	| Just minimumHammingDistance		<- maybeMinimumHammingDistance
	, minimumHammingDistance < 1	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.SearchOptions.mkSearchOptions:\t" . showString minimumHammingDistanceTag . Text.ShowList.showsAssociation $ shows minimumHammingDistance " must exceed zero."
	| Just retireKillerMovesAfter		<- maybeRetireKillerMovesAfter
	, retireKillerMovesAfter < 0	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.SearchOptions.mkSearchOptions:\t" . showString retireKillerMovesAfterTag . Text.ShowList.showsAssociation $ shows retireKillerMovesAfter " can't be negative."
	| let nAutomatedPlayers	= Data.Foldable.length searchDepthByLogicalColour
	, usePondering && nAutomatedPlayers /= 1
	= Control.Exception.throw . Data.Exception.mkIncompatibleData . showString "BishBosh.Input.SearchOptions.mkSearchOptions:\tpondering is pointless unless there's an automated player who can use the unused CPU-time during a manual player's move, but there're " $ shows nAutomatedPlayers " automated players."
	| Just (retireTranspositionsAfter, _)	<- maybeUseTranspositions
	, retireTranspositionsAfter < 0	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.SearchOptions.mkSearchOptions:\t" . showString retireTranspositionsAfterTag . Text.ShowList.showsAssociation $ shows retireTranspositionsAfter " can't be negative."
	| Just (_, minimumTranspositionSearchDepth)	<- maybeUseTranspositions
	, minimumTranspositionSearchDepth < 1	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.SearchOptions.mkSearchOptions:\t" . showString minimumTranspositionSearchDepthTag . Text.ShowList.showsAssociation $ shows minimumTranspositionSearchDepth " must exceed zero."
	| Just (_, minimumTranspositionSearchDepth)	<- maybeUseTranspositions
	, not $ Data.Foldable.null searchDepthByLogicalColour
	, Data.Foldable.all (
		minimumTranspositionSearchDepth >
	) searchDepthByLogicalColour	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.SearchOptions.mkSearchOptions:\t" . showString minimumTranspositionSearchDepthTag . Text.ShowList.showsAssociation $ shows minimumTranspositionSearchDepth . showString " exceeds " . showString searchDepthTag . Text.ShowList.showsAssociation $ shows (Map.toList searchDepthByLogicalColour) "."
	| Data.Foldable.any (
		< minimumSearchDepth
	) searchDepthByLogicalColour	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.SearchOptions.mkSearchOptions:\t" $ showString searchDepthTag " must be positive."
	| otherwise	= MkSearchOptions {
		getSortOnStandardOpeningMoveFrequency	= sortOnStandardOpeningMoveFrequency,
		getMaybeCaptureMoveSortAlgorithm	= maybeCaptureMoveSortAlgorithm,
		getMaybeMinimumHammingDistance		= maybeMinimumHammingDistance,
		getMaybeRetireKillerMovesAfter		= maybeRetireKillerMovesAfter,
		getTrapRepeatedPositions		= trapRepeatedPositions,
		getUsePondering				= usePondering,
		getMaybeUseTranspositions		= maybeUseTranspositions,
		getStandardOpeningOptions		= standardOpeningOptions,
		getSearchDepthByLogicalColour		= searchDepthByLogicalColour
	}

-- | Get either player's search-depth, using a default value when none are defined.
getSearchDepth :: SearchOptions -> Type.Count.NPlies
getSearchDepth MkSearchOptions { getSearchDepthByLogicalColour = searchDepthByLogicalColour }	= Data.Maybe.fromMaybe defaultSearchDepth . Data.Maybe.listToMaybe $ Data.Foldable.toList searchDepthByLogicalColour	-- Manual players don't have a searchDepth, so use the opponent's settings.

-- | Self-documentation.
type RecordKillerMoves	= Bool

-- | Whether killer-moves are to be recorded.
recordKillerMoves :: SearchOptions -> RecordKillerMoves
recordKillerMoves MkSearchOptions { getMaybeRetireKillerMovesAfter = maybeRetireKillerMovesAfter }	= Data.Maybe.isJust maybeRetireKillerMovesAfter

-- | When to retire transpositions.
maybeRetireTranspositionsAfter :: SearchOptions -> MaybeRetireAfterNMoves
maybeRetireTranspositionsAfter MkSearchOptions { getMaybeUseTranspositions = maybeUseTranspositions }	= fmap fst {-NMoves-} maybeUseTranspositions

-- | The search-depth beneath which transpositions aren't recorded.
maybeMinimumTranspositionSearchDepth :: SearchOptions -> Maybe Type.Count.NPlies
maybeMinimumTranspositionSearchDepth MkSearchOptions { getMaybeUseTranspositions = maybeUseTranspositions }	= fmap snd {-NPlies-} maybeUseTranspositions

-- | The type of a function used to transform 'SearchOptions'.
type Transformation	= SearchOptions -> SearchOptions

-- | Mutator.
setSearchDepth :: Type.Count.NPlies -> Transformation
setSearchDepth searchDepth searchOptions@MkSearchOptions { getSearchDepthByLogicalColour = searchDepthByLogicalColour }
	| searchDepth < minimumSearchDepth	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.SearchOptions.setSearchDepth:\t" . showString searchDepthTag . Text.ShowList.showsAssociation $ shows searchDepth " must be positive."
	| otherwise	= searchOptions { getSearchDepthByLogicalColour = Map.map (const searchDepth) searchDepthByLogicalColour }

-- | Swap the /logical colour/ associated with any /searchDepth/ currently assigned.
swapSearchDepth :: Transformation
swapSearchDepth searchOptions@MkSearchOptions {
	getSearchDepthByLogicalColour	= searchDepthByLogicalColour
} = searchOptions {
	getSearchDepthByLogicalColour	= Map.mapKeys Property.Opposable.getOpposite searchDepthByLogicalColour
}

-- | Extract those /logical colour/s for which a search-depth has been defined, which implies that the corresponding player is automated.
identifyAutomatedPlayers :: SearchOptions -> [Attribute.LogicalColour.LogicalColour]
identifyAutomatedPlayers MkSearchOptions {
	getSearchDepthByLogicalColour	= searchDepthByLogicalColour
} = Map.keys searchDepthByLogicalColour

-- | Self-documentation.
type Reader	= Control.Monad.Reader.Reader SearchOptions

