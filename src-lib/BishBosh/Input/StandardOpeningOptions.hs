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

 [@DESCRIPTION@]	Defines configurable options related to the process of searching standard-openings.
-}

module BishBosh.Input.StandardOpeningOptions(
-- * Types
-- ** Type-synonyms
--	TryToMatchMoves,
--	TryToMatchViaJoiningMove,
--	TryToMatchColourFlippedPosition,
	MatchSwitches,
-- ** Data-types
	StandardOpeningOptions(
--		MkStandardOpeningOptions,
--		getTryToMatchMoves,
--		getTryToMatchViaJoiningMove,
--		getTryToMatchColourFlippedPosition
	),
-- * Constants
	tag,
--	tryToMatchMovesTag,
--	tryToMatchViaJoiningMoveTag,
--	tryToMatchColourFlippedPositionTag,
-- * Functions
-- ** Constructor
	mkStandardOpeningOptions,
-- ** Accessors
	getMatchSwitches
) where

import			BishBosh.Data.Bool()		-- For 'HXT.xpickle'.
import qualified	BishBosh.Text.ShowList		as Text.ShowList
import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT

-- | Used to qualify XML.
tag :: String
tag					= "standardOpeningOptions"

-- | Used to qualify XML.
tryToMatchMovesTag :: String
tryToMatchMovesTag			= "tryToMatchMoves"

-- | Used to qualify XML.
tryToMatchViaJoiningMoveTag :: String
tryToMatchViaJoiningMoveTag		= "tryToMatchViaJoiningMove"

-- | Used to qualify XML.
tryToMatchColourFlippedPositionTag :: String
tryToMatchColourFlippedPositionTag	= "tryToMatchColourFlippedPosition"

-- | Whether to attempt to exactly match moves with a standard opening; transpositions won't be matched.
type TryToMatchMoves	= Bool

-- | Whether to attempt to join the current position to a standard opening that's only one ply away.
type TryToMatchViaJoiningMove	= Bool

-- | Whether to attempt to match a colour-flipped version of the current position with a standard opening
type TryToMatchColourFlippedPosition	= Bool

-- | The switches used to control attempts to find a match amongst standard openings.
type MatchSwitches	= (TryToMatchMoves, TryToMatchViaJoiningMove, TryToMatchColourFlippedPosition)

-- | Defines options related to searching for a move.
data StandardOpeningOptions	= MkStandardOpeningOptions {
	getTryToMatchMoves			:: TryToMatchMoves,			-- ^ Whether to attempt to exactly match moves with a standard opening; transpositions won't be matched.
	getTryToMatchViaJoiningMove		:: TryToMatchViaJoiningMove,		-- ^ Whether to attempt to join the current position to a standard opening that's only one ply away.
	getTryToMatchColourFlippedPosition	:: TryToMatchColourFlippedPosition	-- ^ Whether to attempt to match a colour-flipped version of the current position with a standard opening.
} deriving Eq

instance Control.DeepSeq.NFData StandardOpeningOptions where
	rnf MkStandardOpeningOptions {
		getTryToMatchMoves			= tryToMatchMoves,
		getTryToMatchViaJoiningMove		= tryToMatchViaJoiningMove,
		getTryToMatchColourFlippedPosition	= tryToMatchColourFlippedPosition
	} = Control.DeepSeq.rnf (tryToMatchMoves, tryToMatchViaJoiningMove, tryToMatchColourFlippedPosition)

instance Show StandardOpeningOptions where
	showsPrec _ MkStandardOpeningOptions {
		getTryToMatchMoves			= tryToMatchMoves,
		getTryToMatchViaJoiningMove		= tryToMatchViaJoiningMove,
		getTryToMatchColourFlippedPosition	= tryToMatchColourFlippedPosition
	} = Text.ShowList.showsAssociationList' [
		(
			tryToMatchMovesTag,
			shows tryToMatchMoves
		), (
			tryToMatchViaJoiningMoveTag,
			shows tryToMatchViaJoiningMove
		), (
			tryToMatchColourFlippedPositionTag,
			shows tryToMatchColourFlippedPosition
		)
	 ]

instance Data.Default.Default StandardOpeningOptions where
	def = MkStandardOpeningOptions {
		getTryToMatchMoves			= True,
		getTryToMatchViaJoiningMove		= True,
		getTryToMatchColourFlippedPosition	= True
	}

instance HXT.XmlPickler StandardOpeningOptions where
	xpickle	= HXT.xpDefault Data.Default.def . HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c) -> mkStandardOpeningOptions a b c,	-- Construct.
		\MkStandardOpeningOptions {
			getTryToMatchMoves			= tryToMatchMoves,
			getTryToMatchViaJoiningMove		= tryToMatchViaJoiningMove,
			getTryToMatchColourFlippedPosition	= tryToMatchColourFlippedPosition
		} -> (tryToMatchMoves, tryToMatchViaJoiningMove, tryToMatchColourFlippedPosition) -- Deconstruct.
	 ) $ HXT.xpTriple(
		getTryToMatchMoves def `HXT.xpDefault` HXT.xpAttr tryToMatchMovesTag HXT.xpickle
	 ) (
		getTryToMatchViaJoiningMove def `HXT.xpDefault` HXT.xpAttr tryToMatchViaJoiningMoveTag HXT.xpickle
	 ) (
		getTryToMatchColourFlippedPosition def `HXT.xpDefault` HXT.xpAttr tryToMatchColourFlippedPositionTag HXT.xpickle
	 ) where
		def	= Data.Default.def

-- | Smart constructor.
mkStandardOpeningOptions
	:: TryToMatchMoves
	-> TryToMatchViaJoiningMove
	-> TryToMatchColourFlippedPosition
	-> StandardOpeningOptions
mkStandardOpeningOptions tryToMatchMoves tryToMatchViaJoiningMove tryToMatchColourFlippedPosition	= MkStandardOpeningOptions {
	getTryToMatchMoves			= tryToMatchMoves,
	getTryToMatchViaJoiningMove		= tryToMatchViaJoiningMove,
	getTryToMatchColourFlippedPosition	= tryToMatchColourFlippedPosition
}

-- | Accessor.
getMatchSwitches :: StandardOpeningOptions -> MatchSwitches
getMatchSwitches MkStandardOpeningOptions {
	getTryToMatchMoves			= tryToMatchMoves,
	getTryToMatchViaJoiningMove		= tryToMatchViaJoiningMove,
	getTryToMatchColourFlippedPosition	= tryToMatchColourFlippedPosition
} = (tryToMatchMoves, tryToMatchViaJoiningMove, tryToMatchColourFlippedPosition)

