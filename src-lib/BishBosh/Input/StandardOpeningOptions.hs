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
-- ** Data-types
	StandardOpeningOptions(
--		MkStandardOpeningOptions,
--		getTryToMatchMoves,
--		getTryToMatchViaJoiningMove,
--		getTryToMatchColourFlippedPosition,
		getPreferVictories
	),
-- * Constants
	tag,
--	tryToMatchMovesTag,
--	tryToMatchViaJoiningMoveTag,
--	tryToMatchColourFlippedPositionTag,
--	preferVictoriesTag,
-- * Functions
-- ** Constructor
	mkStandardOpeningOptions,
-- ** Accessors
	getMatchSwitches
) where

import			BishBosh.Data.Bool()	-- For 'HXT.xpickle'.
import qualified	BishBosh.Text.ShowList						as Text.ShowList
import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	BishBosh.ContextualNotation.PositionHashQualifiedMoveTree	as ContextualNotation.PositionHashQualifiedMoveTree
import qualified	Text.XML.HXT.Arrow.Pickle					as HXT

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

-- | Used to qualify XML.
preferVictoriesTag :: String
preferVictoriesTag			= "preferVictories"

-- | Defines options related to searching for a move.
data StandardOpeningOptions	= MkStandardOpeningOptions {
	getTryToMatchMoves			:: ContextualNotation.PositionHashQualifiedMoveTree.TryToMatchMoves,			-- ^ Whether to attempt to exactly match moves with a standard opening; transpositions won't be matched.
	getTryToMatchViaJoiningMove		:: ContextualNotation.PositionHashQualifiedMoveTree.TryToMatchViaJoiningMove,		-- ^ Whether to attempt to join the current position to a standard opening that's only one ply away.
	getTryToMatchColourFlippedPosition	:: ContextualNotation.PositionHashQualifiedMoveTree.TryToMatchColourFlippedPosition,	-- ^ Whether to attempt to match a colour-flipped version of the current position with a standard opening.
	getPreferVictories			:: ContextualNotation.PositionHashQualifiedMoveTree.PreferVictories			-- ^ Whether from all matching positions extracted from PGN-Databases, to prefer moves which result in a greater probability of victory, for the player who has the next move.
} deriving Eq

instance Control.DeepSeq.NFData StandardOpeningOptions where
	rnf MkStandardOpeningOptions {
		getTryToMatchMoves			= tryToMatchMoves,
		getTryToMatchViaJoiningMove		= tryToMatchViaJoiningMove,
		getTryToMatchColourFlippedPosition	= tryToMatchColourFlippedPosition,
		getPreferVictories			= preferVictories
	} = Control.DeepSeq.rnf (tryToMatchMoves, tryToMatchViaJoiningMove, tryToMatchColourFlippedPosition, preferVictories)

instance Show StandardOpeningOptions where
	showsPrec _ MkStandardOpeningOptions {
		getTryToMatchMoves			= tryToMatchMoves,
		getTryToMatchViaJoiningMove		= tryToMatchViaJoiningMove,
		getTryToMatchColourFlippedPosition	= tryToMatchColourFlippedPosition,
		getPreferVictories			= preferVictories
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
		), (
			preferVictoriesTag,
			shows preferVictories
		)
	 ]

instance Data.Default.Default StandardOpeningOptions where
	def = MkStandardOpeningOptions {
		getTryToMatchMoves			= True,
		getTryToMatchViaJoiningMove		= True,
		getTryToMatchColourFlippedPosition	= True,
		getPreferVictories			= True
	}

instance HXT.XmlPickler StandardOpeningOptions where
	xpickle	= HXT.xpDefault Data.Default.def . HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d) -> mkStandardOpeningOptions a b c d,	-- Construct.
		\MkStandardOpeningOptions {
			getTryToMatchMoves			= tryToMatchMoves,
			getTryToMatchViaJoiningMove		= tryToMatchViaJoiningMove,
			getTryToMatchColourFlippedPosition	= tryToMatchColourFlippedPosition,
			getPreferVictories			= preferVictories
		} -> (tryToMatchMoves, tryToMatchViaJoiningMove, tryToMatchColourFlippedPosition, preferVictories) -- Deconstruct.
	 ) $ HXT.xp4Tuple (
		getTryToMatchMoves def `HXT.xpDefault` HXT.xpAttr tryToMatchMovesTag HXT.xpickle
	 ) (
		getTryToMatchViaJoiningMove def `HXT.xpDefault` HXT.xpAttr tryToMatchViaJoiningMoveTag HXT.xpickle
	 ) (
		getTryToMatchColourFlippedPosition def `HXT.xpDefault` HXT.xpAttr tryToMatchColourFlippedPositionTag HXT.xpickle
	 ) (
		getPreferVictories def `HXT.xpDefault` HXT.xpAttr preferVictoriesTag HXT.xpickle
	 ) where
		def	= Data.Default.def

-- | Smart constructor.
mkStandardOpeningOptions
	:: ContextualNotation.PositionHashQualifiedMoveTree.TryToMatchMoves
	-> ContextualNotation.PositionHashQualifiedMoveTree.TryToMatchViaJoiningMove
	-> ContextualNotation.PositionHashQualifiedMoveTree.TryToMatchColourFlippedPosition
	-> ContextualNotation.PositionHashQualifiedMoveTree.PreferVictories
	-> StandardOpeningOptions
mkStandardOpeningOptions tryToMatchMoves tryToMatchViaJoiningMove tryToMatchColourFlippedPosition preferVictories	= MkStandardOpeningOptions {
	getTryToMatchMoves			= tryToMatchMoves,
	getTryToMatchViaJoiningMove		= tryToMatchViaJoiningMove,
	getTryToMatchColourFlippedPosition	= tryToMatchColourFlippedPosition,
	getPreferVictories			= preferVictories
}

-- | Accessor.
getMatchSwitches :: StandardOpeningOptions -> ContextualNotation.PositionHashQualifiedMoveTree.MatchSwitches
getMatchSwitches MkStandardOpeningOptions {
	getTryToMatchMoves			= tryToMatchMoves,
	getTryToMatchViaJoiningMove		= tryToMatchViaJoiningMove,
	getTryToMatchColourFlippedPosition	= tryToMatchColourFlippedPosition
} = (tryToMatchMoves, tryToMatchViaJoiningMove, tryToMatchColourFlippedPosition)

