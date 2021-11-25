{-# LANGUAGE LambdaCase #-}
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

 [@DESCRIPTION@]	Defines the result of a /game/.
-}

module BishBosh.Rule.Result(
-- * Types
-- ** Data-types
	Result(
--		VictoryBy,
--		Draw
	),
-- * Function
	findMaybeVictor,
-- ** Constructor
	mkResult,
-- ** Predicates
	isDraw
) where

import qualified	BishBosh.Colour.LogicalColour		as Colour.LogicalColour
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	Control.DeepSeq
import qualified	Data.List.Extra

-- | The sum-type of ways in which a game can legally be terminated.
data Result
	= VictoryBy Colour.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the victor.
	| Draw
	deriving Eq

instance Control.DeepSeq.NFData Result where
	rnf (VictoryBy logicalColour)	= Control.DeepSeq.rnf logicalColour
	rnf Draw			= ()

-- | Convert a game-termination reason into PGN's @Result@ field; <https://www.chessclub.com/help/pgn-spec>.
instance Show Result where
	showsPrec _	= (
		\(showsWhiteResult, showsBlackResult) -> showsWhiteResult . showChar '-' . showsBlackResult
	 ) . \case
		VictoryBy Colour.LogicalColour.Black	-> (lose, win)
		VictoryBy _				-> (win, lose)
		_					-> (draw, draw)
		where
			lose	= showChar '0'
			win	= showChar '1'
			draw	= showString "1/2"

instance Read Result where
	readsPrec _ s	= case Data.List.Extra.trimStart s of
		'0' : '-' : '1' : remainder				-> [(VictoryBy Colour.LogicalColour.Black, remainder)]
		'1' : '-' : '0' : remainder				-> [(VictoryBy Colour.LogicalColour.White, remainder)]
		'1' : '/' : '2' : '-' : '1' : '/' : '2' : remainder	-> [(Draw, remainder)]
		_							-> []	-- No Parse.

instance Property.Opposable.Opposable Result where
	getOpposite (VictoryBy logicalColour)	= VictoryBy $ Property.Opposable.getOpposite logicalColour
	getOpposite _				= Draw

instance Property.FixedMembership.FixedMembership Result where
	members	= Draw : map VictoryBy Property.FixedMembership.members

-- | Constructor.
mkResult :: Maybe Colour.LogicalColour.LogicalColour -> Result
mkResult (Just logicalColour)	= VictoryBy logicalColour
mkResult _			= Draw

-- | Whether the game was drawn.
isDraw :: Result -> Bool
isDraw Draw	= True
isDraw _	= False

-- | Find any winner.
findMaybeVictor :: Result -> Maybe Colour.LogicalColour.LogicalColour
findMaybeVictor (VictoryBy logicalColour)	= Just logicalColour
findMaybeVictor _				= Nothing

