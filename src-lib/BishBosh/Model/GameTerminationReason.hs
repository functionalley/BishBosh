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

	* Itemises the various reasons for terminating a game.

	* Each reason corresponds to a rule in chess.
-}

module BishBosh.Model.GameTerminationReason(
-- * Types
-- ** Data-types
	GameTerminationReason(),
-- * Function
	toResult,
-- ** Constructors
	mkCheckMate,
	mkResignation,
	mkDraw,
-- ** Predicates
	isCheckMateBy,
	isCheckMate,
	isResignation,
	isDraw,
	isDrawByInsufficientMaterial,
	isStaleMate
) where

import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Model.DrawReason		as Model.DrawReason
import qualified	BishBosh.Model.Result			as Model.Result
import qualified	BishBosh.Property.Opposable		as Property.Opposable
import qualified	Control.DeepSeq

-- | The ways in which a game can legally be terminated.
data GameTerminationReason
	= CheckMateOf Attribute.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the /check-mated/ player.
	| ResignationBy Attribute.LogicalColour.LogicalColour	-- ^ The /logical colour/ of the player who resigned.
	| Draw Model.DrawReason.DrawReason
	deriving (Eq, Read, Show)

instance Control.DeepSeq.NFData GameTerminationReason where
	rnf (CheckMateOf logicalColour)		= Control.DeepSeq.rnf logicalColour
	rnf (ResignationBy logicalColour)	= Control.DeepSeq.rnf logicalColour
	rnf (Draw drawReason)			= Control.DeepSeq.rnf drawReason

instance Property.Opposable.Opposable GameTerminationReason where
	getOpposite (CheckMateOf logicalColour)		= CheckMateOf $ Property.Opposable.getOpposite logicalColour
	getOpposite (ResignationBy logicalColour)	= ResignationBy $ Property.Opposable.getOpposite logicalColour
	getOpposite draw				= draw

-- | Convert to a /result/.
toResult :: GameTerminationReason -> Model.Result.Result
toResult gameTerminationReason	= Property.Opposable.getOpposite . Model.Result.mkResult $ case gameTerminationReason of
	CheckMateOf logicalColour	-> Just logicalColour
	ResignationBy logicalColour	-> Just logicalColour
	Draw _				-> Nothing

-- | Constructor.
mkCheckMate :: Attribute.LogicalColour.LogicalColour -> GameTerminationReason
mkCheckMate	= CheckMateOf

-- | Constructor.
mkResignation :: Attribute.LogicalColour.LogicalColour -> GameTerminationReason
mkResignation	= ResignationBy

-- | Constructor.
mkDraw :: Model.DrawReason.DrawReason -> GameTerminationReason
mkDraw	= Draw

-- | Whether the game was won by the specified player.
isCheckMateBy :: Attribute.LogicalColour.LogicalColour -> GameTerminationReason -> Bool
isCheckMateBy logicalColour (CheckMateOf logicalColour')	= logicalColour /= logicalColour'
isCheckMateBy _ _						= False

-- | Whether the game terminated in check-mate.
isCheckMate :: GameTerminationReason -> Bool
isCheckMate (CheckMateOf _)	= True
isCheckMate _			= False

-- | Whether the game was resigned.
isResignation :: GameTerminationReason -> Bool
isResignation (ResignationBy _)	= True
isResignation _			= False

-- | Whether the game was drawn.
isDraw :: GameTerminationReason -> Bool
isDraw (Draw _)	= True
isDraw _	= False

-- | Predicate.
isDrawByInsufficientMaterial :: GameTerminationReason -> Bool
isDrawByInsufficientMaterial (Draw draw)	= draw == Model.DrawReason.insufficientMaterial
isDrawByInsufficientMaterial _			= False

-- | Predicate.
isStaleMate :: GameTerminationReason -> Bool
isStaleMate (Draw draw)	= draw == Model.DrawReason.staleMate
isStaleMate _		= False

