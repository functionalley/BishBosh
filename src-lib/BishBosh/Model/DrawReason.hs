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

	* Itemises the ways in which a game may be declared a draw.

	* Each reason corresponds to a rule in chess.
-}

module BishBosh.Model.DrawReason(
-- * Types
-- ** Data-types
	DrawReason(),
-- * Constants
	maximumConsecutiveRepeatablePlies,
	maximumConsecutiveRepeatablePositions,
	byAgreement,
--	fiftyMoveRule,
	seventyFiveMoveRule,
	insufficientMaterial,
	staleMate,
--	threeFoldRepetition,
	fiveFoldRepetition
) where

import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	BishBosh.Type.Count			as Type.Count
import qualified	Control.DeepSeq

-- | The sum-type of ways in which a game can be drawn.
data DrawReason
	= ByAgreement		-- ^ Both players have agreed to a draw.
	| FiftyMoveRule		-- ^ A draw can be claimed if fifty consecutive full /move/s have occured without any capture or any @Pawn@ being moved.
	| SeventyFiveMoveRule	-- ^ Seventy-five consecutive full /move/s have occured without either capture or @Pawn@-movement; <https://www.chessprogramming.org/Repetitions#Fide_Rule>.
	| InsufficientMaterial	-- ^ Neither player as the fire-power to force /check-mate/.
	| StaleMate		-- ^ The next player hasn't any legal moves, but isn't /in check/.
	| ThreeFoldRepetition	-- ^ A draw can be claimed if the same /position/ has been reached on any three occasions.
	| FiveFoldRepetition	-- ^ The same /position/ has been reached on five successive occasions.
	deriving (Eq, Ord, Read, Show)

instance Control.DeepSeq.NFData DrawReason where
	rnf _	= ()

instance Property.FixedMembership.FixedMembership DrawReason where
	members	= [ByAgreement, FiftyMoveRule, SeventyFiveMoveRule, InsufficientMaterial, StaleMate, ThreeFoldRepetition, FiveFoldRepetition]

-- | Constant.
byAgreement :: DrawReason
byAgreement	= ByAgreement

-- | Constant.
fiftyMoveRule :: DrawReason
fiftyMoveRule	= FiftyMoveRule

-- | Constant.
seventyFiveMoveRule :: DrawReason
seventyFiveMoveRule	= SeventyFiveMoveRule

-- | Constant.
insufficientMaterial :: DrawReason
insufficientMaterial	= InsufficientMaterial

-- | Constant.
staleMate :: DrawReason
staleMate	= StaleMate

-- | Constant.
threeFoldRepetition :: DrawReason
threeFoldRepetition	= ThreeFoldRepetition

-- | Constant.
fiveFoldRepetition :: DrawReason
fiveFoldRepetition	= FiveFoldRepetition

-- | The number of consecutive plies required to trigger a draw by the seventy-five move rule.
maximumConsecutiveRepeatablePlies :: Type.Count.NPlies
maximumConsecutiveRepeatablePlies	= Component.Move.nPliesPerMove * 75

-- | The number of consecutive repeatable positions required for a draw by the five-fold repetition rule.
maximumConsecutiveRepeatablePositions :: Type.Count.NPositions
maximumConsecutiveRepeatablePositions	= 5

