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

 [@DESCRIPTION@]	The ordered sequence of /turn/s alternately made by the players of each /logical colour/.
-}

module BishBosh.State.TurnsByLogicalColour(
-- * Types
-- ** Type-synonyms
--	Transformation
-- ** Data-types
	TurnsByLogicalColour(
--		MkTurnsByLogicalColour,
--		getTurnsByLogicalColour,
		getNPlies
	),
-- * Functions
	inferNextLogicalColour,
	countPlies,
	deriveMoveNumber,
	dereference,
-- ** Constructors
	fromAssocs,
-- ** Mutators
	update,
	prepend
) where

import			Control.Arrow((&&&), (***))
import			Data.Array.IArray((!), (//))
import qualified	BishBosh.Colour.LogicalColour	as Colour.LogicalColour
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Property.Empty		as Property.Empty
import qualified	BishBosh.Property.Null		as Property.Null
import qualified	BishBosh.Property.Opposable	as Property.Opposable
import qualified	BishBosh.Property.Reflectable	as Property.Reflectable
import qualified	BishBosh.Type.Count		as Type.Count
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.Default
import qualified	Data.Foldable
import qualified	Data.List.Extra

-- | The type used to hold a record of each player's /turn/s.
data TurnsByLogicalColour turn	= MkTurnsByLogicalColour {
	getTurnsByLogicalColour	:: Colour.LogicalColour.ArrayByLogicalColour [turn],
	getNPlies		:: Type.Count.NPlies	-- ^ The total number of plies applied to the game for both players; this could alternatively be derived using 'countPlies'.
}

instance Eq turn => Eq (TurnsByLogicalColour turn) where
	MkTurnsByLogicalColour { getTurnsByLogicalColour = aL } == MkTurnsByLogicalColour { getTurnsByLogicalColour = aR }	= aL == aR

instance (Read turn, Show turn) => Read (TurnsByLogicalColour turn) where
	readsPrec precedence s	= Control.Arrow.first fromAssocs `map` readsPrec precedence s

instance Show turn => Show (TurnsByLogicalColour turn) where
	showsPrec precedence MkTurnsByLogicalColour { getTurnsByLogicalColour = byLogicalColour }	= showsPrec precedence $ Data.Array.IArray.assocs byLogicalColour

instance Control.DeepSeq.NFData turn => Control.DeepSeq.NFData (TurnsByLogicalColour turn) where
	rnf MkTurnsByLogicalColour { getTurnsByLogicalColour = byLogicalColour }	= Control.DeepSeq.rnf byLogicalColour

instance Data.Default.Default (TurnsByLogicalColour turn) where
	def = MkTurnsByLogicalColour {
		getTurnsByLogicalColour	= Colour.LogicalColour.listArrayByLogicalColour $ repeat [],
		getNPlies		= 0
	}

instance Property.Empty.Empty (TurnsByLogicalColour turn) where
	empty	= Data.Default.def

instance Property.Null.Null (TurnsByLogicalColour turn) where
	isNull MkTurnsByLogicalColour { getNPlies = 0 }	= True
	isNull _					= False

instance Property.Reflectable.ReflectableOnX turn => Property.Reflectable.ReflectableOnX (TurnsByLogicalColour turn) where
	reflectOnX turnsByLogicalColour@MkTurnsByLogicalColour { getTurnsByLogicalColour = byLogicalColour }	= turnsByLogicalColour {
		getTurnsByLogicalColour	= Colour.LogicalColour.arrayByLogicalColour . map (
			Property.Opposable.getOpposite {-logical colour-} *** Property.Reflectable.reflectOnX {-[turn]-}
		) $ Data.Array.IArray.assocs byLogicalColour
	 }

-- | Smart constructor.
fromAssocs :: Show turn => [(Colour.LogicalColour.LogicalColour, [turn])] -> TurnsByLogicalColour turn
fromAssocs assocs
	| fromIntegral (
		length assocs
	) /= Colour.LogicalColour.nDistinctLogicalColours		= Control.Exception.throw . Data.Exception.mkInsufficientData . showString "BishBosh.State.TurnsByLogicalColour.fromAssocs:\tboth logical colours must be defined; " $ shows assocs "."
	| Data.List.Extra.anySame $ map fst {-logicalColour-} assocs	= Control.Exception.throw . Data.Exception.mkDuplicateData . showString "BishBosh.State.TurnsByLogicalColour.fromAssocs:\tduplicates specified; " $ shows assocs "."
	| (> 1) . abs {-allow for Property.Reflectable.reflectOnX-} . uncurry (-) $ (
		length . (! Colour.LogicalColour.White) &&& length . (! Colour.LogicalColour.Black)
	) byLogicalColour						= Control.Exception.throw . Data.Exception.mkIncompatibleData . showString "BishBosh.State.TurnsByLogicalColour.fromAssocs:\tany difference in the number of turns taken by each player, can't exceed one " $ shows assocs "."
	| otherwise							= turnsByLogicalColour
	where
		byLogicalColour		= Colour.LogicalColour.arrayByLogicalColour assocs
		turnsByLogicalColour	= MkTurnsByLogicalColour {
			getTurnsByLogicalColour	= byLogicalColour,
			getNPlies		= countPlies turnsByLogicalColour	-- Infer.
		}

{- |
	* Derive the /logical colour/ of the next player to move.

	* CAVEAT: the result can't be guaranteed if 'Property.Reflectable.reflectOnX' has been called.
-}
inferNextLogicalColour :: TurnsByLogicalColour turn -> Colour.LogicalColour.LogicalColour
inferNextLogicalColour MkTurnsByLogicalColour { getNPlies = nPlies }
	| even nPlies	= Colour.LogicalColour.White
	| otherwise	= Colour.LogicalColour.Black

{- |
	* Count the total number of plies, regardless of the player.

	* CAVEAT: 'getNPlies' is more efficient.
-}
countPlies :: TurnsByLogicalColour turn -> Type.Count.NPlies
countPlies MkTurnsByLogicalColour { getTurnsByLogicalColour = byLogicalColour }	= fromIntegral $ Data.Foldable.foldl' (\acc -> (+ acc) . length) 0 byLogicalColour

-- | Derive the move-number, as used in PGN.
deriveMoveNumber :: TurnsByLogicalColour turn -> Type.Count.NMoves
deriveMoveNumber MkTurnsByLogicalColour { getNPlies = nPlies }	= succ {-index from 1-} $! fromIntegral nPlies `div` 2

-- | Dereference.
dereference :: TurnsByLogicalColour turn -> Colour.LogicalColour.LogicalColour -> [turn]
dereference MkTurnsByLogicalColour { getTurnsByLogicalColour = byLogicalColour } logicalColour	= byLogicalColour ! logicalColour

-- | Self-documentation.
type Transformation turn	= TurnsByLogicalColour turn -> TurnsByLogicalColour turn

{- |
	* Update the specified logical colours.

	* CAVEAT: obliterates any incumbent data for the specified logical colours.
-}
update :: [(Colour.LogicalColour.LogicalColour, [turn])] -> Transformation turn
update assocs MkTurnsByLogicalColour { getTurnsByLogicalColour = byLogicalColour }	= turnsByLogicalColour where
	turnsByLogicalColour	= MkTurnsByLogicalColour {
		getTurnsByLogicalColour	= byLogicalColour // assocs,
		getNPlies		= countPlies turnsByLogicalColour	-- Infer.
	}

-- | Prepend the specified /turn/.
prepend :: Colour.LogicalColour.LogicalColour -> turn -> Transformation turn
prepend logicalColour turn MkTurnsByLogicalColour {
	getTurnsByLogicalColour	= byLogicalColour,
	getNPlies		= nPlies
} = MkTurnsByLogicalColour {
	getTurnsByLogicalColour	= byLogicalColour // [
		(
			logicalColour,
			turn : byLogicalColour ! logicalColour
		) -- Pair.
	], -- Singleton.
	getNPlies	= succ nPlies
}

