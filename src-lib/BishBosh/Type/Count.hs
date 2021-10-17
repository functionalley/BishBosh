{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
{-
	Copyright (C) 2021 Dr. Alistair Ward

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

	* Defines distinct types for various conceptually different countable quantities to prevent accidental conflation.

	* Nothing but the type is exported, facilitating reversion to unwrapped types.
-}

module BishBosh.Type.Count(
-- * Types
-- ** Type-synonyms
--	Base,
-- ** Data-types
	NCoordinates,
	NDecimalDigits,
	NDirections,
	NGames,
	NLogicalColours,
	NMoves,
	NPieces,
	NPlies,
	NPositions,
	NRanks,
	NSeconds,
) where

#ifdef USE_NEWTYPE_WRAPPERS
import qualified	Control.DeepSeq
import qualified	Data.Array.IArray
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
#endif

-- | The private type which is wrapped by various countable data-types.
type Base	= Int

-- | A number of (board) coordinates.
#ifdef USE_NEWTYPE_WRAPPERS
newtype NCoordinates	= MkNCoordinates Base deriving (Enum, Eq, Integral, Num, Ord, Real)

instance Show NCoordinates where
	showsPrec precedence (MkNCoordinates n)	= showsPrec precedence n
#else
type NCoordinates	= Base
#endif

-- | A number of decimal digits.
#ifdef USE_NEWTYPE_WRAPPERS
newtype NDecimalDigits	= MkNDecimalDigits Base deriving (Control.DeepSeq.NFData, Enum, Eq, HXT.XmlPickler, Integral, Num, Ord, Real)

instance Show NDecimalDigits where
	showsPrec precedence (MkNDecimalDigits n)	= showsPrec precedence n
#else
type NDecimalDigits	= Base
#endif

-- | A number of directions.
#ifdef USE_NEWTYPE_WRAPPERS
newtype NDirections	= MkNDirections Base deriving (Enum, Eq, Integral, Num, Ord, Real)
#else
type NDirections	= Base
#endif

-- | A number of /game/s.
#ifdef USE_NEWTYPE_WRAPPERS
newtype NGames	= MkNGames Base deriving (Control.DeepSeq.NFData, Enum, Eq, HXT.XmlPickler, Integral, Num, Ord, Real)

instance Show NGames where
	showsPrec precedence (MkNGames n)	= showsPrec precedence n
#else
type NGames	= Base
#endif

-- | A number of /logical colour/s.
#ifdef USE_NEWTYPE_WRAPPERS
newtype NLogicalColours	= MkNLogicalColours Base deriving (Enum, Eq, Integral, Num, Ord, Real)

instance Show NLogicalColours where
	showsPrec precedence (MkNLogicalColours n)	= showsPrec precedence n
#else
type NLogicalColours	= Base
#endif

-- | A number of /move/s, i.e. each player takes one turn.
#ifdef USE_NEWTYPE_WRAPPERS
newtype NMoves	= MkNMoves Base deriving (Control.DeepSeq.NFData, Enum, Eq, HXT.XmlPickler, Integral, Num, Ord, Real)

instance Show NMoves where
	showsPrec precedence (MkNMoves n)	= showsPrec precedence n
#else
type NMoves	= Base
#endif

-- | A number of /piece/s.
#ifdef USE_NEWTYPE_WRAPPERS
newtype NPieces	= MkNPieces Base deriving (Control.DeepSeq.NFData, Data.Array.IArray.Ix, Enum, Eq, Integral, Num, Ord, Real)

instance Show NPieces where
	showsPrec precedence (MkNPieces n)	= showsPrec precedence n
#else
type NPieces	= Base
#endif

{- |
	* A number of /plies/.

	* CAVEAT: conceptually similar to /NPositions/ in that a ply (half a move) defines a step down the move-tree to a new position; the difference is that positions don't necessarily relate to a consecutive sequence resulting from a game.

	* CAVEAT: arguably the same type as /NMoves/, just double the value.
-}
#ifdef USE_NEWTYPE_WRAPPERS
newtype NPlies	= MkNPlies Base deriving (Control.DeepSeq.NFData, Enum, Eq, HXT.XmlPickler, Integral, Num, Ord, Read, Real)

instance Show NPlies where
	showsPrec precedence (MkNPlies n)	= showsPrec precedence n
#else
type NPlies	= Base
#endif

-- | A number of /position/s.
#ifdef USE_NEWTYPE_WRAPPERS
newtype NPositions	= MkNPositions Base deriving (Control.DeepSeq.NFData, Enum, Eq, Integral, Num, Ord, Real)

instance Show NPositions where
	showsPrec precedence (MkNPositions n)	= showsPrec precedence n
#else
type NPositions	= Base
#endif

-- | A number of /rank/s.
#ifdef USE_NEWTYPE_WRAPPERS
newtype NRanks	= MkNRanks Base deriving (Enum, Eq, Integral, Num, Ord, Real)
#else
type NRanks	= Base
#endif

-- | A number of seconds.
#ifdef USE_NEWTYPE_WRAPPERS
newtype NSeconds	= MkNSeconds Base deriving (Enum, Eq, Integral, Num, Ord, Real)

instance Show NSeconds where
	showsPrec precedence (MkNSeconds n)	= showsPrec precedence n
#else
type NSeconds		= Base
#endif

