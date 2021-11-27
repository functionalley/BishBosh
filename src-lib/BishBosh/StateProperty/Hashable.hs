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

	* Defines the random-numbers required to construct a hash of a chess-position.

	* Facilitates the construction of a hash from arbitrary data.
-}

module BishBosh.StateProperty.Hashable(
-- * Type-classes
	Hashable(listRandoms),
-- * Constants
--	combiningOp,
-- * Functions
	hash,
	combine
) where

import			Control.Arrow((***))
import qualified	BishBosh.Component.Zobrist	as Component.Zobrist
import qualified	Data.Bits
import qualified	Data.List
import qualified	Data.Maybe

-- | An interface to which hashable data can conform.
class Hashable hashable where
	listRandoms	:: Component.Zobrist.Zobrist positionHash -> hashable -> [positionHash]

instance (Hashable l, Hashable r) => Hashable (l, r) where
	listRandoms zobrist	= uncurry (++) . (listRandoms zobrist *** listRandoms zobrist)

instance (Hashable f, Hashable s, Hashable t) => Hashable (f, s, t) where
	listRandoms zobrist (f, s, t)	= listRandoms zobrist f ++ listRandoms zobrist s ++ listRandoms zobrist t

instance Hashable a => Hashable [a] where
	listRandoms zobrist	= concatMap $ listRandoms zobrist

instance Hashable a => Hashable (Maybe a) where
	listRandoms zobrist	= Data.Maybe.maybe [] $ listRandoms zobrist

-- | The operator used when combining random numbers to compose a hash.
combiningOp :: Data.Bits.Bits positionHash => positionHash -> positionHash -> positionHash
combiningOp	= Data.Bits.xor

-- | Resolve a hashable into a hash.
hash :: (
	Data.Bits.Bits	positionHash,
	Hashable	hashable
 )
	=> Component.Zobrist.Zobrist positionHash
	-> hashable
	-> positionHash
hash zobrist	= Data.List.foldl1' combiningOp . listRandoms zobrist

-- | Include a list of random numbers in the hash.
combine :: Data.Bits.Bits positionHash => positionHash -> [positionHash] -> positionHash
combine	= Data.List.foldl' combiningOp

