{-# LANGUAGE CPP #-}
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

	* Defines suitable concrete types with which to specialise miscellaneous type-parameters.

	* CAVEAT: use of narrow numeric types, results in marginally slower performance without any reduction in space-requirements.
-}

module BishBosh.Type.Crypto(
-- * Types
-- ** Type-synonyms
	PositionHash,
) where

import qualified	Data.Word

-- | The type of the hash used to uniquely represent a /position/.
type PositionHash	=
#ifdef USE_NARROW_NUMBERS
	Data.Word.Word32	-- CAVEAT: hash-collisions become almost inevitable after @ sqrt bits @ trials.
#else
	Data.Word.Word
#endif
