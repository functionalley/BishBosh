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

 [@DESCRIPTION@]	Arbitrary operations on foldable data.
-}

module BishBosh.Data.Foldable(
	findDuplicates
) where

import qualified	Data.Foldable
import qualified	Data.Map.Strict

-- | Returns a unique instance of any item which has been specified more than once.
findDuplicates :: (Foldable foldable, Ord a) => foldable a -> [a]
findDuplicates	= Data.Map.Strict.keys . Data.Map.Strict.filter (> 1) . Data.Foldable.foldr (
	flip (Data.Map.Strict.insertWith $ const succ) (1 :: Int)	-- Count instances.
 ) Data.Map.Strict.empty

