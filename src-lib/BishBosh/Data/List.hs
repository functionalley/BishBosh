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

	* List-operations.

	* CAVEAT: import qualified to avoid clash with 'Data.List'.
-}

module BishBosh.Data.List(
-- * Functions
	findClosest,
	unabbreviate
) where

import			Control.Arrow((&&&))
import qualified	Data.List
import qualified	Data.List.Extra
import qualified	ToolShed.Data.List

{- |
	* Find the closest to the single item supplied, from the supplied choices.

	* All choices of equal proximity are returned.

	* CAVEAT: when applied to Strings, case-sensitivity should be considered by the caller.
-}
findClosest
	:: Eq a
	=> [a]		-- ^ Item.
	-> [[a]]	-- ^ Choices.
	-> [[a]]
findClosest _ []	= []
findClosest s choices	= case Data.List.Extra.groupSort $ map (
	(\d -> d :: Rational) . negate {-largest Jaro-distance is the closest-} . ToolShed.Data.List.measureJaroDistance . (,) s &&& id
 ) choices of
	(_, x) : _	-> x
	_		-> []

-- | Replace the abbreviated item with any item from the specified list, of which it's an unambiguously prefix.
unabbreviate
	:: Eq a
	=> ([a] -> [a])	-- ^ Translate.
	-> [[a]]	-- ^ Choices
	-> [a]		-- ^ Abbreviation.
	-> [a]
unabbreviate f choices l	= case filter (
	Data.List.isPrefixOf (f l) . f
 ) choices of
	[x]	-> x	-- Replace with unambiguous completion.
	_	-> l	-- Don't replace anything.


