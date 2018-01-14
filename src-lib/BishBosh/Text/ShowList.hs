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

 [@DESCRIPTION@]	Exports functions to facilitate unification of the style of output text.
-}

module BishBosh.Text.ShowList(
-- * Constants
	showsInfoPrefix,
	showsWarningPrefix,
	showsErrorPrefix,
	showsAssociation,
-- * Functions
	capitaliseInitial,
	showsSeparator,
	showsDelimitedList,
	showsUnterminatedList,
	showsFormattedList,
	showsFormattedList',
	showsAssociationList,
	showsAssociationList',
	splitOn
) where

import qualified	Data.Char
import qualified	Data.List

-- | Used to qualify output.
showsInfoPrefix :: ShowS
showsInfoPrefix		= showString "INFO:\t"

-- | Used to qualify output.
showsWarningPrefix :: ShowS
showsWarningPrefix	= showString "WARNING:\t"

-- | Used to qualify output.
showsErrorPrefix :: ShowS
showsErrorPrefix	= showString "ERROR:\t"

-- | Used to separate an identifier & the it's value.
showsAssociation :: ShowS
showsAssociation	= showString " = "

-- | Capitalise the initial letter of the specified string.
capitaliseInitial :: ShowS
capitaliseInitial (c : cs)	= Data.Char.toUpper c : cs
capitaliseInitial _		= []

-- | Used to separate the items of a list.
showsSeparator :: ShowS
showsSeparator	= showString ", "

-- | Shows a list with the specified delimiters.
showsDelimitedList
	:: ShowS	-- ^ The list-separator.
	-> ShowS	-- ^ Left delimiter.
	-> ShowS	-- ^ Right delimiter.
	-> [ShowS]
	-> ShowS
showsDelimitedList separator lDelimiter rDelimiter	= foldr (.) rDelimiter . (lDelimiter :) . Data.List.intersperse separator

-- | Shows a list without terminal delimiters.
showsUnterminatedList :: [ShowS] -> ShowS
showsUnterminatedList	= showsDelimitedList showsSeparator id id

-- | Formats & shows a list with standard terminal delimiters.
showsFormattedList
	:: ShowS	-- ^ The list-separator.
	-> (a -> ShowS)	-- ^ Format the list-elements.
	-> [a]		-- ^ An arbitrary list of items.
	-> ShowS
showsFormattedList separator f	= showsDelimitedList separator (showChar '[') (showChar ']') . map f

-- | Formats & shows a list with standard delimiters.
showsFormattedList'
	:: (a -> ShowS)	-- ^ Format the list-elements.
	-> [a]		-- ^ An arbitrary list of items.
	-> ShowS
showsFormattedList'	= showsFormattedList showsSeparator

-- | Shows an association-list with standard terminal delimiters.
showsAssociationList
	:: ShowS	-- ^ The list-separator.
	-> [(String, ShowS)]
	-> ShowS
showsAssociationList separator	= showsDelimitedList separator (showChar '{') (showChar '}') . map (
	\(k, v) -> showString k . showsAssociation . v
 )

-- | Shows an association-list with standard delimiters.
showsAssociationList' :: [(String, ShowS)] -> ShowS
showsAssociationList'	= showsAssociationList showsSeparator

{- |
	Split the specified list, using the predicate to identify the separator.

	CAVEAT: the separator isn't included in the results.
-}
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ []		= []
splitOn predicate l	= case break predicate l of
	(chunk, _ : l')	-> chunk : splitOn predicate l'	-- Recurse.
	(chunk, [])	-> [chunk]

