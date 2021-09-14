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

 [@DESCRIPTION@]

	* Defines the data-type which represents the logical (rather than physical) colour of /piece/s & (conceptually) of the two players.

	* N.B.: conceptually different from the logical colour of squares on the board.
-}

module BishBosh.Attribute.LogicalColour(
-- * Types
-- ** Type-synonyms
	ArrayByLogicalColour,
	UArrayByLogicalColour,
-- ** Data-types
	LogicalColour(..),
-- * Constants
	tag,
	range,
	nDistinctLogicalColours,
-- * Functions
-- ** Constructor
	listArrayByLogicalColour,
	arrayByLogicalColour,
-- ** Predicates
	isBlack
--	isWhite
) where

import qualified	BishBosh.Property.ExtendedPositionDescription	as Property.ExtendedPositionDescription
import qualified	BishBosh.Property.FixedMembership		as Property.FixedMembership
import qualified	BishBosh.Property.ForsythEdwards		as Property.ForsythEdwards
import qualified	BishBosh.Property.Opposable			as Property.Opposable
import qualified	BishBosh.Type.Count				as Type.Count
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.Array.Unboxed
import qualified	Data.List.Extra
import qualified	Text.XML.HXT.Arrow.Pickle			as HXT
import qualified	Text.XML.HXT.Arrow.Pickle.Schema

-- | Used to qualify XML.
tag :: String
tag	= "logicalColour"

-- | The sum-type of /logical colour/s associated with either a piece, or (conceptually) a player.
data LogicalColour
	= Black
	| White
	deriving (
		Bounded,
		Enum,
		Eq,
		Ord,
		Read,
		Show
	)

instance Control.DeepSeq.NFData LogicalColour where
	rnf _	= ()

instance Data.Array.IArray.Ix LogicalColour where
	range (lower, upper)			= Control.Exception.assert (lower == minBound && upper == maxBound) range
	inRange (lower, upper) logicalColour	= Control.Exception.assert (logicalColour >= lower && logicalColour <= upper) True
	index (lower, upper)			= Control.Exception.assert (lower == minBound && upper == maxBound) . fromEnum

-- | The constant ascending range of /logical colour/s.
range :: [LogicalColour]
range	= [minBound, maxBound]

instance Property.FixedMembership.FixedMembership LogicalColour where
	members	= range

-- | The constant number of distinct /logical colour/s.
nDistinctLogicalColours :: Type.Count.NLogicalColours
nDistinctLogicalColours	= fromIntegral $ length range

instance HXT.XmlPickler LogicalColour where
	xpickle	= HXT.xpAttr tag . HXT.xpWrap (read, show) . HXT.xpTextDT . Text.XML.HXT.Arrow.Pickle.Schema.scEnum $ map show range

instance Property.Opposable.Opposable LogicalColour where
	getOpposite Black	= White
	getOpposite _		= Black

instance Property.ExtendedPositionDescription.ReadsEPD LogicalColour where
	readsEPD s	= case Data.List.Extra.trimStart s of
		'b' : remainder	-> [(Black, remainder)]
		'w' : remainder	-> [(White, remainder)]
		_		-> []

instance Property.ExtendedPositionDescription.ShowsEPD LogicalColour where
	showsEPD	= showChar . \case
		Black	-> 'b'
		White	-> 'w'

instance Property.ForsythEdwards.ReadsFEN LogicalColour

instance Property.ForsythEdwards.ShowsFEN LogicalColour

-- | Whether the specified /logical colour/ is @Black@.
isBlack :: LogicalColour -> Bool
{-# INLINE isBlack #-}
isBlack Black	= True
isBlack _	= False

-- | Whether the specified /logical colour/ is @White@.
isWhite :: LogicalColour -> Bool
isWhite	= not . isBlack

-- | A boxed array indexed by /logical colour/, of arbitrary elements.
type ArrayByLogicalColour	= Data.Array.IArray.Array LogicalColour

-- | An unboxed array indexed by /logical colour/, of fixed-size values.
type UArrayByLogicalColour	= Data.Array.Unboxed.UArray LogicalColour

-- | Array-constructor from an ordered list of elements.
listArrayByLogicalColour :: Data.Array.IArray.IArray a e => [e] -> a LogicalColour e
listArrayByLogicalColour	= Data.Array.IArray.listArray (minBound, maxBound)

-- | Array-constructor from an association-list.
arrayByLogicalColour :: Data.Array.IArray.IArray a e => [(LogicalColour, e)] -> a LogicalColour e
arrayByLogicalColour	= Data.Array.IArray.array (minBound, maxBound)

