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

 [@DESCRIPTION@]	Defines options common to native (i.e. not some separate GUI) interfaces.
-}

module BishBosh.Input.NativeUIOptions(
-- * Types
-- ** Type-synonyms
--	ScreenCoordinates,
-- ** Data-types
	NativeUIOptions(
--		MkNativeUIOptions,
		getBoardMagnification,
		getColourScheme
	),
-- * Constants
	tag,
	boardMagnificationTag,
--	nRowsTag,
--	nColumnsTag,
-- * Functions
-- ** Constructors
	mkNativeUIOptions
) where

import			Control.Arrow((***))
import qualified	BishBosh.Attribute.ColourScheme	as Attribute.ColourScheme
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Text.ShowList		as Text.ShowList
import qualified	BishBosh.Type.Length		as Type.Length
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Default
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT

-- | Used to qualify XML.
tag :: String
tag			= "nativeUIOptions"

-- | Used to qualify XML.
boardMagnificationTag :: String
boardMagnificationTag	= "boardMagnification"

-- | Used to qualify XML.
nRowsTag :: String
nRowsTag		= "nRows"

-- | Used to qualify XML.
nColumnsTag :: String
nColumnsTag		= "nColumns"

-- | The coordinates used to index the screen.
type ScreenCoordinates	= (Type.Length.Row, Type.Length.Column)

-- | Constructor.
data NativeUIOptions	= MkNativeUIOptions {
	getBoardMagnification	:: ScreenCoordinates,	-- ^ The factor by which the dimensions of the board are stretched when displayed.
	getColourScheme		:: Attribute.ColourScheme.ColourScheme
} deriving Eq

instance Control.DeepSeq.NFData NativeUIOptions where
	rnf MkNativeUIOptions {
		getBoardMagnification	= boardMagnification,
		getColourScheme		= colourScheme
	} = Control.DeepSeq.rnf (
		boardMagnification,
		colourScheme
	 )

instance Show NativeUIOptions where
	showsPrec precision MkNativeUIOptions {
		getBoardMagnification	= boardMagnification,
		getColourScheme		= colourScheme
	} = Text.ShowList.showsAssociationList' [
		(
			boardMagnificationTag,
			showsPrec precision boardMagnification
		), (
			Attribute.ColourScheme.tag,
			showsPrec precision colourScheme
		)
	 ]

instance Data.Default.Default NativeUIOptions where
	def = MkNativeUIOptions {
		getBoardMagnification	= (1, 1),
		getColourScheme		= Data.Default.def
	}

instance HXT.XmlPickler NativeUIOptions where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		uncurry mkNativeUIOptions,	-- Construct.
		\MkNativeUIOptions {
			getBoardMagnification	= boardMagnification,
			getColourScheme		= colourScheme
		} -> (
			boardMagnification,
			colourScheme
		)
	 ) $ HXT.xpPair (
		getBoardMagnification def `HXT.xpDefault` HXT.xpElem boardMagnificationTag (
			HXT.xpAttr nRowsTag HXT.xpickle `HXT.xpPair` HXT.xpAttr nColumnsTag HXT.xpickle
		)
	 ) (
		getColourScheme def `HXT.xpDefault` HXT.xpickle
	 ) where
		def	= Data.Default.def

-- | Smart constructor.
mkNativeUIOptions
	:: ScreenCoordinates	-- ^ The factor by which the dimensions of the board are stretched when displayed.
	-> Attribute.ColourScheme.ColourScheme
	-> NativeUIOptions
mkNativeUIOptions boardMagnification colourScheme
	| uncurry (||) $ (
		(< 1) *** (< 1)
	) boardMagnification	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.NativeUIOptions.mkNativeUIOptions:\t" . showString boardMagnificationTag . Text.ShowList.showsAssociation $ shows boardMagnification " must both exceed zero."
	| uncurry (||) $ (
		even *** even
	) boardMagnification	= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Input.NativeUIOptions.mkNativeUIOptions:\t" . showString boardMagnificationTag . Text.ShowList.showsAssociation $ shows boardMagnification " must both be odd."
	| otherwise		= MkNativeUIOptions {
		getBoardMagnification	= boardMagnification,
		getColourScheme		= colourScheme
	}
