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

 [@DESCRIPTION@]	Defines the physical colour used to represent each component of the board.
-}

module BishBosh.Colour.ColourScheme (
-- * Types
-- ** Data-types
	ColourScheme(
--		MkColourScheme,
		getDarkPieceColour,
		getLightPieceColour,
		getDarkSquareColour,
		getLightSquareColour
	),
-- * Constants
	tag,
--	darkSquareColourTag,
--	lightSquareColourTag,
--	darkPieceColourTag,
--	lightPieceColourTag,
-- * Functions
-- ** Constructor
--	mkColourScheme
) where

import qualified	BishBosh.Colour.PhysicalColour	as Colour.PhysicalColour
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Property.Opposable	as Property.Opposable
import qualified	BishBosh.Text.ShowList		as Text.ShowList
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Default
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT

-- | Used to qualify XML.
tag :: String
tag	= "colourScheme"

darkPieceColourTag, darkSquareColourTag, lightPieceColourTag, lightSquareColourTag :: String
[darkPieceColourTag, darkSquareColourTag, lightPieceColourTag, lightSquareColourTag]	= [showString brightness $ showString component "Colour" | brightness <- ["dark", "light"], component <- ["Piece", "Square"]] -- List-comprehension.

-- | Defines the command-line options.
data ColourScheme	= MkColourScheme {
	getDarkPieceColour	:: Colour.PhysicalColour.PhysicalColour,	-- ^ The physical colour of the dark pieces.
	getLightPieceColour	:: Colour.PhysicalColour.PhysicalColour,	-- ^ The physical colour of the light pieces.
	getDarkSquareColour	:: Colour.PhysicalColour.PhysicalColour,	-- ^ The physical colour of the dark squares of the board.
	getLightSquareColour	:: Colour.PhysicalColour.PhysicalColour		-- ^ The physical colour of the light squares of the board.
} deriving Eq

instance Control.DeepSeq.NFData ColourScheme where
	rnf MkColourScheme {
		getDarkPieceColour	= darkPieceColour,
		getLightPieceColour	= lightPieceColour,
		getDarkSquareColour	= darkSquareColour,
		getLightSquareColour	= lightSquareColour
	} = Control.DeepSeq.rnf (
		darkPieceColour,
		lightPieceColour,
		darkSquareColour,
		lightSquareColour
	 )

instance Show ColourScheme where
	showsPrec _ MkColourScheme {
		getDarkPieceColour	= darkPieceColour,
		getLightPieceColour	= lightPieceColour,
		getDarkSquareColour	= darkSquareColour,
		getLightSquareColour	= lightSquareColour
	} = Text.ShowList.showsAssociationList' $ map (Control.Arrow.second shows) [
		(
			darkPieceColourTag,
			darkPieceColour
		), (
			lightPieceColourTag,
			lightPieceColour
		), (
			darkSquareColourTag,
			darkSquareColour
		), (
			lightSquareColourTag,
			lightSquareColour
		)
	 ]

instance Data.Default.Default ColourScheme where
	def = MkColourScheme {
		getDarkPieceColour	= Colour.PhysicalColour.blue,
		getLightPieceColour	= Property.Opposable.getOpposite $ getDarkPieceColour Data.Default.def,
		getDarkSquareColour	= Colour.PhysicalColour.black,
		getLightSquareColour	= Property.Opposable.getOpposite $ getDarkSquareColour Data.Default.def
	}

instance HXT.XmlPickler ColourScheme where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d) -> mkColourScheme a b c d,	-- Construct.
		\MkColourScheme {
			getDarkPieceColour	= darkPieceColour,
			getLightPieceColour	= lightPieceColour,
			getDarkSquareColour	= darkSquareColour,
			getLightSquareColour	= lightSquareColour
		} -> (
			darkPieceColour,
			lightPieceColour,
			darkSquareColour,
			lightSquareColour
		) -- Deconstruct.
	 ) $ HXT.xp4Tuple (
		getDarkPieceColour def `HXT.xpDefault` HXT.xpAttr darkPieceColourTag HXT.xpickle
	 ) (
		getLightPieceColour def `HXT.xpDefault` HXT.xpAttr lightPieceColourTag HXT.xpickle
	 ) (
		getDarkSquareColour def `HXT.xpDefault` HXT.xpAttr darkSquareColourTag HXT.xpickle
	 ) (
		getLightSquareColour def `HXT.xpDefault` HXT.xpAttr lightSquareColourTag HXT.xpickle
	 ) where
		def :: ColourScheme
		def	= Data.Default.def

-- | Smart constructor.
mkColourScheme
	:: Colour.PhysicalColour.PhysicalColour	-- ^ Dark piece.
	-> Colour.PhysicalColour.PhysicalColour	-- ^ Light piece.
	-> Colour.PhysicalColour.PhysicalColour	-- ^ Dark square.
	-> Colour.PhysicalColour.PhysicalColour	-- ^ Light square.
	-> ColourScheme
mkColourScheme darkPieceColour lightPieceColour darkSquareColour lightSquareColour
	| darkPieceColour `elem` bgColours		= Control.Exception.throw . Data.Exception.mkIncompatibleData . showString "BishBosh.Colour.ColourScheme.mkColourScheme:\t" . showString darkPieceColourTag . Text.ShowList.showsAssociation . shows darkPieceColour . showString " must differ from the physical colour of both squares; " $ shows bgColours "."
	| lightPieceColour `elem` bgColours		= Control.Exception.throw . Data.Exception.mkIncompatibleData . showString "BishBosh.Colour.ColourScheme.mkColourScheme:\t" . showString lightPieceColourTag . Text.ShowList.showsAssociation . shows lightPieceColour . showString " must differ from the physical colour of both squares; " $ shows bgColours "."
	| darkSquareColour == lightSquareColour		= Control.Exception.throw . Data.Exception.mkIncompatibleData . showString "BishBosh.Colour.ColourScheme.mkColourScheme:\tthe physical colours of " . shows lightSquareColourTag . showString " & " $ shows darkSquareColourTag ", must differ."
	| otherwise					= MkColourScheme {
		getDarkPieceColour	= darkPieceColour,
		getLightPieceColour	= lightPieceColour,
		getDarkSquareColour	= darkSquareColour,
		getLightSquareColour	= lightSquareColour
	}
	where
		bgColours	= [darkSquareColour, lightSquareColour]

