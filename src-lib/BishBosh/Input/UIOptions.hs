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

 [@DESCRIPTION@]	Defines options related to the application's user-interface.
-}

module BishBosh.Input.UIOptions(
-- * Types
-- ** Type-synonyms
	EitherNativeUIOrCECPOptions,
--	Transformation,
-- ** Data-types
	UIOptions(
--		MkUIOptions,
		getMoveNotation,
		getMaybePrintMoveTree,
		getNDecimalDigits,
		getEitherNativeUIOrCECPOptions,
		getVerbosity
	),
-- * Constants
	tag,
	printMoveTreeTag,
	nDecimalDigitsTag,
--	maxNDecimalDigits,
-- * Functions
-- ** Constructors
	mkUIOptions,
-- ** Mutators
	updateCECPFeature,
	deleteCECPFeature,
-- ** Predicates
	isCECPManualMode
) where

import			BishBosh.Data.Integral()	-- For 'HXT.XmlPickler NDecimalDigits'.
import			Control.Arrow((&&&))
import qualified	BishBosh.Data.Either		as Data.Either
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Input.CECPFeatures	as Input.CECPFeatures
import qualified	BishBosh.Input.CECPOptions	as Input.CECPOptions
import qualified	BishBosh.Input.NativeUIOptions	as Input.NativeUIOptions
import qualified	BishBosh.Input.Verbosity	as Input.Verbosity
import qualified	BishBosh.Notation.MoveNotation	as Notation.MoveNotation
import qualified	BishBosh.Property.Arboreal	as Property.Arboreal
import qualified	BishBosh.Property.ShowFloat	as Property.ShowFloat
import qualified	BishBosh.Text.ShowList		as Text.ShowList
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Default
import qualified	Data.Maybe
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT

-- | Used to qualify XML.
tag :: String
tag			= "uiOptions"

-- | Used to qualify XML.
printMoveTreeTag :: String
printMoveTreeTag	= "printMoveTree"

-- | Used to qualify XML.
nDecimalDigitsTag :: String
nDecimalDigitsTag	= "nDecimalDigits"

-- | The maximum number of decimal digits that can be represented using a double-precision floating-point number.
maxNDecimalDigits :: Property.ShowFloat.NDecimalDigits
maxNDecimalDigits	= floor $ fromIntegral (
	floatDigits (
		undefined	:: Double	-- CAVEAT: the actual type could be merely 'Float', but that's currently unknown.
	)
 ) * (logBase 10 2 :: Double)

-- | Self-documentation.
type EitherNativeUIOrCECPOptions row column	= Either (Input.NativeUIOptions.NativeUIOptions row column) Input.CECPOptions.CECPOptions

-- | Defines the application's user-interface.
data UIOptions row column = MkUIOptions {
	getMoveNotation			:: Notation.MoveNotation.MoveNotation,		-- ^ The notation used to describe /move/s.
	getMaybePrintMoveTree		:: Maybe Property.Arboreal.Depth,		-- ^ Print the move-tree to the specified depth.
	getNDecimalDigits		:: Property.ShowFloat.NDecimalDigits,		-- ^ The precision to which fractional auxiliary data is displayed.
	getEitherNativeUIOrCECPOptions	:: EitherNativeUIOrCECPOptions row column,	-- ^ When a native display is configured some additional style-parameters are required.
	getVerbosity			:: Input.Verbosity.Verbosity			-- ^ Set the threshold for ancillary information-output.
} deriving Eq

instance (
	Control.DeepSeq.NFData	column,
	Control.DeepSeq.NFData	row
 ) => Control.DeepSeq.NFData (UIOptions row column) where
	rnf MkUIOptions {
		getMoveNotation			= moveNotation,
		getMaybePrintMoveTree		= maybePrintMoveTree,
		getNDecimalDigits		= nDecimalDigits,
		getEitherNativeUIOrCECPOptions	= eitherNativeUIOrCECPOptions,
		getVerbosity			= verbosity
	} = Control.DeepSeq.rnf (
		moveNotation,
		maybePrintMoveTree,
		nDecimalDigits,
		eitherNativeUIOrCECPOptions,
		verbosity
	 )

instance (Show row, Show column) => Show (UIOptions row column) where
	showsPrec _ MkUIOptions {
		getMoveNotation			= moveNotation,
		getMaybePrintMoveTree		= maybePrintMoveTree,
		getNDecimalDigits		= nDecimalDigits,
		getEitherNativeUIOrCECPOptions	= eitherNativeUIOrCECPOptions,
		getVerbosity			= verbosity
	} = Text.ShowList.showsAssociationList' $ Data.Maybe.maybe id (
		(:) . (,) printMoveTreeTag . shows
	 ) maybePrintMoveTree [
		(
			Notation.MoveNotation.tag,
			shows moveNotation
		), (
			nDecimalDigitsTag,
			shows nDecimalDigits
		),
		either (
			(,) Input.NativeUIOptions.tag . shows
		) (
			(,) Input.CECPOptions.tag . shows
		) eitherNativeUIOrCECPOptions, (
			Input.Verbosity.tag,
			shows verbosity
		)
	 ]

instance (Num row, Num column) => Data.Default.Default (UIOptions row column) where
	def = MkUIOptions {
		getMoveNotation			= Data.Default.def,
		getMaybePrintMoveTree		= Nothing,
		getNDecimalDigits		= 3,
		getEitherNativeUIOrCECPOptions	= Left Data.Default.def,
		getVerbosity			= Data.Default.def
	}

instance (
	HXT.XmlPickler	column,
	HXT.XmlPickler	row,
	Integral	column,
	Integral	row,
	Show		column,
	Show		row
 ) => HXT.XmlPickler (UIOptions row column) where
	xpickle	= HXT.xpDefault Data.Default.def . HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d, e) -> mkUIOptions a b c d e,	-- Construct.
		\MkUIOptions {
			getMoveNotation			= moveNotation,
			getMaybePrintMoveTree		= maybePrintMoveTree,
			getNDecimalDigits		= nDecimalDigits,
			getEitherNativeUIOrCECPOptions	= eitherNativeUIOrCECPOptions,
			getVerbosity			= verbosity
		} -> (
			moveNotation,
			maybePrintMoveTree,
			nDecimalDigits,
			eitherNativeUIOrCECPOptions,
			verbosity
		)
	 ) $ HXT.xp5Tuple HXT.xpickle {-MoveNotation-} (
		HXT.xpOption $ HXT.xpAttr printMoveTreeTag HXT.xpickle {-Depth-}
	 ) (
		getNDecimalDigits def `HXT.xpDefault` HXT.xpAttr nDecimalDigitsTag HXT.xpickle {-NDecimalDigits-}
	 ) (
		getEitherNativeUIOrCECPOptions def `HXT.xpDefault` Data.Either.xpickle HXT.xpickle {-NativeUIOptions-} HXT.xpickle {-CECPOptions-}	-- N.B.: 'hxt-9.3.1.21' includes a pickler for Either.
	 ) (
		getVerbosity def `HXT.xpDefault` HXT.xpickle
	 ) where
		def	= Data.Default.def

-- | Smart constructor.
mkUIOptions
	:: Notation.MoveNotation.MoveNotation	-- ^ The chess-notation used to describe /move/s.
	-> Maybe Property.Arboreal.Depth
	-> Property.ShowFloat.NDecimalDigits	-- ^ The precision to which fractional auxiliary data is displayed.
	-> EitherNativeUIOrCECPOptions row column
	-> Input.Verbosity.Verbosity		-- ^ Set the threshold for logging.
	-> UIOptions row column
mkUIOptions moveNotation maybePrintMoveTree nDecimalDigits eitherNativeUIOrCECPOptions verbosity
	| Just depth <- maybePrintMoveTree
	, depth <= 0				= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.UIOptions.mkUIOptions:\t" . showString printMoveTreeTag . Text.ShowList.showsAssociation $ shows depth " must exceed zero."
	| nDecimalDigits < 1			= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.UIOptions.mkUIOptions:\t" . showString nDecimalDigitsTag . Text.ShowList.showsAssociation $ shows nDecimalDigits " must exceed zero."
	| nDecimalDigits > maxNDecimalDigits	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.UIOptions.mkUIOptions:\t" . showString nDecimalDigitsTag . Text.ShowList.showsAssociation . shows nDecimalDigits . showString " shouldn't exceed " $ shows maxNDecimalDigits "."
	| (
		const False `either` const True
	) eitherNativeUIOrCECPOptions && not (
		Notation.MoveNotation.isPureCoordinate moveNotation
	)					= Control.Exception.throw . Data.Exception.mkIncompatibleData . showString "BishBosh.Input.UIOptions.mkUIOptions:\t" . shows Input.CECPOptions.tag . showString " is incompatible with " . showString Notation.MoveNotation.tag . Text.ShowList.showsAssociation $ shows moveNotation "."
	| otherwise	= MkUIOptions {
		getMoveNotation			= moveNotation,
		getMaybePrintMoveTree		= maybePrintMoveTree,
		getNDecimalDigits		= nDecimalDigits,
		getEitherNativeUIOrCECPOptions	= eitherNativeUIOrCECPOptions,
		getVerbosity			= verbosity
	}

-- | Whether the chess-engine has been temporarily turned-off in order to set-up pieces.
isCECPManualMode :: UIOptions row column -> Bool
isCECPManualMode MkUIOptions { getEitherNativeUIOrCECPOptions = eitherNativeUIOrCECPOptions }	= (
	const False `either` (
		uncurry (||) . (Input.CECPOptions.getEditMode &&& Input.CECPOptions.getForceMode)
	)
 ) eitherNativeUIOrCECPOptions

-- | The type of a function used to transform 'UIOptions'.
type Transformation row column	= UIOptions row column -> UIOptions row column

-- | Mutator.
updateCECPFeature :: Input.CECPFeatures.Feature -> Transformation row column
updateCECPFeature feature uiOptions@MkUIOptions { getEitherNativeUIOrCECPOptions = eitherNativeUIOrCECPOptions }	= uiOptions {
	getEitherNativeUIOrCECPOptions	= Input.CECPOptions.updateFeature feature `fmap` eitherNativeUIOrCECPOptions
}

-- | Mutator.
deleteCECPFeature :: Input.CECPFeatures.Feature -> Transformation row column
deleteCECPFeature feature uiOptions@MkUIOptions { getEitherNativeUIOrCECPOptions = eitherNativeUIOrCECPOptions }	= uiOptions {
	getEitherNativeUIOrCECPOptions	= Input.CECPOptions.deleteFeature feature `fmap` eitherNativeUIOrCECPOptions
}

