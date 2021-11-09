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

import			Control.Arrow((&&&), (|||))
import qualified	BishBosh.Data.Either		as Data.Either
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Input.CECPFeatures	as Input.CECPFeatures
import qualified	BishBosh.Input.CECPOptions	as Input.CECPOptions
import qualified	BishBosh.Input.NativeUIOptions	as Input.NativeUIOptions
import qualified	BishBosh.Input.Verbosity	as Input.Verbosity
import qualified	BishBosh.Notation.MoveNotation	as Notation.MoveNotation
import qualified	BishBosh.Property.Arboreal	as Property.Arboreal
import qualified	BishBosh.Text.ShowList		as Text.ShowList
import qualified	BishBosh.Type.Count		as Type.Count
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
maxNDecimalDigits :: Type.Count.NDecimalDigits
maxNDecimalDigits	= floor $ fromIntegral (
	floatDigits (
		undefined	:: Double	-- CAVEAT: the actual type could be merely 'Float', but that's currently unknown.
	)
 ) * (logBase 10 2 :: Double)

-- | Self-documentation.
type EitherNativeUIOrCECPOptions	= Either Input.NativeUIOptions.NativeUIOptions Input.CECPOptions.CECPOptions

-- | Defines the application's user-interface.
data UIOptions = MkUIOptions {
	getMoveNotation			:: Notation.MoveNotation.MoveNotation,	-- ^ The notation used to describe /move/s.
	getMaybePrintMoveTree		:: Maybe Property.Arboreal.Depth,	-- ^ Print the move-tree to the specified depth.
	getNDecimalDigits		:: Type.Count.NDecimalDigits,		-- ^ The precision to which fractional auxiliary data is displayed.
	getEitherNativeUIOrCECPOptions	:: EitherNativeUIOrCECPOptions,		-- ^ When a native display is configured some additional style-parameters are required.
	getVerbosity			:: Input.Verbosity.Verbosity		-- ^ Set the threshold for ancillary information-output.
} deriving Eq

instance Control.DeepSeq.NFData UIOptions where
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

instance Show UIOptions where
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
		(,) Input.NativeUIOptions.tag . shows ||| (,) Input.CECPOptions.tag . shows $ eitherNativeUIOrCECPOptions,
		(
			Input.Verbosity.tag,
			shows verbosity
		)
	 ]

instance Data.Default.Default UIOptions where
	def = MkUIOptions {
		getMoveNotation			= Data.Default.def,
		getMaybePrintMoveTree		= Nothing,
		getNDecimalDigits		= 3,
		getEitherNativeUIOrCECPOptions	= Left Data.Default.def,
		getVerbosity			= Data.Default.def
	}

instance HXT.XmlPickler UIOptions where
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
	-> Type.Count.NDecimalDigits		-- ^ The precision to which fractional auxiliary data is displayed.
	-> EitherNativeUIOrCECPOptions
	-> Input.Verbosity.Verbosity		-- ^ Set the threshold for logging.
	-> UIOptions
mkUIOptions moveNotation maybePrintMoveTree nDecimalDigits eitherNativeUIOrCECPOptions verbosity
	| Just depth <- maybePrintMoveTree
	, depth <= 0						= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.UIOptions.mkUIOptions:\t" . showString printMoveTreeTag . Text.ShowList.showsAssociation $ shows depth " must exceed zero."
	| nDecimalDigits < 1			= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.UIOptions.mkUIOptions:\t" . showString nDecimalDigitsTag . Text.ShowList.showsAssociation $ shows nDecimalDigits " must exceed zero."
	| nDecimalDigits > maxNDecimalDigits	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.Input.UIOptions.mkUIOptions:\t" . showString nDecimalDigitsTag . Text.ShowList.showsAssociation . shows nDecimalDigits . showString " shouldn't exceed " $ shows maxNDecimalDigits "."
	| (
		const False ||| const True $ eitherNativeUIOrCECPOptions
	) && not (
		Notation.MoveNotation.isPureCoordinate moveNotation
	)					= Control.Exception.throw . Data.Exception.mkIncompatibleData . showString "BishBosh.Input.UIOptions.mkUIOptions:\t" . shows Input.CECPOptions.tag . showString " is incompatible with " . showString Notation.MoveNotation.tag . Text.ShowList.showsAssociation $ shows moveNotation "."
	| otherwise				= MkUIOptions {
		getMoveNotation			= moveNotation,
		getMaybePrintMoveTree		= maybePrintMoveTree,
		getNDecimalDigits		= nDecimalDigits,
		getEitherNativeUIOrCECPOptions	= eitherNativeUIOrCECPOptions,
		getVerbosity			= verbosity
	}

-- | Whether the chess-engine has been temporarily turned-off in order to set-up pieces.
isCECPManualMode :: UIOptions -> Bool
isCECPManualMode MkUIOptions { getEitherNativeUIOrCECPOptions = eitherNativeUIOrCECPOptions }	= const False ||| (
	uncurry (||) . (Input.CECPOptions.getEditMode &&& Input.CECPOptions.getForceMode)
 ) $ eitherNativeUIOrCECPOptions

-- | The type of a function used to transform 'UIOptions'.
type Transformation	= UIOptions -> UIOptions

-- | Mutator.
updateCECPFeature :: Input.CECPFeatures.Feature -> Transformation
updateCECPFeature feature uiOptions@MkUIOptions { getEitherNativeUIOrCECPOptions = eitherNativeUIOrCECPOptions }	= uiOptions {
	getEitherNativeUIOrCECPOptions	= Input.CECPOptions.updateFeature feature <$> eitherNativeUIOrCECPOptions
}

-- | Mutator.
deleteCECPFeature :: Input.CECPFeatures.Feature -> Transformation
deleteCECPFeature feature uiOptions@MkUIOptions { getEitherNativeUIOrCECPOptions = eitherNativeUIOrCECPOptions }	= uiOptions {
	getEitherNativeUIOrCECPOptions	= Input.CECPOptions.deleteFeature feature <$> eitherNativeUIOrCECPOptions
}

