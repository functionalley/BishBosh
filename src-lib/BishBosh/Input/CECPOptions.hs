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

	* Defines configurable options for [Chess-engine Communications-protocol](https://en.wikipedia.org/wiki/Chess_Engine_Communication_Protocol),
	as used by [XBoard](https://www.gnu.org/software/xboard/engine-intf.html)
-}

module BishBosh.Input.CECPOptions(
-- * Types
-- ** Type-synonyms
--	Mode,
--	ProtocolVersion,
--	Transformation,
-- ** Data-types
	CECPOptions(
--		MkCECPOptions,
		getAnalyseMode,
		getDisplaySAN,
		getEditMode,
		getForceMode,
		getMaybePaused,
		getPonderMode,
		getPostMode,
		getProtocolVersion,
		getCECPFeatures
	),
-- * Constants
	tag,
	analyseModeTag,
	displaySANTag,
	editModeTag,
	forceModeTag,
--	pausedTag,
	ponderModeTag,
	postModeTag,
	protocolVersionTag,
-- * Functions
	getNamedModes,
-- ** Constructors
	mkCECPOptions,
-- ** Mutators
	setProtocolVersion,
	updateFeature,
	deleteFeature,
	resetModes
) where

import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Input.CECPFeatures	as Input.CECPFeatures
import qualified	BishBosh.Text.ShowList		as Text.ShowList
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Default
import qualified	Data.Maybe
import qualified	Data.Time.Clock
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT

-- | Used to qualify XML.
tag :: String
tag			= "cecpOptions"

-- | Used to qualify XML.
analyseModeTag :: String
analyseModeTag		= "analyseMode"

-- | Used to qualify XML.
displaySANTag :: String
displaySANTag		= "displaySAN"

-- | Used to qualify XML.
editModeTag :: String
editModeTag		= "editMode"

-- | Used to qualify XML.
forceModeTag :: String
forceModeTag		= "forceMode"

-- | Used to qualify XML.
pausedTag :: String
pausedTag		= "pause"

-- | Used to qualify XML.
ponderModeTag :: String
ponderModeTag		= "ponderMode"

-- | Used to qualify XML.
postModeTag :: String
postModeTag		= "postMode"

-- | Used to qualify XML.
protocolVersionTag :: String
protocolVersionTag	= "protocolVersion"

-- | Self-documentation.
type Mode	= Bool

-- | Self-documentation.
type ProtocolVersion	= Int

-- | Defines options related to CECP.
data CECPOptions	= MkCECPOptions {
	getAnalyseMode		:: Mode,					-- ^ TODO.
	getDisplaySAN		:: Bool,					-- ^ Whether to display moves in SAN or 'Input.UIOptions.getMoveNotation'.
	getEditMode		:: Mode,					-- ^ Whether the game should be placed in set-up mode.
	getForceMode		:: Mode,					-- ^ Neither player's moves are automated, allowing an arbitrary game to be configured.
	getMaybePaused		:: Maybe Data.Time.Clock.NominalDiffTime,	-- ^ Whether the engine was paused after the specified time.
	getPonderMode		:: Mode,					-- ^ Whether to keep thinking while it's one's opponent's turn.
	getPostMode		:: Mode,					-- ^ Whether to show the details of deliberations.
	getProtocolVersion	:: ProtocolVersion,				-- ^ The version of the CECP-protocol to use.
	getCECPFeatures		:: Input.CECPFeatures.CECPFeatures
} deriving Eq

instance Control.DeepSeq.NFData CECPOptions where
	rnf MkCECPOptions {
		getAnalyseMode		= analyseMode,
		getDisplaySAN		= displaySAN,
		getEditMode		= editMode,
		getForceMode		= forceMode,
		getMaybePaused		= maybePaused,
		getPonderMode		= ponderMode,
		getPostMode		= postMode,
		getProtocolVersion	= protocolVersion,
		getCECPFeatures		= cecpFeatures
	} = Control.DeepSeq.rnf (analyseMode, displaySAN, editMode, forceMode, maybePaused, ponderMode, postMode, protocolVersion, cecpFeatures)

instance Show CECPOptions where
	showsPrec _ MkCECPOptions {
		getAnalyseMode		= analyseMode,
		getDisplaySAN		= displaySAN,
		getEditMode		= editMode,
		getForceMode		= forceMode,
		getMaybePaused		= maybePaused,
		getPonderMode		= ponderMode,
		getPostMode		= postMode,
		getProtocolVersion	= protocolVersion,
		getCECPFeatures		= cecpFeatures
	} = Text.ShowList.showsAssociationList' [
		(
			analyseModeTag,
			shows analyseMode
		), (
			displaySANTag,
			shows displaySAN
		), (
			editModeTag,
			shows editMode
		), (
			forceModeTag,
			shows forceMode
		), (
			pausedTag,
			shows maybePaused
		), (
			ponderModeTag,
			shows ponderMode
		), (
			postModeTag,
			shows postMode
		), (
			protocolVersionTag,
			shows protocolVersion
		), (
			Input.CECPFeatures.tag,
			showChar '{' . shows cecpFeatures . showChar '}'
		)
	 ]

instance Data.Default.Default CECPOptions where
	def = MkCECPOptions {
		getAnalyseMode		= False,
		getEditMode		= False,
		getDisplaySAN		= True,
		getForceMode		= False,
		getMaybePaused		= Nothing,
		getPonderMode		= False,
		getPostMode		= False,
		getProtocolVersion	= 1,
		getCECPFeatures		= Data.Default.def
	}

instance HXT.XmlPickler CECPOptions where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d, e, f, g, h, i) -> mkCECPOptions a b c d e f g h i,	-- Construct.
		\MkCECPOptions {
			getAnalyseMode		= analyseMode,
			getDisplaySAN		= displaySAN,
			getEditMode		= editMode,
			getForceMode		= forceMode,
			getMaybePaused		= maybePaused,
			getPonderMode		= ponderMode,
			getPostMode		= postMode,
			getProtocolVersion	= protocolVersion,
			getCECPFeatures		= cecpFeatures
		} -> (analyseMode, displaySAN, editMode, forceMode, maybePaused, ponderMode, postMode, protocolVersion, cecpFeatures) -- Deconstruct.
	 ) $ HXT.xp9Tuple (
		getAnalyseMode def `HXT.xpDefault` HXT.xpAttr analyseModeTag HXT.xpickle
	 ) (
		getDisplaySAN def `HXT.xpDefault` HXT.xpAttr displaySANTag HXT.xpickle
	 ) (
		getEditMode def `HXT.xpDefault` HXT.xpAttr editModeTag HXT.xpickle
	 ) (
		getForceMode def `HXT.xpDefault` HXT.xpAttr forceModeTag HXT.xpickle
	 ) (
		HXT.xpOption . HXT.xpWrap (toEnum, fromEnum) $ HXT.xpAttr pausedTag HXT.xpInt
	 ) (
		getPonderMode def `HXT.xpDefault` HXT.xpAttr ponderModeTag HXT.xpickle
	 ) (
		getPostMode def `HXT.xpDefault` HXT.xpAttr postModeTag HXT.xpickle
	 ) (
		getProtocolVersion def `HXT.xpDefault` HXT.xpAttr protocolVersionTag HXT.xpickle
	 ) HXT.xpickle {-CECPFeatures-} where
		def	= Data.Default.def

-- | Smart constructor.
mkCECPOptions
	:: Mode						-- ^ Analyse-mode.
	-> Bool						-- ^ Display SAN.
	-> Mode						-- ^ Edit-mode.
	-> Mode						-- ^ Force-mode.
	-> Maybe Data.Time.Clock.NominalDiffTime	-- ^ Paused.
	-> Mode						-- ^ Ponder-mode.
	-> Mode						-- ^ Post-mode.
	-> ProtocolVersion
	-> Input.CECPFeatures.CECPFeatures
	-> CECPOptions
mkCECPOptions analyseMode displaySAN editMode forceMode maybePaused ponderMode postMode protocolVersion cecpFeatures
	| protocolVersion < 1				= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.UI.CECPOptions.mkCECPOptions:\t" $ shows protocolVersionTag " must exceed zero."
	| Data.Maybe.maybe False (< 0) maybePaused	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "BishBosh.UI.CECPOptions.mkCECPOptions:\t" $ shows pausedTag "; time already taken can't be negative."
	| otherwise					= MkCECPOptions {
		getAnalyseMode		= analyseMode,
		getEditMode		= editMode,
		getDisplaySAN		= displaySAN,
		getForceMode		= forceMode,
		getMaybePaused		= maybePaused,
		getPonderMode		= ponderMode,
		getPostMode		= postMode,
		getProtocolVersion	= protocolVersion,
		getCECPFeatures		= cecpFeatures
	}

-- | Self-documentation.
type Transformation	= CECPOptions -> CECPOptions

-- | Mutator.
setProtocolVersion :: ProtocolVersion -> Transformation
setProtocolVersion protocolVersion cecpOptions
	| protocolVersion < 1	= Control.Exception.throw $ Data.Exception.mkOutOfBounds . showString "BishBosh.UI.CECPOptions.setProtocolVersion:\t" $ shows protocolVersion " must exceed zero."
	| otherwise		= cecpOptions {
		getProtocolVersion	= protocolVersion
	}

-- | Mutator.
updateFeature :: Input.CECPFeatures.Feature -> Transformation
updateFeature feature cecpOptions@MkCECPOptions { getCECPFeatures = cecpFeatures }	= cecpOptions {
	getCECPFeatures	= Input.CECPFeatures.updateFeature feature cecpFeatures
}

-- | Mutator.
deleteFeature :: Input.CECPFeatures.Feature -> Transformation
deleteFeature feature cecpOptions@MkCECPOptions { getCECPFeatures = cecpFeatures }	= cecpOptions {
	getCECPFeatures	= Input.CECPFeatures.deleteFeature feature cecpFeatures
}

-- | Reset all modes but leave the remaining fields unaltered.
resetModes :: Transformation
resetModes cecpOptions	= cecpOptions {
	getAnalyseMode	= False,
	getEditMode	= False,
	getForceMode	= False,
	getMaybePaused	= Nothing,
	getPonderMode	= False,
	getPostMode	= False
}

-- | Get an association-list of named modes.
getNamedModes :: CECPOptions -> [(String, Mode)]
getNamedModes MkCECPOptions {
	getAnalyseMode	= analyseMode,
	getEditMode	= editMode,
	getForceMode	= forceMode,
	getMaybePaused	= maybePaused,
	getPonderMode	= ponderMode,
	getPostMode	= postMode
} = [
	(
		analyseModeTag,
		analyseMode
	), (
		editModeTag,
		editMode
	), (
		forceModeTag,
		forceMode
	), (
		pausedTag,
		Data.Maybe.isJust maybePaused
	), (
		ponderModeTag,
		ponderMode
	), (
		postModeTag,
		postMode
	)
 ]

