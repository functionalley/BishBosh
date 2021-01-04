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

 [@DESCRIPTION@]	Defines CECP-features.
-}

module BishBosh.Input.CECPFeatures(
-- * Types
-- ** Type-synonyms
--	Key,
--	Value,
	Feature,
--	Transformation,
-- ** Data-types
	CECPFeatures(
--		MkCECPFeatures
		getFeatures
--		getDone
	),
-- * Constants
	tag,
	featureTag,
	analyseTag,
	coloursTag,
--	debugTag,
--	doneTag,
	drawTag,
--	egtTag,
--	excludeTag,
--	highlightTag,
	icsTag,
--	memoryTag,
--	mynameTag,
	nameTag,
	npsTag,
	optionTag,
	pauseTag,
	pingTag,
	playotherTag,
--	reuseTag,
--	sanTag,
	setboardTag,
--	sigintTag,
--	sigtermTag,
--	smpTag,
	timeTag,
	usermoveTag,
--	variantsTag,
--	showsFeatureSeparator,
--	showsKVSeparator,
	resolution,
	inputWidget,
	sliderWidget,
-- * Functions
-- ** Constructors
	mkCECPFeatures,
-- ** Mutators
	prependFeature,
	deleteFeature,
	updateFeature,
-- ** Predicates
	isFeatureDisabled
) where

import			BishBosh.Data.Bool()	-- For 'HXT.XmlPickler Bool'.
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Text.ShowList		as Text.ShowList
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Char
import qualified	Data.Default
import qualified	Data.List
import qualified	Data.Maybe
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT

-- | Used to qualify XML.
tag :: String
tag		= "cecpFeatures"

-- | Used to qualify XML.
featureTag :: String
featureTag	= "feature"

-- | Self-documentation.
type Key	= String

-- | Each feature-value can be either an @Int@ (also used to represent @Bool@ values), or an arbitrary @String@.
type Value	= Either Int String

-- | Self-documentation.
type Feature	= (Key, Value)

-- | Tags the feature which defines whether the GUI may request "analyse" mode.
analyseTag :: Key
analyseTag	= "analyze"

-- | Tags the feature which defines whether the GUI may request that a specific player move next.
coloursTag :: Key
coloursTag	= "colors"

-- | Tags the feature which defines whether the engine may send debug information to the GUI.
debugTag :: Key
debugTag	= "debug"

-- | The tag used to terminate the feature-list.
doneTag :: Key
doneTag		= "done"

-- | Tags the feature which defines whether the GUI may send the "draw" command.
drawTag :: Key
drawTag		= "draw"

-- | Tags the feature which defines whether the GUI may send the "egtpath" command.
egtTag :: Key
egtTag		= "egt"

-- | Tags the feature which defines whether the GUI may send the "exclude" command.
excludeTag :: Key
excludeTag	= "exclude"

-- | Tags the feature which defines whether the GUI may send the "lift", "put" & "hover" commands.
highlightTag :: Key
highlightTag	= "highlight"

-- | Tag the feature which defines whether the GUI should inform the engine whether it's on a chess-server.
icsTag :: Key
icsTag		= "ics"

-- | Tags the feature which defines whether the GUI can request that the engine cap memory-use.
memoryTag :: Key
memoryTag	= "memory"

-- | Tags the feature which defines the engine's name to the GUI.
mynameTag :: Key
mynameTag	= "myname"

-- | Tags the feature which defines whether the GUI can tell the engine the opponent's name.
nameTag :: Key
nameTag		= "name"

-- | Tags the feature which defines whether the GUI can limit thinking-time to a number of nodes searched.
npsTag :: Key
npsTag		= "nps"

-- |
optionTag :: Key
optionTag	= "option"

-- | Tags the feature which defines whether the GUI may send the "pause" command.
pauseTag :: Key
pauseTag	= "pause"

-- | Tags the feature which defines whether the GUI may send a "ping" command.
pingTag :: Key
pingTag		= "ping"

-- | Tags the feature which defines whether the GUI may send the "playother" command.
playotherTag :: Key
playotherTag	= "playother"

-- | Tags the feature which defines whether the GUI may reuse this engine for multiple games.
reuseTag :: Key
reuseTag	= "reuse"

-- | Tags the feature which defines whether the GUI may send moves in "SAN" rather than "PureCoordinate" notation.
sanTag :: Key
sanTag		= "san"

-- | Tags the feature which defines whether the GUI may send the "setboard" command.
setboardTag :: Key
setboardTag	= "setboard"

-- | Tags the feature which defines whether the GUI may send the engine a "SIGINT".
sigintTag :: Key
sigintTag	= "sigint"

-- | Tags the feature which defines whether the GUI may send the engine a "SIGTERM".
sigtermTag :: Key
sigtermTag	= "sigterm"

-- | Tags the feature which defines whether the GUI may cap the number of cores used by the engine.
smpTag :: Key
smpTag		= "smp"

-- | Tags the feature which defines whether the GUI may adjust the engine's move-timer.
timeTag :: Key
timeTag		= "time"

-- | Tags the feature which defines whether the GUI should prefix move-commands to facilitate identification.
usermoveTag :: Key
usermoveTag	= "usermove"

-- | Tags the feature which defines the supported variants of chess.
variantsTag :: Key
variantsTag	= "variants"

-- | Defines the feature-separator used for streaming.
showsFeatureSeparator :: ShowS
showsFeatureSeparator	= showChar ' '

-- | Defines the key-value separator used for streaming features.
showsKVSeparator :: ShowS
showsKVSeparator	= showChar '='

-- | The resolution of sliders depicted in the GUI.
resolution :: Int
resolution	= 1000

-- | The string sent to xboard, to request an input widget.
inputWidget :: String
inputWidget	= "-string"

-- | The string sent to xboard, to request a slider-widget.
sliderWidget :: String
sliderWidget	= "-slider"

-- | Defines CECP-features.
data CECPFeatures	= MkCECPFeatures {
	getFeatures	:: [Feature],	-- ^ The list of features.
	getDone		:: Bool		-- ^ Whether the GUI should assume that all features have been received, or wait until a timeout before proceeding.
} deriving Eq

instance Control.DeepSeq.NFData CECPFeatures where
	rnf MkCECPFeatures {
		getFeatures	= features,
		getDone		= done
	} = Control.DeepSeq.rnf (features, done)

instance Show CECPFeatures where
	showsPrec _ MkCECPFeatures {
		getFeatures	= features,
		getDone		= done
	} = Text.ShowList.showsDelimitedList showsFeatureSeparator id id (
		map (
			\(k, v) -> showString k . showsKVSeparator . either shows {-Int-} (\s -> showChar '"' . showString s . showChar '"') v
		) features
	 ) . showsFeatureSeparator . showString doneTag . showsKVSeparator . shows (if done then (1 :: Int) else 0)

instance Data.Default.Default CECPFeatures where
	def = MkCECPFeatures {
		getFeatures	= let
			false : true : _	= map Left [0 ..]

			mkCommaSeparatedList	= Right . Data.List.intercalate ","
		in [
			(
				analyseTag,	false			-- Whether xboard may send the "analyze" command, should the user asks for analysis mode.
			), (
				coloursTag,	false			-- Whether xboard may send the obsolete "white" and "black" commands.
			), (
				drawTag,	true			-- Whether xboard may send the "draw" command, should the user request one.
			), (
				debugTag,	false			-- Whether the engine may send debug-output (prefixed by '#') to xboard.
			), (
				egtTag,		mkCommaSeparatedList []	-- Whether xboard can send the "egtpath" command to define the path to end-game tables.
			), (
				excludeTag,	false			-- Whether xboard can send the "exclude" command to control which moves are searched.
			), (
				highlightTag,	false			-- Whether xboard can send "lift", "put" and "hover" commands to the engine.
			), (
				icsTag,		true			-- Whether xboard should inform us whether it is playing on a chess-server.
			), (
				memoryTag,	false			-- Whether xboard can send the "memory" command to cap memory-use.
			), (
				mynameTag,	Right "BishBosh"	-- Defines the name xboard uses for in-window banners, in the PGN-tags of saved game-files, & when sending the "name" command to another engine.
			), (
				nameTag,	true			-- Whether xboard may send the "name" command to inform us of the opponent's name.
			), (
				npsTag,		false			-- Whether xboard may send the "nps" command, to limit thinking by the number of nodes searched rather than time.
			), (
				pauseTag,	true			-- Whether xboard may send the "pause" command.
			), (
				pingTag,	true			-- Whether xboard may send the "ping" command.
			), (
				playotherTag,	true			-- Whether xboard may send the "playother" command.
			), (
				reuseTag,	true			-- Whether xboard may reuse this engine for multiple games.
			), (
				sanTag,		false			-- Whether xboard sends moves in "Standard Algebraic Notation" rather than "PureCoordinate".
			), (
				setboardTag,	true			-- Whether xboard may send the "setboard" command, to define the board.
			), (
				sigintTag,	false			-- Whether xboard may send SIGINT, which it might if it believes the engine isn't listening.
			), (
				sigtermTag,	true			-- Whether xboard may send SIGTERM, which it does shortly after the "quit" command.
			), (
				smpTag,		true			-- Whether xboard can send the "cores" command to limit the number of CPU-cores.
			), (
				timeTag,	false			-- Whether xboard may send the "time" and "otim" commands to update the engine's clocks.
			), (
				usermoveTag,	true			-- Whether xboard should send the "usermove"-prefix to moves.
			), (
				variantsTag,	mkCommaSeparatedList [
					"normal"
				]					-- The set of acceptable game-variants.
			)
		],
		getDone	= True	-- Terminate the feature-list.
	 }

instance HXT.XmlPickler CECPFeatures where
	xpickle	= HXT.xpDefault def . HXT.xpElem tag . HXT.xpWrap (
		uncurry mkCECPFeatures,	-- Construct.
		\MkCECPFeatures {
			getFeatures	= features,
			getDone		= done
		} -> (features, done)	-- Deconstruct.
	 ) $ (
		HXT.xpList . HXT.xpElem featureTag $ HXT.xpTextAttr "key" `HXT.xpPair` HXT.xpWrap (
			\s -> case reads s of
				[(i, "")]	-> Left i
				_		-> Right s,
			\value -> showChar '"' $ either shows showString value "\""
		 ) (
			HXT.xpAttr "value" HXT.xpText0
		 )
	 ) `HXT.xpPair` (
		getDone def `HXT.xpDefault` HXT.xpAttr doneTag HXT.xpickle
	 ) where
		def	= Data.Default.def

-- | Smart-constructor.
mkCECPFeatures :: [Feature] -> Bool -> CECPFeatures
mkCECPFeatures features done
	| Just (key, _)	<- Data.List.find (
		any (
			not . Data.Char.isAlpha
		) . fst {-key-}
	) features	= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Input.CECPFeatures.mkCECPFeatures:\tinvalid key" . Text.ShowList.showsAssociation $ shows key "."
	| Just (_, value)	<- Data.List.find (
		either (
			const False
		) (
			any (`elem` "\"\n\r")	-- Prevent command-injection.
		) . snd {-value-}
	) features	= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.Input.CECPFeatures.mkCECPFeatures:\tinvalid value" . Text.ShowList.showsAssociation $ shows value "."
	| otherwise	= MkCECPFeatures {
		getFeatures	= features,
		getDone		= done
	}

-- | Self-documentation.
type Transformation	= CECPFeatures -> CECPFeatures

{- |
	* Prepends the specified feature.

	* CAVEAT: this may create a duplicate key.
-}
prependFeature :: Feature -> Transformation
prependFeature feature cecpFeatures@MkCECPFeatures {
	getFeatures	= features
} = cecpFeatures {
	getFeatures	= feature : features
}

-- | Deletes the specified feature.
deleteFeature :: Feature -> Transformation
deleteFeature feature@(key, value) cecpFeatures@MkCECPFeatures {
	getFeatures	= features
} = cecpFeatures {
	getFeatures	= filter (
		either (
			const $ (/= key) . fst
		) (
			const (/= feature)	-- N.B.: string-valued features must also match the specified value, to account for possibility of duplicates.
		) value
	) features
}

{- |
	* Prepends the specified feature.

	* CAVEAT: deletes all similarly named features.
-}
updateFeature :: Feature -> Transformation
updateFeature feature	= prependFeature feature . deleteFeature feature

-- | Predicate.
isFeatureDisabled :: Key -> CECPFeatures -> Bool
isFeatureDisabled key MkCECPFeatures {
	getFeatures	= features
} = Data.Maybe.maybe (
	Control.Exception.throw . Data.Exception.mkSearchFailure . showString "BishBosh.Input.CECPFeatures.isFeatureDisabled:\t" $ shows key " not found."
 ) (
	(== 0) `either` null
 ) $ lookup key features

