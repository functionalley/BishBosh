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

	* Defines the set of configurable options.
-}

module Duel.Data.Options(
-- * Types
-- ** Type-synonyms
	ReadTimeout,
-- ** Data-types
	Options(
--		MkOptions,
		getVerbosity,
		getNDecimalDigits,
		getNGames,
		getReadTimeout,
		getInputConfigFilePaths
	),
-- * Constants
--	requiredInputConfigFiles,
-- * Functions
-- ** Mutators
	setVerbosity,
	setNDecimalDigits,
	setNGames,
	setReadTimeout,
	appendInputConfigFilePath
) where

import qualified	Control.Exception
import qualified	Data.Default
import qualified	BishBosh.Data.Exception			as Data.Exception
import qualified	BishBosh.Input.Verbosity		as Input.Verbosity
import qualified	BishBosh.Model.Game			as Model.Game
import qualified	BishBosh.Property.SelfValidating	as Property.SelfValidating
import qualified	BishBosh.Property.ShowFloat		as Property.ShowFloat
import qualified	System.FilePath

-- | The read-timeout in ms.
type ReadTimeout	= Int

-- | Container for all command-line options.
data Options	= MkOptions {
	getVerbosity		:: Input.Verbosity.Verbosity,		-- ^ The extent to which logging is required. CAVEAT: this isn't forwarded to the forked instances of BishBosh.
	getNDecimalDigits	:: Property.ShowFloat.NDecimalDigits,	-- ^ The number of successive games to play.
	getNGames		:: Model.Game.NGames,			-- ^ The number of successive games to play.
	getReadTimeout		:: ReadTimeout,				-- ^ The seconds to wait for a move before timing-out. CAVEAT: any positive value should account for both configuration-options (e.g. search-depth) & the machine-speed.
	getInputConfigFilePaths	:: [System.FilePath.FilePath]		-- ^ The configuration-file paths for White & Black respectively.
} deriving (Eq, Show)

instance Data.Default.Default Options where
	def = MkOptions {
		getVerbosity		= Data.Default.def,
		getNDecimalDigits	= 0,
		getNGames		= 1,
		getReadTimeout		= -1,	-- N.B.: a negative value is interpreted as an indefinite period.
		getInputConfigFilePaths	= []	-- CAVEAT: invalid; there must be exactly 2.
	}

instance Property.SelfValidating.SelfValidating Options where
	findInvalidity	= Property.SelfValidating.findErrors [
		((/= requiredInputConfigFiles) . length . getInputConfigFilePaths,	"There must be exactly one configuration file for White & one for Black.")
	 ]

-- | Mutator.
setVerbosity :: Input.Verbosity.Verbosity -> Options -> Options
setVerbosity verbosity options	= options { getVerbosity = verbosity }

-- | Mutator.
setNDecimalDigits :: Property.ShowFloat.NDecimalDigits -> Options -> Options
setNDecimalDigits n options
	| n < 0		= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "Duel.Data.Options:\tnDecimalDigits=" $ shows n " mustn't be negative."
	| otherwise	= options { getNDecimalDigits = n }

-- | Mutator.
setNGames :: Model.Game.NGames -> Options -> Options
setNGames n options
	| n <= 0	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "Duel.Data.Options:\tnGames=" $ shows n " must exceed zero."
	| otherwise	= options { getNGames = n }

-- | Mutator.
setReadTimeout :: ReadTimeout -> Options -> Options
setReadTimeout t options	= options { getReadTimeout = t }

-- | The constant exact number of input config-files required.
requiredInputConfigFiles :: Int
requiredInputConfigFiles	= 2

-- | Mutator.
appendInputConfigFilePath :: System.FilePath.FilePath -> Options -> Options
appendInputConfigFilePath s options
	| length inputConfigFilePaths == requiredInputConfigFiles	= Control.Exception.throw . Data.Exception.mkRedundantData . showString "Duel.Data.Options:\texactly " . shows requiredInputConfigFiles . showString " file-paths are required:\t" $ shows inputConfigFilePaths "."
	| otherwise							= options { getInputConfigFilePaths = s : inputConfigFilePaths }
	where
		inputConfigFilePaths	= getInputConfigFilePaths options
