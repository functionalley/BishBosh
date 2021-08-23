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
		getNGames,
		getReadTimeout,
		getInputConfigFilePaths
	),
-- * Constants
	requiredInputConfigFiles,
-- * Functions
-- ** Mutators
	setVerbosity,
	setNGames,
	setReadTimeout,
	appendInputConfigFilePath
) where

import qualified	Control.Exception
import qualified	Data.Default
import qualified	BishBosh.Data.Exception		as Data.Exception
import qualified	BishBosh.Input.Verbosity	as Input.Verbosity
import qualified	BishBosh.Model.Game		as Model.Game
import qualified	System.FilePath

-- | The read-timeout in ms.
type ReadTimeout	= Int

-- | Container for all comand-line options.
data Options	= MkOptions {
	getVerbosity		:: Input.Verbosity.Verbosity,	-- ^ The extent to which logging is required. CAVEAT: this isn't forwarded to the forked instances of BishBosh.
	getNGames		:: Model.Game.NGames,		-- ^ The number of successive games to play.
	getReadTimeout		:: ReadTimeout,			-- ^ The seconds to wait for a move before timing-out. CAVEAT: a positive value should account for both search-depth & the machine-speed.
	getInputConfigFilePaths	:: [System.FilePath.FilePath]
} deriving (Eq, Show)

instance Data.Default.Default Options where
	def = MkOptions {
		getVerbosity		= Data.Default.def,
		getNGames		= 1,
		getReadTimeout		= -1,	-- N.B.: a negative value is interpreted as an indefinite period.
		getInputConfigFilePaths	= []
	}

-- | Mutator.
setVerbosity :: Input.Verbosity.Verbosity -> Options -> Options
setVerbosity verbosity options	= options { getVerbosity = verbosity }

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
	| length inputConfigFilePaths == requiredInputConfigFiles	= Control.Exception.throw . Data.Exception.mkRedundantData . showString "Duel.Data.Options:\texactly " . shows requiredInputConfigFiles . showString " files are required:\t" $ shows inputConfigFilePaths "."
	| otherwise							= options { getInputConfigFilePaths = s : inputConfigFilePaths }
	where
		inputConfigFilePaths	= getInputConfigFilePaths options
