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
-- ** Data-types
	Options(
--		MkOptions,
		getInputConfigFilePaths,
		getNDecimalDigits,
		getNGames,
		getReadTimeout,
		getVerbosity,
		getVerifyConfiguration
	),
-- * Functions
-- ** Mutators
	setNDecimalDigits,
	setNGames,
	setReadTimeout,
	setVerbosity,
	setVerifyConfiguration,
	appendInputConfigFilePath
) where

import qualified	Control.Exception
import qualified	Data.Default
import qualified	BishBosh.Attribute.LogicalColour	as Attribute.LogicalColour
import qualified	BishBosh.Data.Exception			as Data.Exception
import qualified	BishBosh.Input.Verbosity		as Input.Verbosity
import qualified	BishBosh.Property.SelfValidating	as Property.SelfValidating
import qualified	BishBosh.Type.Count			as Type.Count
import qualified	System.FilePath

-- | Container for all command-line options.
data Options	= MkOptions {
	getInputConfigFilePaths	:: [System.FilePath.FilePath],	-- ^ The configuration-file paths for White & Black respectively.
	getNDecimalDigits	:: Type.Count.NDecimalDigits,	-- ^ The precision with which floating-point quantities are diaplayed.
	getNGames		:: Type.Count.NGames,		-- ^ The number of successive games to play.
	getReadTimeout		:: Type.Count.NSeconds,		-- ^ The seconds to wait for a move before timing-out. CAVEAT: any positive value should account for both configuration-options (e.g. search-depth) & the machine-speed.
	getVerbosity		:: Input.Verbosity.Verbosity,	-- ^ The extent to which logging is required. CAVEAT: this isn't forwarded to the forked instances of BishBosh.
	getVerifyConfiguration	:: Bool				-- ^ Whether to check the configuration-files before forwarding them.
} deriving (Eq, Show)

instance Data.Default.Default Options where
	def = MkOptions {
		getInputConfigFilePaths	= [],	-- CAVEAT: invalid; there must be exactly 2.
		getNDecimalDigits	= 0,
		getNGames		= 1,
		getReadTimeout		= -1,	-- N.B.: a negative value is interpreted as an indefinite period.
		getVerbosity		= Data.Default.def,
		getVerifyConfiguration	= False
	}

instance Property.SelfValidating.SelfValidating Options where
	findInvalidity	= Property.SelfValidating.findErrors [
		((/= Attribute.LogicalColour.nDistinctLogicalColours) . fromIntegral . length . getInputConfigFilePaths,	"There must be exactly one configuration file for White & one for Black.")
	 ]

-- | Mutator.
setNDecimalDigits :: Type.Count.NDecimalDigits -> Options -> Options
setNDecimalDigits nDecimalDigits options
	| nDecimalDigits < 0	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "Duel.Data.Options:\tnDecimalDigits=" $ shows nDecimalDigits " mustn't be negative."
	| otherwise		= options { getNDecimalDigits = nDecimalDigits }

-- | Mutator.
setNGames :: Type.Count.NGames -> Options -> Options
setNGames nGames options
	| nGames <= 0	= Control.Exception.throw . Data.Exception.mkOutOfBounds . showString "Duel.Data.Options:\tnGames=" $ shows nGames " must exceed zero."
	| otherwise	= options { getNGames = nGames }

-- | Mutator.
setReadTimeout :: Type.Count.NSeconds -> Options -> Options
setReadTimeout t options	= options { getReadTimeout = t }

-- | Mutator.
setVerbosity :: Input.Verbosity.Verbosity -> Options -> Options
setVerbosity verbosity options	= options { getVerbosity = verbosity }

-- | Mutator.
setVerifyConfiguration :: Bool -> Options -> Options
setVerifyConfiguration b options	= options { getVerifyConfiguration = b }

-- | Mutator.
appendInputConfigFilePath :: System.FilePath.FilePath -> Options -> Options
appendInputConfigFilePath s options
	| fromIntegral (
		length inputConfigFilePaths
	) == Attribute.LogicalColour.nDistinctLogicalColours	= Control.Exception.throw . Data.Exception.mkRedundantData . showString "Duel.Data.Options:\texactly " . shows Attribute.LogicalColour.nDistinctLogicalColours . showString " file-paths are required:\t" $ shows inputConfigFilePaths "."
	| otherwise						= options { getInputConfigFilePaths = s : inputConfigFilePaths }
	where
		inputConfigFilePaths	= getInputConfigFilePaths options
