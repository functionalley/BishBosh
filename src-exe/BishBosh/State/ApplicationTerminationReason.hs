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

 [@DESCRIPTION@]	Itemises the various reasons for the termination of the application.
-}

module BishBosh.State.ApplicationTerminationReason(
-- * Types
-- ** Data-types
	ApplicationTerminationReason(),
-- * Constants
	byRequest,
	maximumPlies
) where

import qualified	Control.DeepSeq

-- | Categorises the possible reasons for terminating the application.
data ApplicationTerminationReason
	= ByRequest	-- ^ The user requested exit.
	| MaximumPlies	-- ^ The configured maximum number of /turn/s has been reached.

instance Show ApplicationTerminationReason where
	showsPrec _ ByRequest		= showString "by request"
	showsPrec _ MaximumPlies	= showString "because the configured maximum number of plies has been reached"

instance Control.DeepSeq.NFData ApplicationTerminationReason where
	rnf _	= ()

-- | Constant.
byRequest :: ApplicationTerminationReason
byRequest	= ByRequest

-- | Constant.
maximumPlies :: ApplicationTerminationReason
maximumPlies	= MaximumPlies

