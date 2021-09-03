{-# LANGUAGE DeriveDataTypeable #-}
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

	* Exceptions used by this application.

	* CAVEAT: though intended to be orthogonal, there's some inevitable overlap.
-}

module BishBosh.Data.Exception(
-- * Types
-- ** Data-types
	BadData(),
	BadRequest(),
	Exception(
--		MkException,
		getType
--		getDetails
	),
-- * Functions
-- ** Constructors
	mkDuplicateData,
	mkIncompatibleData,
	mkInsufficientData,
	mkInvalidDatum,
	mkNullDatum,
	mkOutOfBounds,
	mkRedundantData,
	mkParseFailure,
	mkRequestFailure,
	mkResultUndefined,
	mkSearchFailure,
-- ** Predicates
	isBadData,
	isBadRequest
) where

import			Control.Arrow((|||))
import qualified	Control.Exception
import qualified	Data.Typeable

-- | This sum-type of exceptions may be thrown by any function which checks its parameters; typically either constructors or mutators.
data BadData
	= DuplicateData		-- ^ Some data is duplicated.
	| IncompatibleData	-- ^ Two or more data with valid values, are incompatible. cf. InvalidDatum.
	| InsufficientData	-- ^ More data is required to fulfill the request. cf. 'NullDatum'.
	| InvalidDatum		-- ^ A datum's value is invalid.
	| NullDatum		-- ^ An empty collection was unexpectedly received; a specialisation of either 'InsufficientData' or 'InvalidDatum'.
	| OutOfBounds		-- ^ Either underflow or overflow of numeric data; a specialisation of 'InvalidDatum'.
	| RedundantData		-- ^ Data superflous to requirements was provided; a specialisation of 'InvalidDatum'.
	deriving Show

-- | This sum-type of exceptions may be thrown by any function which is unable to comply with a correctly formed request.
data BadRequest
	= ParseFailure		-- ^ An attempt to parse data failed.
	| RequestFailure	-- ^ A well-formed request couldn't be completed.
	| ResultUndefined	-- ^ More than one correct result is possible.
	| SearchFailure		-- ^ An attempt to find data failed.
	deriving Show

-- | Each exception includes both a type & arbitrary details.
data Exception	= MkException {
	getType		:: Either BadData BadRequest,
	getDetails	:: String
} deriving Data.Typeable.Typeable

instance Control.Exception.Exception Exception

instance Show Exception where
	showsPrec _ MkException {
		getType		= eitherBadDataOrBadRequest,
		getDetails	= details
	} = (shows ||| shows) eitherBadDataOrBadRequest . showString "; " . showString details

-- | Constructor.
mkDuplicateData :: String -> Exception
mkDuplicateData	= MkException $ Left DuplicateData

-- | Constructor.
mkIncompatibleData :: String -> Exception
mkIncompatibleData	= MkException $ Left IncompatibleData

-- | Constructor.
mkInsufficientData :: String -> Exception
mkInsufficientData	= MkException $ Left InsufficientData

-- | Constructor.
mkInvalidDatum :: String -> Exception
mkInvalidDatum	= MkException $ Left InvalidDatum

-- | Constructor.
mkNullDatum :: String -> Exception
mkNullDatum	= MkException $ Left NullDatum

-- | Constructor.
mkOutOfBounds :: String -> Exception
mkOutOfBounds	= MkException $ Left OutOfBounds

-- | Constructor.
mkRedundantData :: String -> Exception
mkRedundantData	= MkException $ Left RedundantData

-- | Constructor.
mkParseFailure :: String -> Exception
mkParseFailure	= MkException $ Right ParseFailure

-- | Constructor.
mkRequestFailure :: String -> Exception
mkRequestFailure	= MkException $ Right RequestFailure

-- | Constructor.
mkResultUndefined :: String -> Exception
mkResultUndefined	= MkException $ Right ResultUndefined

-- | Constructor.
mkSearchFailure :: String -> Exception
mkSearchFailure	= MkException $ Right SearchFailure

-- | Predicate.
isBadData :: Exception -> Bool
isBadData MkException { getType = Left _ }	= True
isBadData _					= False

-- | Predicate.
isBadRequest :: Exception -> Bool
isBadRequest MkException { getType = Right _ }	= True
isBadRequest _					= False

