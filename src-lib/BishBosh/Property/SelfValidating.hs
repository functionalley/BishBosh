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

 [@DESCRIPTION@]	Defines a class to which data capable of validating itself can confirm.
-}

module BishBosh.Property.SelfValidating(
-- * Type-classes
	SelfValidating(..),
-- * Functions
	findErrors,
-- ** Predicates
	isValid,
	isInvalid
 ) where

import	Control.Arrow((***))

{- |
	* This class serves data-types which must preserve compatibility beyond that which can be guarded by a smart-constructor.

	* E.g.: data-types which are constructed piece-meal & endure a temporarily invalid state.
-}
class SelfValidating a where
	findInvalidity	:: a -> [String]

instance SelfValidating a => SelfValidating [a] where
	findInvalidity	= concatMap findInvalidity

instance (SelfValidating a, SelfValidating b) => SelfValidating (a, b) where
	findInvalidity	= uncurry (++) . (findInvalidity *** findInvalidity)

-- | Predicate.
isValid	:: SelfValidating a => a -> Bool
isValid	= null . findInvalidity

-- | Predicate.
isInvalid :: SelfValidating a => a -> Bool
isInvalid	= not . isValid

-- | Selects relevant error-messages from the specified association-list, to facilitate implementation of 'findInvalidity'.
findErrors :: [(selfValidator -> Bool, String)] -> selfValidator -> [String]
findErrors assocs selfValidator	= [
	errorMessage |
		(predicate, errorMessage)	<- assocs,
		predicate selfValidator
 ] -- List-comprehension.

