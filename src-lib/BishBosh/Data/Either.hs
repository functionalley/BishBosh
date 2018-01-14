{-# OPTIONS_GHC -fno-warn-orphans #-}
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

 [@DESCRIPTION@]	Defines a /pickler/ for 'Either'.
-}

module BishBosh.Data.Either(
-- * Functions
	xpickle
) where

import qualified	Text.XML.HXT.Arrow.Pickle	as HXT

instance (HXT.XmlPickler l, HXT.XmlPickler r) => HXT.XmlPickler (Either l r) where
	xpickle	= xpickle HXT.xpickle HXT.xpickle

-- | Pickler for an arbitrary datum of type 'Either'.
xpickle :: HXT.PU l -> HXT.PU r -> HXT.PU (Either l r)
xpickle lPickler rPickler	= HXT.xpAlt (
	const 0 `either` const 1	-- Index into the subsequent pickler-list.
 ) [
	HXT.xpWrap (
		Left,		-- Construct.
		\(Left l) -> l	-- Deconstruct.
	) lPickler,
	HXT.xpWrap (
		Right,		-- Construct.
		\(Right r) -> r	-- Deconstruct.
	) rPickler
 ]

