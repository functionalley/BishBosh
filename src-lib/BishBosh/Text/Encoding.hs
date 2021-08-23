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

 [@DESCRIPTION@]	Defines instances for 'System.IO.TextEncoding'.
-}

module BishBosh.Text.Encoding (
-- * Constants
	tag
--	range
 ) where

import qualified	BishBosh.Property.FixedMembership	as Property.FixedMembership
import qualified	Data.Default
import qualified	Data.List.Extra
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT
import qualified	Text.XML.HXT.Arrow.Pickle.Schema
import qualified	System.IO

-- | Used to label an XML-attribute.
tag :: String
tag	= "textEncoding"

instance Eq System.IO.TextEncoding where
	l == r	= show l == show r

instance Read System.IO.TextEncoding where
	readsPrec _ s	= case Data.List.Extra.trimStart s of
		'I':'S':'O':remainder -> case remainder of
			'8':'8':'5':'9':'-':'1':remainder2	-> return {-to List-monad-} . (,) System.IO.latin1 $ case remainder2 of
				'(':'c':'h':'e':'c':'k':'e':'d':')':remainder3	-> remainder3
				_						-> remainder2
			'-':'8':'8':'5':'9':'-':'1':remainder2	-> return {-to List-monad-} . (,) System.IO.latin1 $ case remainder2 of
				'(':'c':'h':'e':'c':'k':'e':'d':')':remainder3	-> remainder3
				_						-> remainder2
			_					-> []	-- No parse.
		'U':'T':'F':'-':remainder -> case remainder of
			'8':remainder2		-> [(System.IO.utf8, remainder2)]
			'1':'6':remainder2	-> [(System.IO.utf16, remainder2)]
			'3':'2':remainder2	-> [(System.IO.utf32, remainder2)]
			_			-> []	-- No parse.
		_	-> []	-- No parse.

instance HXT.XmlPickler System.IO.TextEncoding where
	xpickle	= HXT.xpDefault Data.Default.def . HXT.xpWrap (read, show) . HXT.xpAttr tag . HXT.xpTextDT . Text.XML.HXT.Arrow.Pickle.Schema.scEnum $ map show range

instance Data.Default.Default System.IO.TextEncoding where
	def	= System.IO.utf8

-- | The constant range of /Text-encoding/s.
range :: [System.IO.TextEncoding]
range	= [
	System.IO.latin1,
	System.IO.utf8,
	System.IO.utf16,
	System.IO.utf32
 ]

instance Property.FixedMembership.FixedMembership System.IO.TextEncoding where
	members	= range

