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

	* Implements <https://www.chessprogramming.org/Pondering> by an automatic player during a manual player's move.

	* Using the last move one can predict the opponent's response, & consequently one's counter-move.
	If the opponent makes the expected response, then this speculative result can be used,
	otherwise pondering is terminated & the effort wasted, but not the time, since the pondering occurred during the opponent's move.
-}

module BishBosh.Concurrent.Pondering(
-- * Types
-- ** Type-synonyms
--	OnymousPremise,
-- ** Data-types
	Pondering(
--		MkPondering,
		getPremise
--		getThreadId
	),
-- * Functions
	ponder,
	abort
 ) where

import			Control.DeepSeq(($!!))
import qualified	Control.Concurrent
import qualified	Control.DeepSeq
import qualified	Data.Maybe

-- | The premise on which pondering is based & the thread on which it is happening.
data Pondering premise	= MkPondering {
	getPremise	:: premise,
	getThreadId	:: Control.Concurrent.ThreadId
}

-- | A named premise.
type OnymousPremise premise	= (String, premise)

-- | Asynchronously evaluate the specified answer, then drop it into the MVar.
ponder
	:: Control.DeepSeq.NFData answer
	=> (String -> IO ())		-- ^ Used to print arbitrary strings.
	-> OnymousPremise premise	-- ^ (A name for the premise, the basis of the question to which an answer is required).
	-> answer			-- ^ Actually an unevaluated question.
	-> Control.Concurrent.MVar answer
	-> IO (Pondering premise)
ponder putStrLn' (premiseString, premise) answer mVar	= do
	threadId	<- Control.Concurrent.forkIO . Control.Concurrent.putMVar mVar $!! answer

	putStrLn' . showString "pondering (on " . shows threadId $ showString ") a response to " premiseString

	return {-to IO-monad-} MkPondering {
		getPremise	= premise,
		getThreadId	= threadId
	}

-- | Either terminates the running pondering thread, or purges the result it returned.
abort
	:: Control.Concurrent.MVar a
	-> Pondering premise
	-> IO String	-- ^ A string defining the action taken.
abort mVar MkPondering { getThreadId = threadId }	= Control.Concurrent.tryTakeMVar mVar >>= Data.Maybe.maybe (
	do
		Control.Concurrent.killThread threadId

		return {-to IO-monad-} . showString "killing " $ shows threadId "."
 ) (
	const $ return {-to IO-monad-} "purging MVar."
 )

