{-# LANGUAGE CPP #-}
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

 [@DESCRIPTION@]	Defines a parser for PGN; <https://www.chessclub.com/help/pgn-spec>.
-}

module BishBosh.ContextualNotation.PGN(
-- * Types
-- ** Type-synonyms
	Tag,
--	Value,
--	TagPair,
	IsStrictlySequential,
-- ** Data-types
	PGN(
--		MkPGN,
--		getMaybeEventName,
--		getMaybeSiteName,
--		getDay,
		getMaybeRoundName,
--		getMaybeWhitePlayerName,
--		getMaybeBlackPlayerName,
		getIdentificationTagPairs,
		getGame
	),
-- * Constants
--	tagPairDelimiters,
--	ravDelimiters,
--	inProgressFlag,
--	moveNumberTerminator,
	quoteDelimiter,
--	dateComponentSeparator,
	unknownTagValue,
--	eventNameTag,
--	siteNameTag,
	dateTag,
--	roundNameTag,
--	whitePlayerNameTag,
--	blackPlayerNameTag,
--	resultTag,
-- * Functions
	showsDate,
	showsMoveText,
	showsGame,
--	nagParser,
--	moveNumberParser,
--	maybeResultParser,
	moveTextParser,
	parser,
-- ** Constructors
	mkPGN,
	mkPGN'
 ) where

import			Control.Arrow((&&&), (***))
import qualified	BishBosh.Colour.LogicalColour			as Colour.LogicalColour
import qualified	BishBosh.ContextualNotation.PGNComment		as ContextualNotation.PGNComment
import qualified	BishBosh.ContextualNotation.StandardAlgebraic	as ContextualNotation.StandardAlgebraic
import qualified	BishBosh.Data.Exception				as Data.Exception
import qualified	BishBosh.Model.Game				as Model.Game
import qualified	BishBosh.Property.FixedMembership		as Property.FixedMembership
import qualified	BishBosh.Rule.GameTerminationReason		as Rule.GameTerminationReason
import qualified	BishBosh.Rule.Result				as Rule.Result
import qualified	BishBosh.State.TurnsByLogicalColour		as State.TurnsByLogicalColour
import qualified	BishBosh.Text.ShowList				as Text.ShowList
import qualified	BishBosh.Type.Count				as Type.Count
import qualified	Control.Applicative
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Char
import qualified	Data.Default
import qualified	Data.List
import qualified	Data.Map					as Map
import qualified	Data.Maybe
import qualified	Data.Time.Calendar
import qualified	Data.Time.Clock
import qualified	Text.Printf

#ifdef USE_POLYPARSE
import qualified	BishBosh.Text.Poly				as Text.Poly
#	if USE_POLYPARSE == 'L'
import qualified	Text.ParserCombinators.Poly.Lazy		as Poly
#	elif USE_POLYPARSE == 'P'
import qualified	Text.ParserCombinators.Poly.Plain		as Poly
#	else
#		error "USE_POLYPARSE invalid"
#	endif
#else /* Parsec */
import qualified	BishBosh.Data.Integral				as Data.Integral
import qualified	Text.ParserCombinators.Parsec			as Parsec
import			Text.ParserCombinators.Parsec((<?>), (<|>))
#endif

-- | The constant delimiters for a /tag-pair/.
tagPairDelimiters :: (Char, Char)
tagPairDelimiters	= ('[', ']')

-- | The constant delimiters for a /Recursive Annotation Variation/.
ravDelimiters :: (Char, Char)
ravDelimiters		= ('(', ')')

-- | Constant indication of a game still in progress.
inProgressFlag :: Char
inProgressFlag		= '*'

-- | Constant prefix for a /Numeric Annotation Glyph/.
nagPrefix :: Char
nagPrefix		= '$'

-- | Constant terminator for a move-number.
moveNumberTerminator :: Char
moveNumberTerminator	= '.'

-- | Constant delimiter for a tag-value.
quoteDelimiter :: Char
quoteDelimiter		= '"'

-- | Constant separator between the components of a date.
dateComponentSeparator :: Char
dateComponentSeparator	= '.'

-- | Constant used to represent an unknown value for a mandatory tag.
unknownTagValue :: Char
unknownTagValue		= '?'

-- | The type of the name of field in a PGN-specification.
type Tag	= String

-- | The type of the value of field in a PGN-specification.
type Value	= String

-- | Self-documentation.
type TagPair	= (Tag, Value)

-- | Whether moves with an unexpected move-number should be considered to be an error.
type IsStrictlySequential	= Bool

-- | Qualifies a mandatory tag-value.
eventNameTag :: Tag
eventNameTag		= "Event"

-- | Qualifies a mandatory tag-value.
siteNameTag :: Tag
siteNameTag		= "Site"

-- | Qualifies a mandatory tag-value.
dateTag :: Tag
dateTag			= "Date"

-- | Qualifies a tag-value.
roundNameTag :: Tag
roundNameTag		= "Round"

-- | Qualifies a mandatory tag-value.
whitePlayerNameTag :: Tag
whitePlayerNameTag	= "White"

-- | Qualifies a mandatory tag-value.
blackPlayerNameTag :: Tag
blackPlayerNameTag	= "Black"

-- | Qualifies a tag-value.
resultTag :: Tag
resultTag		= "Result"

-- | Shows the specified date in double quotes.
showsDate :: Data.Time.Calendar.Day -> ShowS
showsDate	= (
	\(y, m, d) -> shows (Text.Printf.printf "%04d.%02d.%02d" y m d :: String)
 ) . Data.Time.Calendar.toGregorian

{- |
	* The data defined by PGN.

	* The first six fields are mandatory according to the PGN-specification, though none are used by this application.
	The seventh mandatory field /Result/ can be derived from 'getGame'.
-}
data PGN	= MkPGN {
	getMaybeEventName		:: Maybe Value,
	getMaybeSiteName		:: Maybe Value,
	getDay				:: Data.Time.Calendar.Day,
	getMaybeRoundName		:: Maybe Value,		-- ^ The name of the round; typically dotted decimal.
	getMaybeWhitePlayerName		:: Maybe Value,
	getMaybeBlackPlayerName		:: Maybe Value,
	getIdentificationTagPairs	:: [TagPair],		-- ^ Arbitrary tagged values.
	getGame				:: Model.Game.Game	-- ^ Defines the turn-sequence & the result.
} deriving Eq

instance Show PGN where
	showsPrec _ MkPGN {
		getMaybeEventName		= maybeEventName,
		getMaybeSiteName		= maybeSiteName,
		getDay				= day,
		getMaybeRoundName		= maybeRoundName,
		getMaybeWhitePlayerName		= maybeWhitePlayerName,
		getMaybeBlackPlayerName		= maybeBlackPlayerName,
		getIdentificationTagPairs	= identificationTagPairs,
		getGame				= game
	} = foldr (
		\(tagName, showsTagValue) accum -> showChar (fst tagPairDelimiters) . showString tagName . showChar ' ' . showsTagValue . showChar (snd tagPairDelimiters) . showChar '\n' . accum
	 ) (
		showsMoveText game . showChar '\n'
	 ) $ [
		(
			eventNameTag,		representUnknownTagValue maybeEventName
		), (
			siteNameTag,		representUnknownTagValue maybeSiteName
		), (
			dateTag,		showsDate day
		), (
			roundNameTag,		representUnknownTagValue maybeRoundName
		), (
			whitePlayerNameTag,	representUnknownTagValue maybeWhitePlayerName
		), (
			blackPlayerNameTag,	representUnknownTagValue maybeBlackPlayerName
		), (
			resultTag,		quote . Data.Maybe.maybe (showChar inProgressFlag) (shows . Rule.GameTerminationReason.toResult) $ Model.Game.getMaybeTerminationReason game
		)
	 ] ++ map (
		Control.Arrow.second $ quote . showString
	 ) identificationTagPairs where
		quote :: ShowS -> ShowS
		quote s	= showChar quoteDelimiter . s . showChar quoteDelimiter

		representUnknownTagValue :: Maybe Value -> ShowS
		representUnknownTagValue	= quote . Data.Maybe.maybe (showChar unknownTagValue) showString

instance Control.DeepSeq.NFData PGN where
	rnf MkPGN {
		getIdentificationTagPairs	= identificationTagPairs,
		getGame				= game
	} = Control.DeepSeq.rnf (identificationTagPairs, game)	-- CAVEAT: treat other fields lazily, since though they're specified as mandatory, this is isn't observed in practice because they're not typically required.

-- | Smart constructor.
mkPGN
	:: Maybe Value	-- ^ Event-name.
	-> Maybe Value	-- ^ Site-name.
	-> Data.Time.Calendar.Day
	-> Maybe Value	-- ^ Round.
	-> Maybe Value	-- ^ Name of White player.
	-> Maybe Value	-- ^ Name of Black player.
	-> [TagPair]	-- ^ Arbitrary tag-pairs.
	-> Model.Game.Game
	-> PGN
mkPGN maybeEventName maybeSiteName day maybeRoundName maybeWhitePlayerName maybeBlackPlayerName identificationTagPairs game
	| any (
		(== [unknownTagValue]) . snd {-tag-value-}
	) explicitTags	= Control.Exception.throw . Data.Exception.mkInvalidDatum . showString "BishBosh.ContextualNotation.PGN.mkPGN:\tunknownTagValue" . Text.ShowList.showsAssociation . shows unknownTagValue . showString " used literally; " $ show explicitTags
	| otherwise	= MkPGN {
		getMaybeEventName		= maybeEventName,
		getMaybeSiteName		= maybeSiteName,
		getDay				= day,
		getMaybeRoundName		= maybeRoundName,
		getMaybeWhitePlayerName		= maybeWhitePlayerName,
		getMaybeBlackPlayerName		= maybeBlackPlayerName,
		getIdentificationTagPairs	= identificationTagPairs,
		getGame				= game
	}
	where
		explicitTags	= [
			(tag, value) |
				(tag, Just value)	<- [
					(
						eventNameTag,		maybeEventName
					), (
						siteNameTag,		maybeSiteName
					), (
						whitePlayerNameTag,	maybeWhitePlayerName
					), (
						blackPlayerNameTag,	maybeBlackPlayerName
					)
				 ] -- Those tags for which an unknown value is represented by 'unknownTagValue'.
		 ] -- List-comprehension.

-- | Smart constructor.
mkPGN'
	:: [Tag]	-- ^ Identify fields used to form a unique composite game-identifier.
	-> [TagPair]	-- ^ The data from which to extract the required values.
	-> Model.Game.Game
	-> PGN
mkPGN' identificationTags tagPairs	= mkPGN maybeEventName maybeSiteName (
	let
#ifdef USE_POLYPARSE
		dateParser :: Text.Poly.TextParser Data.Time.Calendar.Day
		dateParser	= do
			[y, m, d]	<- Text.Poly.spaces >> Poly.sepBy1 Text.Poly.unsignedDecimal (Text.Poly.char dateComponentSeparator)

			return {-to Parser-monad-} $ Data.Time.Calendar.fromGregorian (fromIntegral y) m d
	in Data.Maybe.maybe (
		Control.Exception.throw . Data.Exception.mkSearchFailure . showString "failed to find " $ show dateTag	-- N.B.: this will only terminate the application when the date is evaluated.
	) (
#	if USE_POLYPARSE != 'L'
		either (
			Control.Exception.throw . Data.Exception.mkParseFailure . showString "failed to parse " . shows dateTag . showString "; " . show
		) id .
#	endif
		fst . Poly.runParser dateParser
#else /* Parsec */
		dateParser :: Parsec.Parser Data.Time.Calendar.Day
		dateParser	= do
			[y, m, d]	<- Parsec.spaces >> (Parsec.sepBy1 (Control.Applicative.some Parsec.digit) (Parsec.char dateComponentSeparator)	<?> "YYYY.MM.DD")

			return {-to ParsecT-monad-} $ Data.Time.Calendar.fromGregorian (read y) (read m) (read d)
	in Data.Maybe.maybe (
		Control.Exception.throw . Data.Exception.mkSearchFailure . showString "failed to find " $ show dateTag	-- N.B.: this will only terminate the application when the date is evaluated.
	) (
		either (
			Control.Exception.throw . Data.Exception.mkParseFailure . showString "failed to parse " . shows dateTag . showString "; " . show
		) id . Parsec.parse dateParser "Date-parser"
#endif
	) maybeDate
 ) maybeRoundName maybeWhitePlayerName maybeBlackPlayerName $ filter (
	(`elem` identificationTags) . fst {-tag-}
 ) tagPairs where
	[maybeEventName, maybeSiteName, maybeDate, maybeRoundName, maybeWhitePlayerName, maybeBlackPlayerName]	= map (
		`Map.lookup` Map.fromList tagPairs
	 ) [
		eventNameTag,
		siteNameTag,
		dateTag,
		roundNameTag,
		whitePlayerNameTag,
		blackPlayerNameTag
	 ]

{- |
	* Represents the specified /game/ in /Portable Game Notation/; <https://www.chessclub.com/help/pgn-spec>.

	* This function is only responsible for the line defining the numbered sequence of /move/s represented in SAN.
-}
showsMoveText :: Model.Game.Game -> ShowS
showsMoveText game	= foldr (.) (
	Data.Maybe.maybe (
		showsBlockComment "Game unfinished" . showChar ' ' . showChar inProgressFlag
	) (
		(
			\(s, showsPGN) -> showsBlockComment s . showChar ' ' . showsPGN
		) . (
			filter (/= ContextualNotation.PGNComment.blockCommentEnd) . show &&& shows . Rule.GameTerminationReason.toResult
		)
	) $ Model.Game.getMaybeTerminationReason game
 ) $ Data.List.unfoldr (
	\(moveNumber, turns) -> case turns of
		showsWhiteMove : remainder	-> Just . (
			(
				\moveList -> shows moveNumber . showChar moveNumberTerminator . showChar ' ' . showsWhiteMove . Data.Maybe.maybe id (
					\showsBlackMove -> showChar ' ' . showsBlackMove
				) (
					Data.Maybe.listToMaybe moveList
				) . showChar ' '	-- Show an element.
			) *** (,) (succ moveNumber)
		 ) $ Data.List.splitAt 1 remainder
		_				-> Nothing
 ) (
	1	:: Type.Count.NMoves,
	reverse {-chronological-} $ fst . foldr (
		\turn (l, game')	-> (
			ContextualNotation.StandardAlgebraic.showsTurn False turn game' : l,
			Model.Game.takeTurn turn game'
		) -- Pair.
	) ([], Data.Default.def {-game-}) $ Model.Game.listTurns game
 ) where
	showsBlockComment :: String -> ShowS
	showsBlockComment	= shows . ContextualNotation.PGNComment.BlockComment

-- | Shows PGN for the specified /game/, with defaults for other fields.
showsGame :: Model.Game.Game -> IO ShowS
showsGame game	= do
	utcTime	<- Data.Time.Clock.getCurrentTime

	return {-to IO-monad-} . shows $ mkPGN Nothing Nothing (Data.Time.Clock.utctDay utcTime) Nothing Nothing Nothing [] game

-- | Parse a /Numeric Annotation Glyph/.
nagParser :: (
	Num	i,
	Ord	i,
	Show	i
 ) =>
#ifdef USE_POLYPARSE
	Text.Poly.TextParser i
nagParser	= do
	code	<- Control.Applicative.many ContextualNotation.PGNComment.blockCommentParser >> Text.Poly.spaces >> Text.Poly.char nagPrefix >> Text.Poly.unsignedDecimal

	if code > maxCode
		then Poly.failBad . shows code . showString " > maximum NAG" . Text.ShowList.showsAssociation $ shows maxCode "."
		else return {-to Parser-monad-} code
#else /* Parsec */
	Parsec.Parser i
nagParser	= Parsec.try $ do
	code	<- Control.Applicative.many ContextualNotation.PGNComment.blockCommentParser >> Parsec.spaces >> Parsec.char nagPrefix >> fmap Data.Integral.stringToUnsignedDecimal (Control.Applicative.some Parsec.digit)

	if code > maxCode
		then fail . shows code . showString " > maximum NAG" . Text.ShowList.showsAssociation $ shows maxCode "."
		else return {-to ParsecT-monad-} code
#endif
	where
		maxCode	= 255

{- |
	* Parses a move-number.

	* N.B.: officially terminated by a single period (for White), or three (by Black); though this parser is more flexible.
-}
moveNumberParser ::
#ifdef USE_POLYPARSE
	Text.Poly.TextParser Type.Count.NMoves
moveNumberParser	= Control.Applicative.many ContextualNotation.PGNComment.blockCommentParser >> Text.Poly.unsignedDecimal `Poly.discard` Control.Applicative.some (Text.Poly.char moveNumberTerminator)
#else /* Parsec */
	Parsec.Parser Type.Count.NMoves
moveNumberParser	= Parsec.try $ Control.Applicative.many ContextualNotation.PGNComment.blockCommentParser >> Parsec.spaces >> fmap Data.Integral.stringToUnsignedDecimal (
	Parsec.manyTill Parsec.digit (
		Control.Applicative.some $ Parsec.char moveNumberTerminator
	) <?> "Move-number"
 )
#endif

-- | Parse an optional result.
maybeResultParser ::
#ifdef USE_POLYPARSE
	Text.Poly.TextParser (Maybe Rule.Result.Result)
maybeResultParser	= Control.Applicative.many ContextualNotation.PGNComment.blockCommentParser >> Text.Poly.spaces >> Poly.oneOf' [
	(
		"Result",
		Poly.oneOf $ map (
			\result -> Text.Poly.string (show result) >> return {-to Parser-monad-} (Just result)	-- CAVEAT: for some reason, this conflates "1-0" with "1/2-1/2", when lazy-parsing with ghc-8.0.1 & polyparse-1.12 ?!
		) Property.FixedMembership.members
	), (
		"Game unfinished",
		Poly.commit (Text.Poly.char inProgressFlag) >> return {-to Parser-monad-} Nothing
	)
 ]
#else /* Parsec */
	Parsec.Parser (Maybe Rule.Result.Result)
maybeResultParser	= Control.Applicative.many ContextualNotation.PGNComment.blockCommentParser >> Parsec.spaces >> (
	Parsec.try (
		Parsec.choice $ map (
			\result -> Parsec.try $ (Parsec.string (show result) <?> "Result") >> return {-to ParsecT-monad-} (Just result)
		) Property.FixedMembership.members
	) <|> (
		(Parsec.char inProgressFlag <?> "Game unfinished") >> return {-to ParsecT-monad-} Nothing
	)
 )
#endif

-- | Parses a /game/ from PGN move-text.
moveTextParser
	:: IsStrictlySequential
	-> ContextualNotation.StandardAlgebraic.ValidateMoves
#ifdef USE_POLYPARSE
	-> Text.Poly.TextParser Model.Game.Game
moveTextParser isStrictlySequential validateMoves	= let
	elementSequenceParser :: Model.Game.Game -> Text.Poly.TextParser Model.Game.Game
	elementSequenceParser game	= let
		expectedMoveNumber :: Type.Count.NMoves
		expectedMoveNumber	= State.TurnsByLogicalColour.deriveMoveNumber $ Model.Game.getTurnsByLogicalColour game
	 in do
		moveNumber	<- (
			if Colour.LogicalColour.isBlack $ Model.Game.getNextLogicalColour game
				then fmap (Data.Maybe.fromMaybe expectedMoveNumber) . Control.Applicative.optional
				else id
		 ) moveNumberParser

		if isStrictlySequential && moveNumber /= expectedMoveNumber
			then do
				context	<- Poly.manyFinally' Poly.next $ Text.Poly.char '\n'

				Poly.failBad . showString "found " . shows (Model.Game.getNextLogicalColour game) . showString " move-number" . Text.ShowList.showsAssociation . shows moveNumber . showString " where " . shows expectedMoveNumber . showString " expected, before " $ shows context "."
			else do
				game'	<- fmap (`ContextualNotation.StandardAlgebraic.movePiece` game) $ Control.Applicative.many ContextualNotation.PGNComment.blockCommentParser >> ContextualNotation.StandardAlgebraic.parser False {-explicitEnPassant-} validateMoves game

				Control.Applicative.many (
					nagParser :: Text.Poly.TextParser Int
				 ) >> Control.Applicative.many (
					uncurry Poly.bracket (
						(
							\c -> Control.Applicative.many ContextualNotation.PGNComment.blockCommentParser >> Text.Poly.spaces >> Text.Poly.char c
						) *** (
							\c -> Poly.commit $ Control.Applicative.many ContextualNotation.PGNComment.blockCommentParser >> Text.Poly.spaces >> Text.Poly.char c
						) $ ravDelimiters
					) $ elementSequenceParser game {-recurse-}
				 ) >> fmap (Data.Maybe.fromMaybe game') (
					Control.Applicative.optional $ elementSequenceParser game' {-recurse-}
				 )
 in do
	game	<- fmap (Data.Maybe.fromMaybe Data.Default.def {-game-}) . Control.Applicative.optional $ elementSequenceParser Data.Default.def {-game-}
#else /* Parsec */
	-> Parsec.Parser Model.Game.Game
moveTextParser isStrictlySequential validateMoves	= let
	elementSequenceParser :: Model.Game.Game -> Parsec.Parser Model.Game.Game
	elementSequenceParser game	= let
		expectedMoveNumber :: Type.Count.NMoves
		expectedMoveNumber	= State.TurnsByLogicalColour.deriveMoveNumber $ Model.Game.getTurnsByLogicalColour game
	 in do
		moveNumber	<- (
			if Colour.LogicalColour.isBlack $ Model.Game.getNextLogicalColour game
				then fmap (Data.Maybe.fromMaybe expectedMoveNumber) . Control.Applicative.optional
				else id
		 ) moveNumberParser

		if isStrictlySequential && moveNumber /= expectedMoveNumber
			then fail . showString "found " . shows (Model.Game.getNextLogicalColour game) . showString " move-number" . Text.ShowList.showsAssociation . shows moveNumber . showString " where " $ shows expectedMoveNumber " expected."
			else do
				game'	<- (`ContextualNotation.StandardAlgebraic.movePiece` game) <$> (Control.Applicative.many ContextualNotation.PGNComment.blockCommentParser >> ContextualNotation.StandardAlgebraic.parser False {-explicitEnPassant-} validateMoves game)

				Control.Applicative.many (
					nagParser :: Parsec.Parser Int
				 ) >> Control.Applicative.many (
					Parsec.try . uncurry Parsec.between (
						(
							\c -> Control.Applicative.many ContextualNotation.PGNComment.blockCommentParser >> Parsec.spaces >> Parsec.char c
						) *** (
							\c -> Control.Applicative.many ContextualNotation.PGNComment.blockCommentParser >> Parsec.spaces >> Parsec.char c
						) $ ravDelimiters
					) $ elementSequenceParser game {-recurse-}
				 ) >> Parsec.option game' (
					Parsec.try $ elementSequenceParser game' {-recurse-}
				 )
 in do
	game	<- Parsec.option Data.Default.def {-game-} . Parsec.try $ elementSequenceParser Data.Default.def {-game-}
#endif
	Data.Maybe.maybe game (`Model.Game.updateTerminationReasonWith` game) <$> maybeResultParser

{- |
	* Parses /PGN/.

	* CAVEAT: this function doesn't /produce/ when using either "Parsec" or "Poly.Plain", since it returns 'Either' the appropriate constructor for which may be unknown until the last character is parsed.
	Equally for these parsers, all data must be strictly evaluated before any data can retrieved.
-}
parser
	:: IsStrictlySequential
	-> ContextualNotation.StandardAlgebraic.ValidateMoves
	-> [Tag]	-- ^ Identify fields used to form a unique composite game-identifier.
#ifdef USE_POLYPARSE
	-> Text.Poly.TextParser PGN
parser isStrictlySequential validateMoves identificationTags	= do
	tagPairs	<- Control.Applicative.many $ tagPairParser <* Control.Applicative.many ContextualNotation.PGNComment.parser
	game		<- moveTextParser' <* Control.Applicative.many ContextualNotation.PGNComment.parser

	return {-to Parser-monad-} $ mkPGN' identificationTags (removeUnknownTagValues tagPairs) game

	where
		tagPairParser :: Text.Poly.TextParser TagPair
		tagPairParser	= Control.Applicative.many ContextualNotation.PGNComment.parser >> Text.Poly.spaces >> uncurry Poly.bracket (
			Text.Poly.char *** Poly.commit . Text.Poly.char $ tagPairDelimiters
		 ) (
			do
				tagName		<- Control.Applicative.many ContextualNotation.PGNComment.blockCommentParser >> Text.Poly.spaces >> Poly.commit (Control.Applicative.some $ Poly.satisfyMsg isValidTagCharacter "Tag-name")
				tagValue	<- Control.Applicative.many ContextualNotation.PGNComment.blockCommentParser >> Text.Poly.spaces >> Poly.bracket (
					Text.Poly.char quoteDelimiter
				 ) (
					Poly.commit $ Text.Poly.char quoteDelimiter
				 ) (
					Control.Applicative.many $ Poly.satisfyMsg (/= quoteDelimiter) "Tag-value"	-- TODO: must account for escaped quotes.
				 )

				Control.Applicative.many ContextualNotation.PGNComment.blockCommentParser >> Text.Poly.spaces

				return {-to Parser-monad-} (tagName, tagValue)
		 )
#else /* Parsec */
	-> Parsec.Parser PGN
parser isStrictlySequential validateMoves identificationTags	= mkPGN' identificationTags <$> (
	removeUnknownTagValues <$> Control.Applicative.many (
		tagPairParser <* Control.Applicative.many ContextualNotation.PGNComment.parser
	)
 ) <*> moveTextParser' <* Control.Applicative.many ContextualNotation.PGNComment.parser
	where
		tagPairParser :: Parsec.Parser TagPair
		tagPairParser	= Parsec.try $ Control.Applicative.many ContextualNotation.PGNComment.parser >> Parsec.spaces >> uncurry Parsec.between (
			Parsec.char *** Parsec.char $ tagPairDelimiters
		 ) (
			do
				tagName		<- Control.Applicative.many ContextualNotation.PGNComment.blockCommentParser >> Parsec.spaces >> (Control.Applicative.some (Parsec.satisfy isValidTagCharacter) <?> "Tag-name")
				tagValue	<- Control.Applicative.many ContextualNotation.PGNComment.blockCommentParser >> Parsec.spaces >> Parsec.between (
					Parsec.char quoteDelimiter
				 ) (
					Parsec.char quoteDelimiter
				 ) (
					Control.Applicative.many (Parsec.satisfy (/= quoteDelimiter))	<?> "Tag-value"	-- TODO: must account for escaped quotes.
				 )

				Control.Applicative.many ContextualNotation.PGNComment.blockCommentParser >> Parsec.spaces

				return {-to ParsecT-monad-} (tagName, tagValue)
		 )
#endif
		moveTextParser'	= moveTextParser isStrictlySequential validateMoves

		isValidTagCharacter :: Char -> Bool
		isValidTagCharacter	= uncurry (&&) . (not . Data.Char.isSpace &&& (/= quoteDelimiter))

		removeUnknownTagValues :: [TagPair] -> [TagPair]
		removeUnknownTagValues	= filter ((/= [unknownTagValue]) . snd {-tag-value-})

