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

 [@DESCRIPTION@]	<https://en.wikipedia.org/wiki/Algebraic_notation_(chess)>
-}

module BishBosh.ContextualNotation.StandardAlgebraic(
-- * Types
-- ** Type-synonyms
	ValidateMoves,
	ExplicitEnPassant,
-- ** Data-types
	StandardAlgebraic(
--		MkStandardAlgebraic
		getQualifiedMove
	),
-- * Constants
--	captureFlag,
--	checkFlag,
--	checkMateFlag,
--	promotionFlag,
--	enPassantToken,
--	longCastleToken,
--	shortCastleToken,
--	moveSuffixAnnotations,
--	xMin,
--	yMin,
--	xMax,
--	yMax,
--	xOrigin,
--	yOrigin,
-- * Functions
--	encode,
	showsCoordinates,
	showsTurn,
	showTurn,
	showsMove,
	showMove,
	movePiece,
--	rankParser,
--	abscissaParser,
--	ordinateParser,
--	coordinatesParser,
--	captureParser,
--	moveSuffixAnnotationParser,
	parser,
	fromRank,
	toRank,
-- ** Constructors
	fromQualifiedMove
-- ** Predicates
--	inXRange,
--	inYRange
) where

import			Control.Arrow((&&&), (***))
import			Data.Array.IArray((!))
import qualified	BishBosh.Attribute.MoveType		as Attribute.MoveType
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa		as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate		as Cartesian.Ordinate
import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Component.Piece		as Component.Piece
import qualified	BishBosh.Component.QualifiedMove	as Component.QualifiedMove
import qualified	BishBosh.Component.Turn			as Component.Turn
import qualified	BishBosh.Data.Exception			as Data.Exception
import qualified	BishBosh.Model.Game			as Model.Game
import qualified	BishBosh.Model.GameTerminationReason	as Model.GameTerminationReason
import qualified	BishBosh.Property.ForsythEdwards	as Property.ForsythEdwards
import qualified	BishBosh.State.Board			as State.Board
import qualified	BishBosh.State.MaybePieceByCoordinates	as State.MaybePieceByCoordinates
import qualified	BishBosh.Text.ShowList			as Text.ShowList
import qualified	BishBosh.Types				as T
import qualified	Control.Applicative
import qualified	Control.Exception
import qualified	Control.Monad
import qualified	Data.Char
import qualified	Data.List
import qualified	Data.Maybe

#ifdef USE_POLYPARSE
import qualified	BishBosh.Text.Poly			as Text.Poly
#if USE_POLYPARSE == 1
import qualified	Text.ParserCombinators.Poly.Lazy	as Poly
#else /* Plain */
import qualified	Text.ParserCombinators.Poly.Plain	as Poly
#endif
#else /* Parsec */
import qualified	Text.ParserCombinators.Parsec		as Parsec
import			Text.ParserCombinators.Parsec((<?>), (<|>))
#endif

-- | Whether each move should be validated.
type ValidateMoves	= Bool

-- | Constant indication of capture.
captureFlag :: Char
captureFlag		= 'x'

-- | Constant indication of Check.
checkFlag :: Char
checkFlag		= '+'

-- | Constant indication of Check-mate.
checkMateFlag :: Char
checkMateFlag		= '#'

-- | Constant indication of promotion.
promotionFlag :: Char
promotionFlag		= '='

-- | Constant indication of En-passant.
enPassantToken :: String
enPassantToken		= "e.p."

-- | Constant indication of a long @Queen@-side Castle.
longCastleToken :: String
longCastleToken		= "O-O-O"

-- | Constant indication of a short @King@-side Castle.
shortCastleToken :: String
shortCastleToken	= "O-O"

{- |
	* The characters which may be used to annotate a half move.

	* Zero to two of these (including duplicates) may follow each half move, but the parser intentionally permits any number.

	* CAVEAT: the parser intentionally permits any number of annotations.
-}
moveSuffixAnnotations :: String
moveSuffixAnnotations	= "!?"

-- | The minimum permissible values for /x/ & /y/ coordinates.
min' :: (Char, Char)
xMin, yMin :: Char
min'@(xMin, yMin)	= ('a', '1')

-- | The origin of the coordinate-system.
origin :: (Int, Int)
xOrigin, yOrigin :: Int
origin@(xOrigin, yOrigin)	= Data.Char.ord *** Data.Char.ord $ min'

-- | The maximum permissible values for /x/ & /y/ coordinates.
xMax, yMax :: Char
(xMax, yMax)	= Data.Char.chr . (
	+ pred {-fence-post-} (fromIntegral Cartesian.Abscissa.xLength)
 ) *** Data.Char.chr . (
	+ pred {-fence-post-} (fromIntegral Cartesian.Ordinate.yLength)
 ) $ origin

-- | Whether the specified character is a valid abscissa.
inXRange :: Char -> Bool
inXRange	= uncurry (&&) . ((>= xMin) &&& (<= xMax))

-- | Whether the specified character is a valid ordinate.
inYRange :: Char -> Bool
inYRange	= uncurry (&&) . ((>= yMin) &&& (<= yMax))

-- | Defines a /move/, to enable i/o in /StandardAlgebraic/-notation.
newtype StandardAlgebraic x y	= MkStandardAlgebraic {
	getQualifiedMove	:: Component.QualifiedMove.QualifiedMove x y
} deriving (Eq, Show)

-- | Constructor.
fromQualifiedMove :: Component.QualifiedMove.QualifiedMove x y -> StandardAlgebraic x y
fromQualifiedMove	= MkStandardAlgebraic

-- | Encodes the ordinate & abscissa.
encode :: (Enum x, Enum y) => Cartesian.Coordinates.Coordinates x y -> (ShowS, ShowS)
encode	= showChar . Data.Char.chr . (+ (xOrigin - Cartesian.Abscissa.xOrigin)) . fromEnum . Cartesian.Coordinates.getX &&& showChar . Data.Char.chr . (+ (yOrigin - Cartesian.Ordinate.yOrigin)) . fromEnum . Cartesian.Coordinates.getY

-- | Shows the specified /coordinates/.
showsCoordinates :: (Enum x, Enum y) => Cartesian.Coordinates.Coordinates x y -> ShowS
showsCoordinates	= uncurry (.) . encode

-- | Whether en-passant moves are tagged, or implicit.
type ExplicitEnPassant	= Bool

-- | Represent the specified /turn/ in SAN.
showsTurn :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Show	x,
	Show	y
 )
	=> ExplicitEnPassant
	-> Component.Turn.Turn x y
	-> Model.Game.Game x y	-- ^ The /game/ prior to application of the specified /turn/.
	-> ShowS
{-# SPECIALISE showsTurn :: ExplicitEnPassant -> Component.Turn.Turn T.X T.Y -> Model.Game.Game T.X T.Y -> ShowS #-}
showsTurn explicitEnPassant turn game
	| Just sourceRank <- fmap Component.Piece.getRank . State.MaybePieceByCoordinates.dereference source $ State.Board.getMaybePieceByCoordinates board	= (
		if sourceRank == Attribute.Rank.Pawn
			then (
				if isCapture
					then showsX . showsCapture
					else id
			) . showsDestination . if isEnPassant
				then if explicitEnPassant
					then showString enPassantToken
					else id
				else Data.Maybe.maybe id (
					\promotionRank -> showChar promotionFlag . showsRank promotionRank
				) $ Attribute.Rank.getMaybePromotionRank moveType
			else {-not a Pawn-} case moveType of
				Attribute.MoveType.Castle isShort	-> showString $ if isShort
					then shortCastleToken
					else longCastleToken
				_ {-not a castling-}			-> showsRank sourceRank . (
					case Data.List.delete source {-search for alternatives-} $ State.Board.findAttacksBy (
						Component.Piece.mkPiece (Model.Game.getNextLogicalColour game) sourceRank
					) destination board of
						[]		-> id	-- There're aren't any pieces of this rank which can perform this move.
						coordinates	-> case any (
							(== Cartesian.Coordinates.getX source) . Cartesian.Coordinates.getX
						 ) &&& any (
							(== Cartesian.Coordinates.getY source) . Cartesian.Coordinates.getY
						 ) $ coordinates of
							(True, True)	-> showsX . showsY	-- There're other pieces of this rank, some with similar X-coordinate & some with similar Y-coordinate.
							(_, False)	-> showsY		-- There's another piece of this rank & X-coordinate; specify Y-coordinate to disambiguate.
							_		-> showsX		-- There's anoher piece of this rank, but neither X nor Y coordinates are similar.
				 ) . (
					if isCapture
						then showsCapture
						else id
				 ) . showsDestination
	) . (
		if Data.Maybe.isJust $ Model.Game.getMaybeChecked game'
			then showChar $ if Data.Maybe.maybe False Model.GameTerminationReason.isCheckMate $ Model.Game.getMaybeTerminationReason game'
				then checkMateFlag
				else checkFlag
			else id
	)
	| otherwise	= Control.Exception.throw . Data.Exception.mkSearchFailure . showString "BishBosh.ContextualNotation.StandardAlgebraic.showsTurn:\tno piece exists at " . showsCoordinates source . showString "; " $ Property.ForsythEdwards.showsFEN game "."
	where
		((source, destination), moveType)	= (Component.Move.getSource &&& Component.Move.getDestination) . Component.QualifiedMove.getMove &&& Component.QualifiedMove.getMoveType $ Component.Turn.getQualifiedMove turn
		board					= Model.Game.getBoard game

		isEnPassant, isCapture :: Bool
		isEnPassant	= Attribute.MoveType.isEnPassant moveType
		isCapture	= State.MaybePieceByCoordinates.isOccupied destination (State.Board.getMaybePieceByCoordinates board) || isEnPassant

		showsRank :: Attribute.Rank.Rank -> ShowS
		showsRank rank	= showChar $ fromRank rank

		showsCapture, showsX, showsY, showsDestination :: ShowS
		showsCapture		= showChar captureFlag
		(showsX, showsY)	= encode source
		showsDestination	= showsCoordinates destination

		game'	= Model.Game.takeTurn turn game

-- | Calls 'showsTurn'.
showTurn :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Show	x,
	Show	y
 )
	=> ExplicitEnPassant
	-> Component.Turn.Turn x y
	-> Model.Game.Game x y	-- ^ The /game/ prior to application of the specified /turn/.
	-> String
{-# SPECIALISE showTurn :: ExplicitEnPassant -> Component.Turn.Turn T.X T.Y -> Model.Game.Game T.X T.Y -> String #-}
showTurn explicitEnPassant turn game	= showsTurn explicitEnPassant turn game ""

-- | A convenience-function, which generates the /turn/ required to call 'showsTurn'.
showsMove :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Show	x,
	Show	y
 )
	=> ExplicitEnPassant
	-> Component.QualifiedMove.QualifiedMove x y
	-> Model.Game.Game x y
	-> ShowS
{-# SPECIALISE showsMove :: ExplicitEnPassant -> Component.QualifiedMove.QualifiedMove T.X T.Y -> Model.Game.Game T.X T.Y -> ShowS #-}
showsMove explicitEnPassant qualifiedMove game	= showsTurn explicitEnPassant (
	Data.Maybe.fromMaybe (
		Control.Exception.throw $ Data.Exception.mkResultUndefined "BishBosh.ContextualNotation.StandardAlgebraic.showsMove:\tModel.Game.maybeLastTurn failed."
	) . Model.Game.maybeLastTurn $ Model.Game.applyQualifiedMove qualifiedMove game
 ) game

-- | Calls 'showsMove'.
showMove :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Show	x,
	Show	y
 )
	=> ExplicitEnPassant
	-> Component.QualifiedMove.QualifiedMove x y
	-> Model.Game.Game x y
	-> String
{-# SPECIALISE showMove :: ExplicitEnPassant -> Component.QualifiedMove.QualifiedMove T.X T.Y -> Model.Game.Game T.X T.Y -> String #-}
showMove explicitEnPassant qualifiedMove game	= showsMove explicitEnPassant qualifiedMove game ""

-- | Applies the specified /move/ to the specified /game/.
movePiece :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Show	x,
	Show	y
 ) => StandardAlgebraic x y -> Model.Game.Transformation x y
{-# SPECIALISE movePiece :: StandardAlgebraic T.X T.Y -> Model.Game.Transformation T.X T.Y #-}
movePiece MkStandardAlgebraic { getQualifiedMove = qualifiedMove }	= Model.Game.applyQualifiedMove qualifiedMove

#ifdef USE_POLYPARSE
-- | Parse the /rank/ of the /piece/ being moved.
rankParser :: Text.Poly.TextParser Attribute.Rank.Rank
rankParser	= toRank `fmap` Poly.satisfyMsg (`elem` map fromRank Attribute.Rank.pieces) Attribute.Rank.tag

-- | Parse an /x/-coordinate.
abscissaParser :: Enum x => Text.Poly.TextParser x
{-# SPECIALISE abscissaParser :: Text.Poly.TextParser T.X #-}
abscissaParser	= (
	toEnum . (+ (Cartesian.Abscissa.xOrigin - xOrigin)) . Data.Char.ord
 ) `fmap` Poly.satisfyMsg inXRange "Abscissa"

-- | Parse a /y/-coordinate.
ordinateParser :: Enum y => Text.Poly.TextParser y
{-# SPECIALISE ordinateParser :: Text.Poly.TextParser T.Y #-}
ordinateParser	= (
	toEnum . (+ (Cartesian.Ordinate.yOrigin - yOrigin)) . Data.Char.ord
 ) `fmap` Poly.satisfyMsg inYRange "Ordinate"

-- | Parse a pair of /coordinates/.
coordinatesParser :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => Text.Poly.TextParser (Cartesian.Coordinates.Coordinates x y)
{-# SPECIALISE coordinatesParser :: Text.Poly.TextParser (Cartesian.Coordinates.Coordinates T.X T.Y) #-}
coordinatesParser	= do
	x	<- abscissaParser
	y	<- ordinateParser

	return {-to Parser-monad-} $ Cartesian.Coordinates.mkCoordinates x y

-- | Parse the flag which denotes capture.
captureParser :: Text.Poly.TextParser Char
captureParser	= Poly.satisfyMsg (== captureFlag) "Capture"
#else
-- | Parse the /rank/ of the /piece/ being moved.
rankParser :: Parsec.Parser Attribute.Rank.Rank
rankParser	= toRank <$> Parsec.oneOf (map fromRank Attribute.Rank.pieces) <?> Attribute.Rank.tag

-- | Parse an /x/-coordinate.
abscissaParser :: Enum x => Parsec.Parser x
{-# SPECIALISE abscissaParser :: Parsec.Parser T.X #-}
abscissaParser	= (
	toEnum . (+ (Cartesian.Abscissa.xOrigin - xOrigin)) . Data.Char.ord
 ) <$> Parsec.satisfy inXRange <?> "Abscissa"

-- | Parse a /y/-coordinate.
ordinateParser :: Enum y => Parsec.Parser y
{-# SPECIALISE ordinateParser :: Parsec.Parser T.X #-}
ordinateParser	= (
	toEnum . (+ (Cartesian.Ordinate.yOrigin - yOrigin)) . Data.Char.ord
 ) <$> Parsec.satisfy inYRange <?> "Ordinate"

-- | Parse a pair of /coordinates/.
coordinatesParser :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y
 ) => Parsec.Parser (Cartesian.Coordinates.Coordinates x y)
{-# SPECIALISE coordinatesParser :: Parsec.Parser (Cartesian.Coordinates.Coordinates T.X T.Y) #-}
coordinatesParser	= Cartesian.Coordinates.mkCoordinates <$> abscissaParser <*> ordinateParser

-- | Parse the flag which denotes capture.
captureParser :: Parsec.Parser ()
captureParser	= Control.Monad.void (Parsec.char captureFlag <?> "Capture")
#endif

-- | Parse a Move Suffix-annotation.
moveSuffixAnnotationParser ::
#ifdef USE_POLYPARSE
	Text.Poly.TextParser String
moveSuffixAnnotationParser	= Text.Poly.spaces >> Control.Applicative.some (Poly.oneOf' $ map (\c -> ([c], Poly.satisfyMsg (== c) "Move Suffix-annotation")) moveSuffixAnnotations)
#else /* Parsec */
	Parsec.Parser String
moveSuffixAnnotationParser	= Parsec.try (
	Parsec.spaces >> Control.Applicative.some (Parsec.choice $ map Parsec.char moveSuffixAnnotations)	<?> "Move Suffix-annotation"
 )
#endif

-- | Parses a /move/ from SAN, & optionally validates it against the specified /game/.
parser :: (
	Enum	x,
	Enum	y,
	Ord	x,
	Ord	y,
	Show	x,
	Show	y
 )
	=> ExplicitEnPassant
	-> ValidateMoves
	-> Model.Game.Game x y
#ifdef USE_POLYPARSE
	-> Text.Poly.TextParser (StandardAlgebraic x y)
{-# SPECIALISE parser :: ExplicitEnPassant -> ValidateMoves -> Model.Game.Game T.X T.Y -> Text.Poly.TextParser (StandardAlgebraic T.X T.Y) #-}
parser explicitEnPassant validateMoves game	= let
	nextLogicalColour			= Model.Game.getNextLogicalColour game
	(shortCastlingMoves, longCastlingMoves)	= Data.List.partition (\(Attribute.MoveType.Castle isShort, _, _) -> isShort) $ Component.Move.castlingMovesByLogicalColour ! nextLogicalColour
	board					= Model.Game.getBoard game
	getMaybePiece				= (`State.MaybePieceByCoordinates.dereference` State.Board.getMaybePieceByCoordinates board)
	getMaybeRank				= fmap Component.Piece.getRank . getMaybePiece
 in do
	qualifiedMove	<- Text.Poly.spaces >> Poly.oneOf' [
		(
			"Non-castling move",
			do
				rank	<- Data.Maybe.fromMaybe Attribute.Rank.Pawn `fmap` Control.Applicative.optional rankParser

				let
					piece :: Component.Piece.Piece
					piece	= Component.Piece.mkPiece nextLogicalColour rank

					findAttacksBy destination	= State.Board.findAttacksBy piece destination board

				if rank == Attribute.Rank.Pawn
					then let
						promotionParser :: Text.Poly.TextParser Attribute.Rank.Rank
						promotionParser	= Text.Poly.char promotionFlag >> rankParser
					in Poly.oneOf' [
						(
							"Pawn-advance",
							do
								destination	<- coordinatesParser

								Data.Maybe.maybe (
									do
										context	<- Poly.manyFinally' Poly.next $ Text.Poly.char '\n'

										Poly.failBad . showString "failed to locate any " . shows piece . showString " which can advance to " . shows destination . showString ". Before " $ shows context "."
								 ) (
									\source -> (
										Component.QualifiedMove.mkQualifiedMove (Component.Move.mkMove source destination) . Attribute.MoveType.mkNormalMoveType Nothing {-capture-}
									) `fmap` Control.Applicative.optional promotionParser
								 ) . Data.List.find (
									(== Just piece) . getMaybePiece
								 ) . Data.Maybe.catMaybes . take 2 {-maximum Pawn-advance-} . tail {-drop the original-} $ iterate (
									>>= Cartesian.Coordinates.maybeRetreat nextLogicalColour
								 ) $ Just destination
						), (
							"Pawn-capture",
							do
								x		<- abscissaParser
								_		<- captureParser
								destination	<- Poly.commit coordinatesParser

								let maybeDestinationRank	= getMaybeRank destination

								Data.Maybe.maybe (
									do
										context	<- Poly.manyFinally' Poly.next $ Text.Poly.char '\n'

										Poly.failBad . showString "failed to locate any " . shows piece . showString " which can capture " . shows destination . showString " from abscissa" . Text.ShowList.showsAssociation . shows x . showString ". Before " $ shows context "."
								 ) (
									\source -> Component.QualifiedMove.mkQualifiedMove (
										Component.Move.mkMove source destination
									) `fmap` Poly.oneOf' [
										(
											"En-passant",
											do
												if explicitEnPassant
													then Text.Poly.string enPassantToken
													else Control.Monad.when (Data.Maybe.isJust maybeDestinationRank) $ fail undefined

												return {-to Parser-monad-} Attribute.MoveType.enPassant
										), (
											"Normal pawn capture",
											Poly.commit $ Attribute.MoveType.mkNormalMoveType maybeDestinationRank `fmap` Control.Applicative.optional promotionParser
										)
									]
								 ) . Data.List.find (
									(== x) . Cartesian.Coordinates.getX
								 ) $ findAttacksBy destination
						)
					]
					else {-not a Pawn-} let
						mkNormalMoveType destination	= Attribute.MoveType.mkNormalMoveType (getMaybeRank destination) Nothing {-promotion-}

						resolveQualifiedMove destination candidates	= case candidates of
							[]			-> do
								context	<- Poly.manyFinally' Poly.next $ Text.Poly.char '\n'

								Poly.failBad . showString "failed to locate any " . shows piece . showString " able to move to " . shows destination . showString ". Before " $ shows context "."
							[source]		-> return {-to Parser-monad-} . Component.QualifiedMove.mkQualifiedMove (Component.Move.mkMove source destination) $ mkNormalMoveType destination
							sourceCandidates	-> Poly.oneOf' [
								show &&& return {-to Parser-monad-} $ qualifiedMove |
									source	<- sourceCandidates,-- Attempt to resolve the ambiguity by playing subsequent moves.
									let qualifiedMove	= Component.QualifiedMove.mkQualifiedMove (Component.Move.mkMove source destination) $ mkNormalMoveType destination,
									Model.Game.isValidQualifiedMove qualifiedMove game
							 ] -- List-comprehension.
					in Poly.oneOf' [
						(
							"Fully qualified move",	-- N.B. this scenario occurs when there are identical pieces on both the same row & the same column, as the intended attacker; i.e. after a promotion.
							do
								source		<- coordinatesParser
								destination	<- Control.Applicative.optional captureParser >> coordinatesParser

								return {-to Parser-monad-} . Component.QualifiedMove.mkQualifiedMove (Component.Move.mkMove source destination) $ mkNormalMoveType destination
						), (
							"Partially qualified move",	-- This scenario occurs if there's an identical piece on either the same row or the same column, as the intended attacker.
							do
								sourceFilter	<- Poly.oneOf' [
									(
										"Abscissa qualification",
										(
											\x -> filter $ (== x) . Cartesian.Coordinates.getX
										) `fmap` abscissaParser
									), (
										"Ordinate qualification",
										(
											\y -> filter $ (== y) . Cartesian.Coordinates.getY
										) `fmap` ordinateParser
									)
								 ] -- Build a filter from the source-qualifier.

								destination	<- Control.Applicative.optional captureParser >> coordinatesParser

								resolveQualifiedMove destination . sourceFilter $ findAttacksBy destination
						), (
							"Unqualified move",	-- The most likely scenario, where the intended attacker is unambiguous.
							Poly.commit $ do
								destination	<- Control.Applicative.optional captureParser >> coordinatesParser

								resolveQualifiedMove destination $ findAttacksBy destination
						)
					]
		), (
			"Long castle",
			Text.Poly.string longCastleToken >> Data.Maybe.maybe (
				fail "Failed to find any appropriate long castling move."
			) (
				\(moveType, kingsMove, _) -> return {-to Parser-monad-} $ Component.QualifiedMove.mkQualifiedMove kingsMove moveType
			) (
				Data.Maybe.listToMaybe longCastlingMoves
			)
		), (
			"Short castle",
			Text.Poly.string shortCastleToken >> Data.Maybe.maybe (
				fail "Failed to find any appropriate short castling move."
			) (
				\(moveType, kingsMove, _) -> return {-to Parser-monad-} $ Component.QualifiedMove.mkQualifiedMove kingsMove moveType
			) (
				Data.Maybe.listToMaybe shortCastlingMoves
			)
		) -- TODO: for some reason, lazy-parsing with ghc-8.0.1 & polyparse-1.12 conflates "O-O-O" with "O-O"; confirm.
	 ]

	_	<- Control.Applicative.optional (Poly.satisfyMsg (`elem` [checkFlag, checkMateFlag]) "Check") >> Control.Applicative.optional moveSuffixAnnotationParser

	fmap fromQualifiedMove $ if validateMoves
		then Data.Maybe.maybe (return {-to Parser-monad-} qualifiedMove) (Poly.failBad . showString "failed: ") $ Model.Game.validateQualifiedMove qualifiedMove game
		else return {-to Parser-monad-} qualifiedMove
#else /* Parsec */
	-> Parsec.Parser (StandardAlgebraic x y)
{-# SPECIALISE parser :: ExplicitEnPassant -> ValidateMoves -> Model.Game.Game T.X T.Y -> Parsec.Parser (StandardAlgebraic T.X T.Y) #-}
parser explicitEnPassant validateMoves game	= let
	nextLogicalColour			= Model.Game.getNextLogicalColour game
	(shortCastlingMoves, longCastlingMoves)	= Data.List.partition (\(Attribute.MoveType.Castle isShort, _, _) -> isShort) $ Component.Move.castlingMovesByLogicalColour ! nextLogicalColour
	board					= Model.Game.getBoard game
	getMaybePiece				= (`State.MaybePieceByCoordinates.dereference` State.Board.getMaybePieceByCoordinates board)
	getMaybeRank				= fmap Component.Piece.getRank . getMaybePiece
 in do
	qualifiedMove	<- Parsec.spaces >> Parsec.choice [
		do
			rank	<- Parsec.option Attribute.Rank.Pawn rankParser

			let
				piece :: Component.Piece.Piece
				piece	= Component.Piece.mkPiece nextLogicalColour rank

				findAttacksBy destination	= State.Board.findAttacksBy piece destination board

			if rank == Attribute.Rank.Pawn
				then let
					promotionParser :: Parsec.Parser Attribute.Rank.Rank
					promotionParser	= (Parsec.char promotionFlag <?> "Promotion") >> rankParser
				in Parsec.try (
					do
						destination	<- coordinatesParser	<?> "Destination"

						Data.Maybe.maybe (
							fail . showString "Failed to locate any " . shows piece . showString " which can advance to " $ shows destination "."
						 ) (
							\source -> (
								Component.QualifiedMove.mkQualifiedMove (Component.Move.mkMove source destination) . Attribute.MoveType.mkNormalMoveType Nothing {-capture-}
							) <$> Control.Applicative.optional promotionParser
						 ) . Data.List.find (
							Data.Maybe.maybe False {-no piece-} (== piece) . getMaybePiece
						 ) . Data.Maybe.catMaybes . take 2 {-maximum Pawn-advance-} . tail {-drop the original-} $ iterate (
							>>= Cartesian.Coordinates.maybeRetreat nextLogicalColour
						 ) $ Just destination
				) <|> do
					x		<- abscissaParser <* captureParser
					destination	<- coordinatesParser	<?> "Destination"

					let maybeDestinationRank	= getMaybeRank destination

					Data.Maybe.maybe (
						fail . showString "Failed to locate any " . shows piece . showString " which can capture " . shows destination . showString " from abscissa" . Text.ShowList.showsAssociation $ shows x "."
					 ) (
						\source -> fmap (
							Component.QualifiedMove.mkQualifiedMove (Component.Move.mkMove source destination)
						) $ (
							do
								_	<- if explicitEnPassant
									then Parsec.string enPassantToken	<?> "En-passant"
									else if Data.Maybe.isNothing maybeDestinationRank
										then return {-to ParsecT-monad-} enPassantToken
										else fail undefined

								return {-to ParsecT-monad-} Attribute.MoveType.enPassant
						) <|> (
							Attribute.MoveType.mkNormalMoveType maybeDestinationRank <$> Control.Applicative.optional promotionParser
						)
					 ) . Data.List.find (
						(== x) . Cartesian.Coordinates.getX
					 ) $ findAttacksBy destination
				else {-not a Pawn-} let
					mkNormalMoveType destination	= Attribute.MoveType.mkNormalMoveType (getMaybeRank destination) Nothing {-promotion-}

					resolveQualifiedMove destination candidates	= case candidates of
						[]			-> fail . showString "Failed to locate any " . shows piece . showString " able to move to " $ shows destination "."
						[source]		-> return {-to ParsecT-monad-} . Component.QualifiedMove.mkQualifiedMove (Component.Move.mkMove source destination) $ mkNormalMoveType destination
						sourceCandidates	-> Parsec.choice [
							Parsec.try $ return {-to ParsecT-monad-} qualifiedMove |
								source	<- sourceCandidates,-- Attempt to resolve the ambiguity by playing subsequent moves.
								let qualifiedMove	= Component.QualifiedMove.mkQualifiedMove (Component.Move.mkMove source destination) $ mkNormalMoveType destination,
								Model.Game.isValidQualifiedMove qualifiedMove game
						 ] -- List-comprehension.
				in Parsec.choice [
					Parsec.try $ do -- N.B. this scenario occurs when there are identical pieces on both the same row & the same column, as the intended attacker; i.e. after a promotion.
						source		<- coordinatesParser	<?> "Source"

						Parsec.optional captureParser		<?> "Optional capture"

						destination	<- coordinatesParser	<?> "Destination"

						return {-to ParsecT-monad-} . Component.QualifiedMove.mkQualifiedMove (Component.Move.mkMove source destination) $ mkNormalMoveType destination,
					Parsec.try $ do	-- This scenario occurs if there's an identical piece on either the same row or the same column, as the intended attacker.
						sourceFilter	<- (
							(
								\x -> filter $ (== x) . Cartesian.Coordinates.getX
							) <$> abscissaParser
						 ) <|> (
							(
								\y -> filter $ (== y) . Cartesian.Coordinates.getY
							) <$> ordinateParser
						 ) -- Build a filter from the source-qualifier.

						Parsec.optional captureParser		<?> "Optional capture"

						destination	<- coordinatesParser	<?> "Destination"

						resolveQualifiedMove destination . sourceFilter $ findAttacksBy destination,
					do	-- The most likely scenario, where the intended attacker is unambiguous.
						Parsec.optional captureParser		<?> "Optional capture"

						destination	<- coordinatesParser	<?> "Unqualified destination"

						resolveQualifiedMove destination $ findAttacksBy destination
				],
		Parsec.try $ (
			Parsec.string longCastleToken	<?> "Long castle"
		) >> Data.Maybe.maybe (
			fail "Failed to find any appropriate long castling move."
		) (
			\(moveType, kingsMove, _) -> return {-to ParsecT-monad-} $ Component.QualifiedMove.mkQualifiedMove kingsMove moveType
		) (
			Data.Maybe.listToMaybe longCastlingMoves
		),
		(
			Parsec.string shortCastleToken	<?> "Short castle"
		) >> Data.Maybe.maybe (
			fail "Failed to find any appropriate short castling move."
		) (
			\(moveType, kingsMove, _) -> return {-to ParsecT-monad-} $ Component.QualifiedMove.mkQualifiedMove kingsMove moveType
		) (
			Data.Maybe.listToMaybe shortCastlingMoves
		)
	 ]

	_	<- Parsec.optional (Parsec.oneOf [checkFlag, checkMateFlag] <?> "Check") >> Parsec.optional moveSuffixAnnotationParser

	fromQualifiedMove <$> if validateMoves
		then Data.Maybe.maybe (return {-to ParsecT-monad-} qualifiedMove) (fail . showString "Failed: ") $ Model.Game.validateQualifiedMove qualifiedMove game
		else return {-to ParsecT-monad-} qualifiedMove
#endif

-- | Represent a /rank/ in SAN.
fromRank :: Attribute.Rank.Rank -> Char
fromRank	= Data.Char.toUpper . head . show

-- | Translate from SAN to a /rank/.
toRank :: Char -> Attribute.Rank.Rank
toRank	= read . return {-to List-monad-}

