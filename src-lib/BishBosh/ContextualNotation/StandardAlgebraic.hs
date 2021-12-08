{-# LANGUAGE CPP, LambdaCase #-}
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
-- * Functions
	showsTurn,
	showTurn,
	showsMove,
	showMove,
	movePiece,
--	rankParser,
--	captureParser,
--	moveSuffixAnnotationParser,
	parser,
	fromRank,
	toRank,
-- ** Constructors
	fromQualifiedMove
) where

import			Control.Arrow((&&&))
import qualified	BishBosh.Attribute.MoveType		as Attribute.MoveType
import qualified	BishBosh.Attribute.Rank			as Attribute.Rank
import qualified	BishBosh.Cartesian.Coordinates		as Cartesian.Coordinates
import qualified	BishBosh.Component.CastlingMove		as Component.CastlingMove
import qualified	BishBosh.Component.Move			as Component.Move
import qualified	BishBosh.Component.Piece		as Component.Piece
import qualified	BishBosh.Component.QualifiedMove	as Component.QualifiedMove
import qualified	BishBosh.Component.Turn			as Component.Turn
import qualified	BishBosh.Data.Exception			as Data.Exception
import qualified	BishBosh.Model.Game			as Model.Game
import qualified	BishBosh.Notation.Notation		as Notation.Notation
import qualified	BishBosh.Notation.PureCoordinate	as Notation.PureCoordinate
import qualified	BishBosh.Property.ForsythEdwards	as Property.ForsythEdwards
import qualified	BishBosh.Rule.GameTerminationReason	as Rule.GameTerminationReason
import qualified	BishBosh.State.Board			as State.Board
import qualified	BishBosh.State.MaybePieceByCoordinates	as State.MaybePieceByCoordinates
import qualified	BishBosh.Text.ShowList			as Text.ShowList
import qualified	Control.Applicative
import qualified	Control.Exception
import qualified	Control.Monad
import qualified	Data.Char
import qualified	Data.List
import qualified	Data.Maybe

#ifdef USE_POLYPARSE
import qualified	BishBosh.Text.Poly			as Text.Poly
#	if USE_POLYPARSE == 'L'
import qualified	Text.ParserCombinators.Poly.Lazy	as Poly
#	elif USE_POLYPARSE == 'P'
import qualified	Text.ParserCombinators.Poly.Plain	as Poly
#	else
#		error "USE_POLYPARSE invalid"
#	endif
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

-- | Defines a /move/, to enable i/o in /StandardAlgebraic/-notation.
newtype StandardAlgebraic	= MkStandardAlgebraic {
	getQualifiedMove	:: Component.QualifiedMove.QualifiedMove
} deriving (Eq, Show)

-- | Constructor.
fromQualifiedMove :: Component.QualifiedMove.QualifiedMove -> StandardAlgebraic
fromQualifiedMove	= MkStandardAlgebraic

-- | Whether en-passant moves are tagged, or implicit.
type ExplicitEnPassant	= Bool

-- | Represent the specified /turn/ in SAN.
showsTurn
	:: ExplicitEnPassant
	-> Component.Turn.Turn
	-> Model.Game.Game	-- ^ The /game/ prior to application of the specified /turn/.
	-> ShowS
showsTurn explicitEnPassant turn game
	| Just sourceRank <- Component.Piece.getRank <$> State.MaybePieceByCoordinates.dereference (State.Board.getMaybePieceByCoordinates board) source	= (
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
					case Data.List.delete source {-search for alternatives-} $ State.Board.findAttacksBy board (
						Component.Piece.mkPiece (Model.Game.getNextLogicalColour game) sourceRank
					) destination of
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
			then showChar $ if Data.Maybe.maybe False Rule.GameTerminationReason.isCheckMate $ Model.Game.getMaybeTerminationReason game'
				then checkMateFlag
				else checkFlag
			else id
	)
	| otherwise	= Control.Exception.throw . Data.Exception.mkSearchFailure . showString "BishBosh.ContextualNotation.StandardAlgebraic.showsTurn:\tno piece exists at " . Notation.Notation.showsCoordinates Notation.PureCoordinate.notation source . showString "; " $ Property.ForsythEdwards.showsFEN game "."
	where
		((source, destination), moveType)	= (Component.Move.getSource &&& Component.Move.getDestination) . Component.QualifiedMove.getMove &&& Component.QualifiedMove.getMoveType $ Component.Turn.getQualifiedMove turn
		board					= Model.Game.getBoard game

		isEnPassant, isCapture :: Bool
		isEnPassant	= Attribute.MoveType.isEnPassant moveType
		isCapture	= State.MaybePieceByCoordinates.isOccupied (State.Board.getMaybePieceByCoordinates board) destination || isEnPassant

		showsRank :: Attribute.Rank.Rank -> ShowS
		showsRank rank	= showChar $ fromRank rank

		showsCapture, showsX, showsY, showsDestination :: ShowS
		showsCapture				= showChar captureFlag
		((showsX, showsY), showsDestination)	= (`Notation.Notation.encode` source) &&& (`Notation.Notation.showsCoordinates` destination) $ Notation.PureCoordinate.notation

		game'	= Model.Game.takeTurn turn game

-- | Calls 'showsTurn'.
showTurn
	:: ExplicitEnPassant
	-> Component.Turn.Turn
	-> Model.Game.Game	-- ^ The /game/ prior to application of the specified /turn/.
	-> String
showTurn explicitEnPassant turn game	= showsTurn explicitEnPassant turn game ""

-- | A convenience-function, which generates the /turn/ required to call 'showsTurn'.
showsMove
	:: ExplicitEnPassant
	-> Component.QualifiedMove.QualifiedMove
	-> Model.Game.Game
	-> ShowS
showsMove explicitEnPassant qualifiedMove game	= showsTurn explicitEnPassant (
	Data.Maybe.fromMaybe (
		Control.Exception.throw $ Data.Exception.mkResultUndefined "BishBosh.ContextualNotation.StandardAlgebraic.showsMove:\tModel.Game.maybeLastTurn failed."
	) . Model.Game.maybeLastTurn $ Model.Game.applyQualifiedMove qualifiedMove game
 ) game

-- | Calls 'showsMove'.
showMove
	:: ExplicitEnPassant
	-> Component.QualifiedMove.QualifiedMove
	-> Model.Game.Game
	-> String
showMove explicitEnPassant qualifiedMove game	= showsMove explicitEnPassant qualifiedMove game ""

-- | Applies the specified /move/ to the specified /game/.
movePiece :: StandardAlgebraic -> Model.Game.Transformation
movePiece MkStandardAlgebraic { getQualifiedMove = qualifiedMove }	= Model.Game.applyQualifiedMove qualifiedMove

#ifdef USE_POLYPARSE
-- | Parse the /rank/ of the /piece/ being moved.
rankParser :: Text.Poly.TextParser Attribute.Rank.Rank
rankParser	= toRank `fmap` Poly.satisfyMsg (`elem` map fromRank Attribute.Rank.pieces) Attribute.Rank.tag

-- | Parse the flag which denotes capture.
captureParser :: Text.Poly.TextParser Char
captureParser	= Poly.satisfyMsg (== captureFlag) "Capture"
#else
-- | Parse the /rank/ of the /piece/ being moved.
rankParser :: Parsec.Parser Attribute.Rank.Rank
rankParser	= toRank <$> Parsec.oneOf (map fromRank Attribute.Rank.pieces) <?> Attribute.Rank.tag

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
parser
	:: ExplicitEnPassant
	-> ValidateMoves
	-> Model.Game.Game
#ifdef USE_POLYPARSE
	-> Text.Poly.TextParser StandardAlgebraic
parser explicitEnPassant validateMoves game	= let
	nextLogicalColour			= Model.Game.getNextLogicalColour game
	(longCastlingMove, shortCastlingMove)	= Component.CastlingMove.getLongAndShortMoves nextLogicalColour
	board					= Model.Game.getBoard game
	getMaybePiece				= State.MaybePieceByCoordinates.dereference $ State.Board.getMaybePieceByCoordinates board
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

					findAttacksBy destination	= State.Board.findAttacksBy board piece destination

				if rank == Attribute.Rank.Pawn
					then let
						promotionParser :: Text.Poly.TextParser Attribute.Rank.Rank
						promotionParser	= Text.Poly.char promotionFlag >> rankParser
					in Poly.oneOf' [
						(
							"Pawn-advance",
							do
								destination	<- Notation.PureCoordinate.coordinatesParser

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
								x		<- Notation.PureCoordinate.abscissaParser
								_		<- captureParser
								destination	<- Poly.commit Notation.PureCoordinate.coordinatesParser

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

						resolveQualifiedMove destination	= \case
							[]			-> do
								context	<- Poly.manyFinally' Poly.next $ Text.Poly.char '\n'

								Poly.failBad . showString "failed to locate any " . shows piece . showString " able to move to " . shows destination . showString ". Before " $ shows context "."
							[source]		-> return {-to Parser-monad-} . Component.QualifiedMove.mkQualifiedMove (Component.Move.mkMove source destination) $ mkNormalMoveType destination
							sourceCandidates	-> Poly.oneOf' [
								show &&& return {-to Parser-monad-} $ qualifiedMove |
									source	<- sourceCandidates,-- Attempt to resolve the ambiguity by playing subsequent moves.
									let qualifiedMove	= Component.QualifiedMove.mkQualifiedMove (Component.Move.mkMove source destination) $ mkNormalMoveType destination,
									Model.Game.isValidQualifiedMove game qualifiedMove
							 ] -- List-comprehension.
					in Poly.oneOf' [
						(
							"Fully qualified move",	-- N.B. this scenario occurs when there are identical pieces on both the same row & the same column, as the intended attacker; i.e. after a promotion.
							do
								source		<- Notation.PureCoordinate.coordinatesParser
								destination	<- Control.Applicative.optional captureParser >> Notation.PureCoordinate.coordinatesParser

								return {-to Parser-monad-} . Component.QualifiedMove.mkQualifiedMove (Component.Move.mkMove source destination) $ mkNormalMoveType destination
						), (
							"Partially qualified move",	-- This scenario occurs if there's an identical piece on either the same row or the same column, as the intended attacker.
							do
								sourceFilter	<- Poly.oneOf' [
									(
										"Abscissa qualification",
										(
											\x -> filter $ (== x) . Cartesian.Coordinates.getX
										) `fmap` Notation.PureCoordinate.abscissaParser
									), (
										"Ordinate qualification",
										(
											\y -> filter $ (== y) . Cartesian.Coordinates.getY
										) `fmap` Notation.PureCoordinate.ordinateParser
									)
								 ] -- Build a filter from the source-qualifier.

								destination	<- Control.Applicative.optional captureParser >> Notation.PureCoordinate.coordinatesParser

								resolveQualifiedMove destination . sourceFilter $ findAttacksBy destination
						), (
							"Unqualified move",	-- The most likely scenario, where the intended attacker is unambiguous.
							Poly.commit $ do
								destination	<- Control.Applicative.optional captureParser >> Notation.PureCoordinate.coordinatesParser

								resolveQualifiedMove destination $ findAttacksBy destination
						)
					]
		), (
			"Long castle",
			Text.Poly.string longCastleToken >> return {-to Parser-monad-} (
				uncurry Component.QualifiedMove.mkQualifiedMove $ (Component.CastlingMove.getKingsMove &&& Component.CastlingMove.getMoveType) longCastlingMove
			)
		), (
			"Short castle",
			Text.Poly.string shortCastleToken >> return {-to Parser-monad-} (
				uncurry Component.QualifiedMove.mkQualifiedMove $ (Component.CastlingMove.getKingsMove &&& Component.CastlingMove.getMoveType) shortCastlingMove
			)
		) -- TODO: for some reason, lazy-parsing with ghc-8.0.1 & polyparse-1.12 conflates "O-O-O" with "O-O"; confirm.
	 ]

	_	<- Control.Applicative.optional (Poly.satisfyMsg (`elem` [checkFlag, checkMateFlag]) "Check") >> Control.Applicative.optional moveSuffixAnnotationParser

	fromQualifiedMove `fmap` (
		if validateMoves
			then Data.Maybe.maybe (return {-to Parser-monad-} qualifiedMove) (Poly.failBad . showString "failed: ") . Model.Game.validateQualifiedMove game
			else return {-to Parser-monad-}
	 ) qualifiedMove
#else /* Parsec */
	-> Parsec.Parser StandardAlgebraic
parser explicitEnPassant validateMoves game	= let
	nextLogicalColour			= Model.Game.getNextLogicalColour game
	(longCastlingMove, shortCastlingMove)	= Component.CastlingMove.getLongAndShortMoves nextLogicalColour
	board					= Model.Game.getBoard game
	getMaybePiece				= State.MaybePieceByCoordinates.dereference $ State.Board.getMaybePieceByCoordinates board
	getMaybeRank				= fmap Component.Piece.getRank . getMaybePiece
 in do
	qualifiedMove	<- Parsec.spaces >> Parsec.choice [
		do
			rank	<- Parsec.option Attribute.Rank.Pawn rankParser

			let
				piece :: Component.Piece.Piece
				piece	= Component.Piece.mkPiece nextLogicalColour rank

				findAttacksBy destination	= State.Board.findAttacksBy board piece destination

			if rank == Attribute.Rank.Pawn
				then let
					promotionParser :: Parsec.Parser Attribute.Rank.Rank
					promotionParser	= (Parsec.char promotionFlag <?> "Promotion") >> rankParser
				in Parsec.try (
					do
						destination	<- Notation.PureCoordinate.coordinatesParser	<?> "Destination"

						Data.Maybe.maybe (
							fail . showString "Failed to locate any " . shows piece . showString " which can advance to " $ shows destination "."
						 ) (
							\source -> Component.QualifiedMove.mkQualifiedMove (Component.Move.mkMove source destination) . Attribute.MoveType.mkNormalMoveType Nothing {-capture-} <$> Control.Applicative.optional promotionParser
						 ) . Data.List.find (
							(== Just piece) . getMaybePiece
						 ) . Data.Maybe.catMaybes . take 2 {-maximum Pawn-advance-} . tail {-drop the original-} $ iterate (
							>>= Cartesian.Coordinates.maybeRetreat nextLogicalColour
						 ) $ Just destination
				) <|> do
					x		<- Notation.PureCoordinate.abscissaParser <* captureParser
					destination	<- Notation.PureCoordinate.coordinatesParser	<?> "Destination"

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

					resolveQualifiedMove destination	= \case
						[]			-> fail . showString "Failed to locate any " . shows piece . showString " able to move to " $ shows destination "."
						[source]		-> return {-to ParsecT-monad-} . Component.QualifiedMove.mkQualifiedMove (Component.Move.mkMove source destination) $ mkNormalMoveType destination
						sourceCandidates	-> Parsec.choice [
							Parsec.try $ return {-to ParsecT-monad-} qualifiedMove |
								source	<- sourceCandidates,-- Attempt to resolve the ambiguity by playing subsequent moves.
								let qualifiedMove	= Component.QualifiedMove.mkQualifiedMove (Component.Move.mkMove source destination) $ mkNormalMoveType destination,
								Model.Game.isValidQualifiedMove game qualifiedMove
						 ] -- List-comprehension.
				in Parsec.choice [
					Parsec.try $ do -- N.B. this scenario occurs when there are identical pieces on both the same row & the same column, as the intended attacker; i.e. after a promotion.
						source		<- Notation.PureCoordinate.coordinatesParser	<?> "Source"

						Parsec.optional captureParser		<?> "Optional capture"

						destination	<- Notation.PureCoordinate.coordinatesParser	<?> "Destination"

						return {-to ParsecT-monad-} . Component.QualifiedMove.mkQualifiedMove (Component.Move.mkMove source destination) $ mkNormalMoveType destination,
					Parsec.try $ do	-- This scenario occurs if there's an identical piece on either the same row or the same column, as the intended attacker.
						sourceFilter	<- (
							(
								\x -> filter $ (== x) . Cartesian.Coordinates.getX
							) <$> Notation.PureCoordinate.abscissaParser
						 ) <|> (
							(
								\y -> filter $ (== y) . Cartesian.Coordinates.getY
							) <$> Notation.PureCoordinate.ordinateParser
						 ) -- Build a filter from the source-qualifier.

						Parsec.optional captureParser		<?> "Optional capture"

						destination	<- Notation.PureCoordinate.coordinatesParser	<?> "Destination"

						resolveQualifiedMove destination . sourceFilter $ findAttacksBy destination,
					do	-- The most likely scenario, where the intended attacker is unambiguous.
						Parsec.optional captureParser		<?> "Optional capture"

						destination	<- Notation.PureCoordinate.coordinatesParser	<?> "Unqualified destination"

						resolveQualifiedMove destination $ findAttacksBy destination
				],
		Parsec.try $ (
			Parsec.string longCastleToken	<?> "Long castle"
		) >> return {-to ParsecT-monad-} (
			uncurry Component.QualifiedMove.mkQualifiedMove $ (Component.CastlingMove.getKingsMove &&& Component.CastlingMove.getMoveType) longCastlingMove
		), (
			Parsec.string shortCastleToken	<?> "Short castle"
		) >> return {-to ParsecT-monad-} (
			uncurry Component.QualifiedMove.mkQualifiedMove $ (Component.CastlingMove.getKingsMove &&& Component.CastlingMove.getMoveType) shortCastlingMove
		)
	 ]

	_	<- Parsec.optional (Parsec.oneOf [checkFlag, checkMateFlag] <?> "Check") >> Parsec.optional moveSuffixAnnotationParser

	fromQualifiedMove <$> (
		if validateMoves
			then Data.Maybe.maybe (return {-to ParsecT-monad-} qualifiedMove) (fail . showString "Failed: ") . Model.Game.validateQualifiedMove game
			else return {-to ParsecT-monad-}
	 ) qualifiedMove
#endif

-- | Represent a /rank/ in SAN.
fromRank :: Attribute.Rank.Rank -> Char
fromRank	= Data.Char.toUpper . head . show

-- | Translate from SAN to a /rank/.
toRank :: Char -> Attribute.Rank.Rank
toRank	= read . return {-to List-monad-}

