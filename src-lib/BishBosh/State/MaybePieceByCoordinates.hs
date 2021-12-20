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

	* Models the /board/ as a sparse array, each element of which might contain a /piece/.

	* N.B.: while this could be represented as @Data.Map.Map Coordinates Piece@, replacing 'Data.Array.IArray.!' with 'Data.Map.lookup',
	it actually required more space (despite having at most half the elements) & runs slower (because of 'compare').

	* cf. the piece-centric model of the board defined in "BishBosh.State.CoordinatesByRankByLogicalColour".
-}

module BishBosh.State.MaybePieceByCoordinates(
-- * Types
-- ** Data-types
	MaybePieceByCoordinates(),
-- * Functions
	inferMoveType,
	findBlockingPiece,
	findBlockingPieces,
	findAttackerInDirection,
	findAttackerInDirections,
	listDestinationsFor,
--	listToRaster,
--	shows2D,
	show2D,
-- ** Accessors
	dereference,
--	getPieces,
-- ** Predicates
	isVacant,
	isOccupied,
	isClear,
	isObstructed,
	isEnPassantMove
) where

import			Control.Applicative((<|>))
import			Control.Arrow((&&&), (***))
import			Control.Category((>>>))
import			Data.Array.IArray((!), (//))
import qualified	BishBosh.Attribute.MoveType				as Attribute.MoveType
import qualified	BishBosh.Attribute.Rank					as Attribute.Rank
import qualified	BishBosh.Cartesian.Abscissa				as Cartesian.Abscissa
import qualified	BishBosh.Cartesian.Coordinates				as Cartesian.Coordinates
import qualified	BishBosh.Cartesian.Ordinate				as Cartesian.Ordinate
import qualified	BishBosh.Colour.ANSIColourCode				as Colour.ANSIColourCode
import qualified	BishBosh.Colour.ColourScheme				as Colour.ColourScheme
import qualified	BishBosh.Colour.LogicalColour				as Colour.LogicalColour
import qualified	BishBosh.Colour.LogicalColourOfSquare			as Colour.LogicalColourOfSquare
import qualified	BishBosh.Colour.PhysicalColour				as Colour.PhysicalColour
import qualified	BishBosh.Component.Accountant				as Component.Accountant
import qualified	BishBosh.Component.CastlingMove				as Component.CastlingMove
import qualified	BishBosh.Component.Move					as Component.Move
import qualified	BishBosh.Component.Piece				as Component.Piece
import qualified	BishBosh.Component.PieceSquareValueByCoordinates	as Component.PieceSquareValueByCoordinates
import qualified	BishBosh.Component.PieceSquareValueByCoordinatesByRank	as Component.PieceSquareValueByCoordinatesByRank
import qualified	BishBosh.Component.Zobrist				as Component.Zobrist
import qualified	BishBosh.Data.Exception					as Data.Exception
import qualified	BishBosh.Direction.Direction				as Direction.Direction
import qualified	BishBosh.Notation.Figurine				as Notation.Figurine
import qualified	BishBosh.Property.Empty					as Property.Empty
import qualified	BishBosh.Property.ExtendedPositionDescription		as Property.ExtendedPositionDescription
import qualified	BishBosh.Property.FixedMembership			as Property.FixedMembership
import qualified	BishBosh.Property.ForsythEdwards			as Property.ForsythEdwards
import qualified	BishBosh.Property.Opposable				as Property.Opposable
import qualified	BishBosh.Property.Orientated				as Property.Orientated
import qualified	BishBosh.Property.Reflectable				as Property.Reflectable
import qualified	BishBosh.Property.SelfValidating			as Property.SelfValidating
import qualified	BishBosh.StateProperty.Censor				as StateProperty.Censor
import qualified	BishBosh.StateProperty.Hashable				as StateProperty.Hashable
import qualified	BishBosh.StateProperty.Mutator				as StateProperty.Mutator
import qualified	BishBosh.StateProperty.Seeker				as StateProperty.Seeker
import qualified	BishBosh.StateProperty.View				as StateProperty.View
import qualified	BishBosh.Text.ShowList					as Text.ShowList
import qualified	BishBosh.Type.Length					as Type.Length
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Exception
import qualified	Data.Array.IArray
import qualified	Data.Char
import qualified	Data.Default
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.List.Extra
import qualified	Data.Maybe
import qualified	ToolShed.Data.List.Runlength

{- |
	* This structure allows one to determine what /piece/ (if any) is located at specific /coordinates/.

	* N.B.: this could be implemented using 'Data.Vector.Vector', which being indexed by 'Int' is no longer polymorphic & permits many unsafe operations; but the result is no faster.
-}
newtype MaybePieceByCoordinates	= MkMaybePieceByCoordinates {
	deconstruct	:: Cartesian.Coordinates.ArrayByCoordinates (Maybe Component.Piece.Piece)	-- ^ Each square optionally contains a piece.
} deriving (Eq, Ord)

{- |
	* Used to separate the /ranks/ of the /board/ as represented by the IO-format <https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation>.

	* Chops a list into a 2-D list.
-}
listToRaster :: [a] -> [[a]]
listToRaster	= Data.List.Extra.chunksOf $ fromIntegral Cartesian.Abscissa.xLength {-CAVEAT: this also depends on the raster-order-}

instance Read MaybePieceByCoordinates where
	readsPrec _	= Property.ForsythEdwards.readsFEN

instance Show MaybePieceByCoordinates where
	showsPrec _	= Property.ForsythEdwards.showsFEN

instance Property.ExtendedPositionDescription.ReadsEPD MaybePieceByCoordinates where
	readsEPD s
		| length rows /= fromIntegral Cartesian.Ordinate.yLength || any (
			(/= fromIntegral Cartesian.Abscissa.xLength) . length
		) rows		= []	-- No parse.
		| otherwise	= [(MkMaybePieceByCoordinates . Cartesian.Coordinates.listArrayByCoordinates . concat $ reverse rows, remainder)]
		where
			(rows, remainder)	= Control.Arrow.first (
				map (
					concatMap (
						\c -> case reads [c] of
							[(i, "")]	-> replicate i Nothing	-- Expand the runlength-code so that each row has the same length.
							_		-> [Just piece | (piece, []) <- Property.ExtendedPositionDescription.readsEPD [c]] -- List-comprehension.
					)
				) . Text.ShowList.splitOn (== Property.ExtendedPositionDescription.rankSeparator)
			 ) . span (
				`elem` (
					Property.ExtendedPositionDescription.rankSeparator : Component.Piece.epdCharacterSet ++ concatMap show [1 .. Cartesian.Abscissa.xLength]
				)
			 ) $ Data.List.Extra.trimStart s

instance Property.ExtendedPositionDescription.ShowsEPD MaybePieceByCoordinates where
	showsEPD MkMaybePieceByCoordinates { deconstruct = byCoordinates }	= foldr1 (
		>>>	-- Render the line with the highest y-coordinate first.
	 ) . Data.List.intersperse (
		showChar Property.ExtendedPositionDescription.rankSeparator	-- Separate the lines.
	 ) . map (
		foldr1 (.) . concatMap (
			\(runLength, maybePiece) -> Data.Maybe.maybe [
				shows runLength	-- Represent empty squares.
			] (
				replicate runLength . Property.ExtendedPositionDescription.showsEPD	-- Render each piece.
			) maybePiece
		) . ToolShed.Data.List.Runlength.encode
	 ) . listToRaster $ Data.Foldable.toList byCoordinates

instance Property.ForsythEdwards.ReadsFEN MaybePieceByCoordinates

instance Property.ForsythEdwards.ShowsFEN MaybePieceByCoordinates

instance Data.Default.Default MaybePieceByCoordinates where
	def = Property.ForsythEdwards.readFEN . Data.List.intercalate [Property.ExtendedPositionDescription.rankSeparator] $ map ($ Colour.LogicalColour.Black) [
		showNobility,
		showPawnRow
	 ] ++ replicate 4 "8" ++ map ($ Colour.LogicalColour.White) [
		showPawnRow,
		showNobility
	 ] where
		showPieces :: [Component.Piece.Piece] -> String
		showPieces	= concatMap Property.ForsythEdwards.showFEN

		showPawnRow, showNobility :: Colour.LogicalColour.LogicalColour -> String
		showPawnRow logicalColour	= showPieces . replicate (fromIntegral Cartesian.Abscissa.xLength) $ Component.Piece.mkPawn logicalColour
		showNobility logicalColour	= showPieces $ map (Component.Piece.mkPiece logicalColour) Attribute.Rank.nobility

instance Property.Reflectable.ReflectableOnX MaybePieceByCoordinates where
	reflectOnX MkMaybePieceByCoordinates { deconstruct = byCoordinates }	= MkMaybePieceByCoordinates . Cartesian.Coordinates.arrayByCoordinates . map (
		Property.Reflectable.reflectOnX *** fmap Property.Opposable.getOpposite
	 ) $ Data.Array.IArray.assocs byCoordinates

instance Property.Reflectable.ReflectableOnY MaybePieceByCoordinates where
	reflectOnY MkMaybePieceByCoordinates { deconstruct = byCoordinates }	= MkMaybePieceByCoordinates $ Data.Array.IArray.ixmap (minBound, maxBound) Property.Reflectable.reflectOnY byCoordinates

instance Property.Empty.Empty MaybePieceByCoordinates where
	empty	= MkMaybePieceByCoordinates . Cartesian.Coordinates.listArrayByCoordinates $ repeat Property.Empty.empty

instance Control.DeepSeq.NFData MaybePieceByCoordinates where
	rnf MkMaybePieceByCoordinates { deconstruct = byCoordinates }	= Control.DeepSeq.rnf byCoordinates

instance StateProperty.Censor.Censor MaybePieceByCoordinates where
	countPiecesByLogicalColour	= Data.List.foldl' (
		\(nBlack, nWhite) piece -> if Component.Piece.isBlack piece
			then let nBlack' = succ nBlack in nBlack' `seq` (nBlack', nWhite)
			else let nWhite' = succ nWhite in nWhite' `seq` (nBlack, nWhite')
	 ) (0, 0) . getPieces

	countPieces	= fromIntegral . length . getPieces

	countPieceDifferenceByRank	= Data.Array.IArray.accumArray (+) 0 (minBound, maxBound) . map (
		Component.Piece.getRank &&& (
			\piece -> (
				if Component.Piece.isBlack piece
					then negate
					else id
			) 1
		)
	 ) . getPieces

	hasInsufficientMaterial maybePieceByCoordinates	= all (
		(`notElem` Attribute.Rank.individuallySufficientMaterial) . Component.Piece.getRank . snd {-piece-}
	 ) locatedPieces && case blackKnights ++ whiteKnights of
		[]	-> Cartesian.Coordinates.areSquaresIsochromatic bishops
		[_]	-> null bishops
		_	-> False
		where
			locatedPieces	= StateProperty.Seeker.findAllPieces maybePieceByCoordinates

			[blackKnights, blackBishops, whiteKnights, whiteBishops]	= [
				[
					coordinates |
						(coordinates, piece)	<- locatedPieces,
						piece == Component.Piece.mkPiece logicalColour rank
				] |
					logicalColour	<- Property.FixedMembership.members,
					rank		<- [Attribute.Rank.Knight, Attribute.Rank.Bishop]
			 ] -- List-comprehension.

			bishops	= blackBishops ++ whiteBishops

	hasBothKings maybePieceByCoordinates	= case Data.List.partition Component.Piece.isBlack . filter Component.Piece.isKing $ getPieces maybePieceByCoordinates of
		([_], [_])	-> True
		_		-> False

instance StateProperty.Hashable.Hashable MaybePieceByCoordinates where
	listRandoms zobrist MkMaybePieceByCoordinates { deconstruct = byCoordinates }	= [
		Component.Zobrist.dereferenceRandomByCoordinatesByRankByLogicalColour zobrist $ uncurry (,,) (Component.Piece.getLogicalColour &&& Component.Piece.getRank $ piece) coordinates |
			(coordinates, Just piece)	<- Data.Array.IArray.assocs byCoordinates
	 ] -- List-comprehension.

instance StateProperty.Mutator.Mutator MaybePieceByCoordinates where
	defineCoordinates maybePiece coordinates MkMaybePieceByCoordinates { deconstruct = byCoordinates }	= Control.Exception.assert (
		Data.Maybe.isJust maybePiece || Data.Maybe.isJust (byCoordinates ! coordinates)
	 ) . MkMaybePieceByCoordinates $ byCoordinates // [(coordinates, maybePiece)]

	movePiece move moveType sourcePiece MkMaybePieceByCoordinates { deconstruct = byCoordinates }	= MkMaybePieceByCoordinates $ byCoordinates // (
		if Attribute.MoveType.isEnPassant moveType
			then (:) (
				Cartesian.Coordinates.retreat (Component.Piece.getLogicalColour sourcePiece) $ Component.Move.getDestination move,
				Nothing
			)
			else id
	 ) [
		(
			Component.Move.getSource move,
			Nothing	-- Remove the piece from the source.
		), (
			Component.Move.getDestination move,
			Just $ Data.Maybe.maybe id Component.Piece.promote (Attribute.MoveType.getMaybePromotedRank moveType) sourcePiece       -- Place the piece at the destination, removing any opposing incumbent as a side-effect.
		)
	 ]

{- |
	* Find any @Knight@s of the specified /logical colour/, in attack-range around the specified /coordinates/.

	* CAVEAT: nothing is said about whether any /piece/ at the specified /coordinates/ belongs to the opponent, as one might expect.

	* CAVEAT: less efficient than 'State.CoordinatesByRankByLogicalColour.findProximateKnights'.
-}
instance StateProperty.Seeker.Seeker MaybePieceByCoordinates where
	findProximateKnights MkMaybePieceByCoordinates { deconstruct = byCoordinates } logicalColour destination	= filter (
		(== Just knight) . (byCoordinates !)
	 ) $ Component.Piece.findAttackDestinations knight destination where
		knight	= Component.Piece.mkKnight logicalColour

	findPieces predicate MkMaybePieceByCoordinates { deconstruct = byCoordinates }	= [
		(coordinates, piece) |
			(coordinates, Just piece)	<- Data.Array.IArray.assocs byCoordinates,
			predicate piece
	 ] -- List-comprehension.

instance StateProperty.View.View MaybePieceByCoordinates where
	fromAssocs	= MkMaybePieceByCoordinates . Data.Array.IArray.accumArray (flip const) Nothing {-default-} (minBound, maxBound) . map (Control.Arrow.second Just)

instance Component.Accountant.Accountant MaybePieceByCoordinates where
	sumPieceSquareValueByLogicalColour pieceSquareValueByCoordinatesByRank maybePieceByCoordinates nPieces = (
		\(b, w) -> [b, w]
	 ) . Data.List.foldl' (
		\(b, w) (coordinates, piece) -> let
			logicalColour		= Component.Piece.getLogicalColour piece
			pieceSquareValue	= realToFrac $! Component.PieceSquareValueByCoordinates.getPieceSquareValue (getPieceSquareValueByCoordinates $ Component.Piece.getRank piece) logicalColour coordinates
		in if Colour.LogicalColour.isBlack logicalColour
			then let b' = b + pieceSquareValue in b' `seq` (b', w)
			else let w' = w + pieceSquareValue in w' `seq` (b, w')
	 ) (0, 0) $ StateProperty.Seeker.findAllPieces maybePieceByCoordinates where
		getPieceSquareValueByCoordinates	= Component.PieceSquareValueByCoordinatesByRank.getPieceSquareValueByCoordinates pieceSquareValueByCoordinatesByRank nPieces

instance Property.SelfValidating.SelfValidating MaybePieceByCoordinates where
	findInvalidity	= uncurry (++) . (StateProperty.Censor.findInvalidity &&& StateProperty.Seeker.findInvalidity)

-- | Dereference the array.
dereference :: MaybePieceByCoordinates -> Cartesian.Coordinates.Coordinates -> Maybe Component.Piece.Piece
{-# INLINE dereference #-}
dereference MkMaybePieceByCoordinates { deconstruct = byCoordinates }	= (byCoordinates !)

-- | Infer the type of the specified /move/.
inferMoveType
	:: MaybePieceByCoordinates
	-> Component.Move.Move
	-> Maybe Attribute.Rank.Rank	-- ^ The /rank/ to which a @Pawn@ should be promoted; defaulting to @Queen@.
	-> Attribute.MoveType.MoveType
inferMoveType maybePieceByCoordinates@MkMaybePieceByCoordinates { deconstruct = byCoordinates } move maybePromotionRank
	| Just sourcePiece <- byCoordinates ! Component.Move.getSource move	= Data.Maybe.maybe (
		if isEnPassantMove maybePieceByCoordinates move
			then Attribute.MoveType.enPassant	-- N.B.: if this move is valid, then one's opponent must have just double advanced an adjacent Pawn.
			else let
				destination	= Component.Move.getDestination move
			in Attribute.MoveType.mkNormalMoveType (
				Component.Piece.getRank <$> byCoordinates ! destination	-- Record the rank of any piece which was taken; the logical colour is inferred to be the opposite of 'sourcePiece'.
			) $ if Component.Piece.isPawnPromotion sourcePiece destination
				then maybePromotionRank <|> Just Attribute.Rank.defaultPromotionRank
				else Nothing
	) Component.CastlingMove.getMoveType $ if Component.Piece.isKing sourcePiece
		then Data.List.find (
			(== move) . Component.CastlingMove.getKingsMove
		) . Component.CastlingMove.getCastlingMoves $ Component.Piece.getLogicalColour sourcePiece
		else Nothing
	| otherwise	= Control.Exception.throw . Data.Exception.mkSearchFailure . showString "BishBosh.State.MaybePieceByCoordinates.inferMoveType:\tno piece exists at " . shows (Component.Move.getSource move) . showString "; " $ shows maybePieceByCoordinates "."

{- |
	* Lists the destination-/coordinates/ to which the referenced /piece/ can move, & the /rank/ of any /piece/ taken.

	* N.B.: one can reference either player's /piece/, regardless of whose turn it is to move.

	* CAVEAT: doesn't include either /Castling/ or /En-passant/, because this function doesn't know the history of the game.

	* CAVEAT: doesn't check whether any proposed /move/ exposes one's @King@, because this function doesn't assume the existence of a @King@.

	* CAVEAT: the opponent's @King@ may be one of the destinations returned, but only if it was actually their move next.

	* CAVEAT: doesn't typically check whether anything (let alone the specified /piece/) exists at the specified source-/coordinates/.
-}
listDestinationsFor
	:: MaybePieceByCoordinates
	-> Cartesian.Coordinates.Coordinates					-- ^ The source for which destinations are required.
	-> Component.Piece.Piece						-- ^ The /piece/ at the specified source.
	-> [(Cartesian.Coordinates.Coordinates, Maybe Attribute.Rank.Rank)]	-- ^ The destination & the rank of any piece taken.
listDestinationsFor maybePieceByCoordinates@MkMaybePieceByCoordinates { deconstruct = byCoordinates } source piece	= Control.Exception.assert (
	byCoordinates ! source == Just piece
 ) $ if Component.Piece.getRank piece `elem` Attribute.Rank.fixedAttackRange
	then {-P,N,K-} let
		findAttackDestinations :: (Maybe Component.Piece.Piece -> Bool) -> [(Cartesian.Coordinates.Coordinates, Maybe Attribute.Rank.Rank)]
		findAttackDestinations predicate	= [
			(destination, Component.Piece.getRank <$> maybeDestinationPiece) |
				destination	<- Component.Piece.findAttackDestinations piece source,
				let maybeDestinationPiece	= byCoordinates ! destination,
				predicate maybeDestinationPiece
		 ] -- List-comprehension.
	in if Component.Piece.isPawn piece
		then findAttackDestinations (
			Data.Maybe.maybe False {-unoccupied-} $ (/= logicalColour) . Component.Piece.getLogicalColour
		) ++ let
			advance	:: Cartesian.Coordinates.Coordinates -> Cartesian.Coordinates.Coordinates
			advance	= Cartesian.Coordinates.advance logicalColour

			advancedLocation	= advance source
		in if isVacant maybePieceByCoordinates advancedLocation
			then map (
				flip (,) Nothing	-- N.B.: a Pawn can only take diagonally.
			) $ advancedLocation : [
				doubleAdvancedLocation |
					Cartesian.Coordinates.isPawnsFirstRank source logicalColour,
					let doubleAdvancedLocation	= advance advancedLocation,
					isVacant maybePieceByCoordinates doubleAdvancedLocation
			] -- List-comprehension.
			else []	-- The path immediately ahead is blocked.
		else {-N,K-} findAttackDestinations . Data.Maybe.maybe True {-unoccupied-} $ (/= logicalColour) . Component.Piece.getLogicalColour
	else {-R,B,Q-} let
		takeUntil :: [Cartesian.Coordinates.Coordinates] -> [(Cartesian.Coordinates.Coordinates, Maybe Attribute.Rank.Rank)]
		takeUntil (destination : remainder)
			| Just blockingPiece	<- byCoordinates ! destination	= [
				(
					destination,
					Just $ Component.Piece.getRank blockingPiece
				) | Component.Piece.getLogicalColour blockingPiece /= logicalColour
			] -- List-comprehension.
			| otherwise						= (destination, Nothing) : takeUntil remainder	-- Recurse.
		takeUntil _							= []
	in Cartesian.Coordinates.applyAlongDirectionsFrom takeUntil source $ if Component.Piece.isQueen piece
		then Nothing	-- i.e. all directions.
		else Just $ Component.Piece.getAttackDirections piece
	where
		logicalColour	= Component.Piece.getLogicalColour piece

-- | Show the /board/ in two dimensions, with /x/ & /y/ indexes.
shows2D
	:: MaybePieceByCoordinates
	-> Type.Length.Column			-- ^ The column-magnification.
	-> Colour.ColourScheme.ColourScheme
	-> Bool					-- ^ Whether to depict pieces as Unicode figurines.
	-> (Type.Length.X, Type.Length.Y)	-- ^ The origin from which axes are labelled.
	-> ShowS				-- ^ Output suitable for display on a terminal.
shows2D MkMaybePieceByCoordinates { deconstruct = byCoordinates } boardColumnMagnification colourScheme depictFigurine (xOrigin, yOrigin)	= (
	foldr (
		\(y, pairs) showsRow -> showsRow . showString axisGraphicsRendition . showChar y . foldr (
			\(coordinates, c) acc' -> showString (
				Colour.ANSIColourCode.selectGraphicsRendition False {-isBold-} . Colour.ANSIColourCode.mkBgColourCode $ (
					if Colour.LogicalColourOfSquare.isBlack $ Cartesian.Coordinates.getLogicalColourOfSquare coordinates
						then Colour.ColourScheme.getDarkSquareColour
						else Colour.ColourScheme.getLightSquareColour
				) colourScheme
			) . showString (
				Colour.ANSIColourCode.selectGraphicsRendition True {-isBold-} . Colour.ANSIColourCode.mkFgColourCode $ (
					if Data.Char.isLower c {-Black-}
						then Colour.ColourScheme.getDarkPieceColour
						else Colour.ColourScheme.getLightPieceColour
				) colourScheme
			) . let
				showPadding	= showString (fromIntegral (pred boardColumnMagnification) `replicate` ' ')
			in showPadding . showChar c . showPadding . acc'
		) showsReset pairs . showChar '\n'
	) id . zip (
		take (fromIntegral Cartesian.Ordinate.yLength) . enumFrom . Data.Char.chr $ fromIntegral yOrigin
	) . listToRaster . map (
		Control.Arrow.second . Data.Maybe.maybe ' ' $ if depictFigurine
			then Notation.Figurine.toFigurine	-- Represent each piece as a Unicode figurine.
			else head . show			-- Represent each piece as an ASCII character.
	) $ Data.Array.IArray.assocs byCoordinates
 ) . showString (
	replicate (fromIntegral boardColumnMagnification) ' '	-- Shift the line of x-axis labels right.
 ) . showString axisGraphicsRendition . foldr (.) showsReset (
	Data.List.intersperse (
		showString $ replicate (2 * fromIntegral (pred boardColumnMagnification)) ' '	-- Separate each of the x-axis labels.
	) . map showChar . take (
		fromIntegral Cartesian.Abscissa.xLength
	) . enumFrom . Data.Char.chr $ fromIntegral xOrigin
 ) where
	axisGraphicsRendition :: Colour.ANSIColourCode.GraphicsRendition
	axisGraphicsRendition	= Colour.ANSIColourCode.selectGraphicsRendition True {-isBold-} $ Colour.ANSIColourCode.mkFgColourCode Colour.PhysicalColour.green

	showsReset :: ShowS
	showsReset	= showString $ Colour.ANSIColourCode.selectGraphicsRendition False Data.Default.def

-- | Show the board using a two-dimensional representation.
show2D
	:: MaybePieceByCoordinates
	-> Type.Length.Column			-- ^ The column-magnification.
	-> Colour.ColourScheme.ColourScheme
	-> Bool					-- ^ Whether to depict figurines.
	-> (Type.Length.X, Type.Length.Y)	-- ^ The origin from which axes are labelled.
	-> String				-- ^ The output suitable for display on a terminal.
show2D maybePieceByCoordinates boardColumnMagnification colourScheme depictFigurine (xOrigin, yOrigin)	= shows2D maybePieceByCoordinates boardColumnMagnification colourScheme depictFigurine (xOrigin, yOrigin) ""

-- | Extract the pieces from the board, discarding their coordinates.
getPieces :: MaybePieceByCoordinates -> [Component.Piece.Piece]
getPieces MkMaybePieceByCoordinates { deconstruct = byCoordinates }	= Data.Maybe.catMaybes $ Data.Foldable.toList byCoordinates

{- |
	* Find the first /piece/ of either /logical colour/, encountered in the specified /direction/, from just after the specified /coordinates/.

	* CAVEAT: this is a performance-hotspot.
-}
findBlockingPiece
	:: MaybePieceByCoordinates
	-> Cartesian.Coordinates.Coordinates	-- ^ The starting point.
	-> Direction.Direction.Direction	-- ^ The direction in which to search.
	-> Maybe Component.Piece.LocatedPiece	-- ^ Any blocking piece.
findBlockingPiece MkMaybePieceByCoordinates { deconstruct = byCoordinates } source	= slave . Cartesian.Coordinates.extrapolate source where
	slave :: [Cartesian.Coordinates.Coordinates] -> Maybe Component.Piece.LocatedPiece
	slave (coordinates : remainder)
		| Just blockingPiece	<- byCoordinates ! coordinates	= Just (coordinates, blockingPiece)	-- Terminate with success.
		| otherwise						= slave remainder			-- Recurse.
	slave _								= Nothing				-- Terminate with failure.

{- |
	* Find the first /piece/ of either /logical colour/, encountered in each of the specified /direction/s, from just after the specified /coordinates/.

	* N.B.: one could call 'findBlockingPiece' for each /direction/, but this function exploits optimisations available when all /direction/s are required.
-}
findBlockingPieces
	:: MaybePieceByCoordinates
	-> Cartesian.Coordinates.Coordinates		-- ^ The starting point.
	-> Maybe [Direction.Direction.Direction]	-- ^ The directions in which to search; 'Nothing' implies omni-directional.
	-> [Component.Piece.LocatedPiece]		-- ^ Blocking pieces in non-specific directions.
findBlockingPieces MkMaybePieceByCoordinates { deconstruct = byCoordinates }	= Cartesian.Coordinates.applyAlongDirectionsFrom slave where
	slave :: [Cartesian.Coordinates.Coordinates] -> [Component.Piece.LocatedPiece]
	slave (coordinates : remainder)
		| Just blockingPiece	<- byCoordinates ! coordinates	= [(coordinates, blockingPiece)]	-- Terminate with success.
		| otherwise						= slave remainder			-- Recurse.
	slave _								= []					-- Terminate with failure.

{- |
	* Find the /coordinates/ of any attacker who can strike the specified /coordinates/, from the specified /direction/ (as seen by the target).

	* N.B.: there no requirement for there to actually be a /piece/ to attack at the specified target.
-}
findAttackerInDirection
	:: MaybePieceByCoordinates
	-> Colour.LogicalColour.LogicalColour					-- ^ The defender's /logical colour/.
	-> Cartesian.Coordinates.Coordinates					-- ^ The defender's square.
	-> Direction.Direction.Direction					-- ^ The /direction/ from the /coordinates/ of concern; the opposite /direction/ from which an attacker might strike.
	-> Maybe (Cartesian.Coordinates.Coordinates, Attribute.Rank.Rank)	-- ^ Any opposing /piece/ which can attack the specified square from the specified /direction/.
findAttackerInDirection maybePieceByCoordinates destinationLogicalColour destination direction	= findBlockingPiece maybePieceByCoordinates destination direction >>= \(source, sourcePiece) -> if Component.Piece.getLogicalColour sourcePiece /= destinationLogicalColour && Component.Piece.canAttackAlong source destination sourcePiece
	then Just (source, Component.Piece.getRank sourcePiece)
	else Nothing

{- |
	* Find the /coordinates/ of any attacker who can strike the specified /coordinates/, from the specified /direction/s (as seen by the target).

	* N.B.: one could call 'findAttackerInDirection' for each /direction/, but this function exploits optimisations available when all /direction/s are required.
-}
findAttackerInDirections
	:: MaybePieceByCoordinates
	-> Colour.LogicalColour.LogicalColour				-- ^ The defender's /logical colour/.
	-> Cartesian.Coordinates.Coordinates				-- ^ The defender's square.
	-> Maybe [Direction.Direction.Direction]			-- ^ The /direction/s from the /coordinates/ of concern; the opposite /direction/ from which an attacker might strike; 'Nothing' implies omni-directional.
	-> [(Cartesian.Coordinates.Coordinates, Attribute.Rank.Rank)]	-- ^ Any opposing /piece/s which can attack the specified square from the specified /direction/s.
findAttackerInDirections maybePieceByCoordinates destinationLogicalColour destination	= Data.Maybe.mapMaybe (
	\(source, sourcePiece) -> if Component.Piece.getLogicalColour sourcePiece /= destinationLogicalColour && Component.Piece.canAttackAlong source destination sourcePiece
		then Just (source, Component.Piece.getRank sourcePiece)
		else Nothing
 ) . findBlockingPieces maybePieceByCoordinates destination

-- | Whether the specified /coordinates/ are unoccupied.
isVacant :: MaybePieceByCoordinates -> Cartesian.Coordinates.Coordinates -> Bool
{-# INLINE isVacant #-}
isVacant MkMaybePieceByCoordinates { deconstruct = byCoordinates } coordinates
	| Nothing	<- byCoordinates ! coordinates	= True
	| otherwise					= False

-- | Whether the specified /coordinates/ are occupied.
isOccupied :: MaybePieceByCoordinates -> Cartesian.Coordinates.Coordinates -> Bool
isOccupied maybePieceByCoordinates	= not . isVacant maybePieceByCoordinates

{- |
	* Whether the open interval (source, destination) is unobstructed.

	* CAVEAT: the move must be straight, so that all intermediate points lie on squares of the board.

	* N.B.: the specified end-points are uninspected.
-}
isClear
	:: MaybePieceByCoordinates
	-> Cartesian.Coordinates.Coordinates	-- ^ Source.
	-> Cartesian.Coordinates.Coordinates	-- ^ Destination.
	-> Bool
isClear maybePieceByCoordinates source destination	= Control.Exception.assert (
	source /= destination && Property.Orientated.isStraight (Component.Move.mkMove source destination)
 ) . all (isVacant maybePieceByCoordinates) . init {-discard the destination-} $ Cartesian.Coordinates.interpolate source destination

-- | Whether there's a blockage between a /piece/ presumed to exist at the specified source, & a /piece/ presumed to exist @ the specified destination.
isObstructed
	:: MaybePieceByCoordinates
	-> Cartesian.Coordinates.Coordinates	-- ^ Source.
	-> Cartesian.Coordinates.Coordinates	-- ^ Destination.
	-> Bool
isObstructed maybePieceByCoordinates source	= not . isClear maybePieceByCoordinates source

{- |
	* Whether the specified /move/ matches the rules for /en-passant/.

	* CAVEAT: assumes that the /move/ is valid;
	otherwise one would also need to confirm that the opponent's @Pawn@ had just double-advanced into the appropriate position.
-}
isEnPassantMove :: MaybePieceByCoordinates -> Component.Move.Move -> Bool
isEnPassantMove maybePieceByCoordinates@MkMaybePieceByCoordinates { deconstruct = byCoordinates } move
	| Just piece	<- byCoordinates ! source
	, let logicalColour	= Component.Piece.getLogicalColour piece
	= Cartesian.Coordinates.isEnPassantRank source logicalColour && Component.Piece.isPawn piece && destination `elem` Component.Piece.findAttackDestinations piece source && isVacant maybePieceByCoordinates destination	-- The move is either En-passant or invalid.
	| otherwise	= False	-- No piece.
	where
		(source, destination)	= Component.Move.getSource &&& Component.Move.getDestination $ move

