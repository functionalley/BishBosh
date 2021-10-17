# 2018-01-01 Dr. Alistair Ward <bishbosh@functionalley.com>

## 0.0.0.1
* First version of the package.

## 0.0.0.2
* Minor changes required to build on Windows.

## 0.0.0.3
* Added **Paths_bishbosh** to **Other-modules** section of cabal file.

## 0.0.0.4
* Changed references to author's domain-name.

## 0.0.0.5
* Added ability to specify the text-encoding used in a PGN-database file.
* Updated list of test-compilers.

## 0.0.0.6
* Fixed failure to persist game-state after requesting that the game be restarted.
* Fixed parsing of **TextEncoding** in **PGNOptions**.
* Replaced module **Distribution.Verbosity** with **BishBosh.Input.Verbosity**.

## 0.0.0.7
* Rewrote function **BishBosh.Data.RoseTree.countTerminalNodes** in accordance with the suggestions of David Feuer.
* Amended function **BishBosh.State.EnPassantAbscissa.mkMaybeEnPassantAbscissa** to guard against exposing one's King after En-passant capture.

## 0.0.0.8
* Corrected the parsing of FEN when an Enpassant-destination defined on file **b** was erroneously interpreted as a bishop in the previous **CastleableRooks** field.
* Added parent class **BishBosh.Property.ExtendedPositionDescription.EPD** for **Property.ForsythEdwards.FEN**, for which the latter typically has a default implementation of both methods.

## 0.1.0.0
### Bug-fixes:
* Upgraded the transposition-table in module **Search.DynamicMoveData**, from merely recording moves (which doesn't include the rank to which a Pawn is promoted), to recording qualified-moves.
* In function **Search.AlphaBeta.negaMax.descend.selectMax**, amended bound function **isFitter** to prefer shorter move-sequences where fitness is equal, & corrected the scenario in which all nodes were skipped because they were repetitious, but without ever defining alpha.

### Features:
* Added the configurable runtime ability to asynchronously decompress PGN-databases, & to set a maximum number of games to read.
* Added a configuration-option to normalise the values of specified piece-square tables into the closed unit-interval.
* Modularised the packaged config-files, by defining XML **External Entities** in the DTD.
* Added suggestions on failure to parse a user-command, & created a module **Text.AutoComplete** to contain common code.
* Added **makefile** to facilitate common tasks.
* Removed the configuration-option **preferMovesTowardsCentre** & its implementation in function **Cartesian.Coordinates.radiusSquared**, because of it's conceptually wobbly foundations.

### New Runtime Cmmands:
-----------------------------------------
Command				| Purpose
--------------------------------| -------
**availableMoves**		| to report all available moves from the current position.
**maxPositionInstances**	| to reveal the maximum number of instances any available position has been visited.
**reversiblePlyCount**		| to count the number of consecutive reversible plies that have been made.
-----------------------------------------

### Command-line Options:
* Added a new module **Input.CategorisedCommandLineOptions** to improved the partitioning of command-line options into functional categories.
* Added a command-line option **--formatPieceSquareTableForGNUPlot** to print the piece-square tables in a format suitable for **GNUPlot**.

### Performance:
* Included a compilation-flag **unboxedarrays**, to request the use of unboxed arrays where (infrequently) possible.
* Changed data-type **Component.PieceSquareByCoordinatesByRank.EitherPieceSquareValueByNPiecesByCoordinates**, bringing type **Cartesian.Coordinates.ByCoordinates** inside **Either**, leading to significant space/time gains.
* Constructed each large constant data-structure in parallel. Bracketed all data-parallel operations with CPP-conditionals controlled by the compilation-flag **threaded**.
* Parallelised function **Attribute.CriterionValue.calculateWeightedMean**.

### New Modules:
-----------------------------------------
Module				| Purpose
--------------------------------| -------
**Component.CastlingMove**	| Forked from module **Component.Move**.
**Data.Enum**			| Currently single-function.
**Data.Foldable**		| Currently single-function.
**Property.FixedMembership**	| Defines a class to which sum-types can conform.
**StateProperty.Censor**	| Relocated from directory **State/**.
**StateProperty.Mutator**	| defines a class to express the dual implementations within **State.Board**.
**StateProperty.Seeker**	| defines a class to express the dual implementations within **State.Board**.
**Text.Case**			| Forked from **Text.ShowList** to contain case-related operations.
**Text.Prefix**			| Forked from **Text.ShowList** to define the constant prefixes of log-messages.
-----------------------------------------

### Testing:
* Split **src-test/Main.hs** into **src-test/HUnit.hs** & **src-test/QuickCheck.hs**, each referenced independently from the cabal file.
* Added an executable **duel** (to coordinate a battle between two independently configured instances of **bishbosh**) & a corresponding section-1 man-page.
* Validated the list of ranks supplied to construct either **Attribute.RankValues.RankValues** or **Input.PieceSquareTable.PieceSquareTable**.

### Refactoring:
* Flattened the nested array **Component.Zobrist.getRandomByCoordinatesByRankByLogicalColour**, by means of a composite index.
* Reimplemented function **Cartesian.Coordinates.getLogicalColourOfSquare**.
* Reimplemented function **Cartesian.Coordinates.interpolationsByDestinationBySource** in terms of function **Cartesian.Coordinates.extrapolationsByDirectionByCoordinates**.
* Used the **LambdaCase** language-extension.

## 0.1.1.0
### New Modules:
---------------------------------------------------------
New Module				| Purpose
----------------------------------------| -------
**BishBosh.Time.StopWatch**		| Replaces module **BishBosh.Data.Time** to encapsulate interaction with module **Data.Time.Clock**.
**BishBosh.Time.GameClock**		| Contains two **BishBosh.Time.StopWatch**es to enable module **Duel.Process.Intermediary** to measure the time used by each player.
**BishBosh.Property.Switchable**	| Exports a type-class, which both **BishBosh.Time.StopWatch** & **BishBosh.Time.GameClock** implement, to expose their functionality.
**BishBosh.Property.SelfValidating**	| Exports a type-class, which both **BishBosh.Time.GameClock** & **Duel.Data.Options** implement, to validate themselves.
**BishBosh.Type.Countable**		| Defines newtypes to enhance type-safety, replacing type-synonyms for **Int**. There is a performance-degradation, so this enhancement can be disabled using a new cabal-flag.
**BishBosh.Type.Crypto**		| Self-documentation.
**BishBosh.Type.Length**		| Replaced the polymorphic type-parameters **row** & **column** with newtypes to enhance type-safety.
**BishBosh.Type.Mass**			| Self-documentation.
**BishBosh.Metric.RankValue**		| Replaced the polymorphic type-parameter **rankValue**, with a newtype & a smart-constructor to guard permissible bounds.

### Duel:
* Added command-line option **--verifyConfiguration**, to request that the mutual compatibility of the two configuration-files be verified before forwarding each to a forked instance of **bishbosh**.

### BishBosh:
* Created a new directory **Rule/** to which **Model.**{**DrawReason**, **GameTerminationReason**, **Result**} were relocated.
* Refactored functions **BishBosh.ContextualNotation.PositionHashQualifiedMoveTree.findNextOnymousQualifiedMovesForPosition**, **BishBosh.Model.MoveFrequency.insertMoves** & **BishBosh.Model.GameTree.toMoveFrequency**.
* Evaluation-criteria:
	+ Moved **BishBosh.Attribute.**{**CriterionValue**, **CriterionWeight**, **WeightedMeanAndCriterionValues**} to a new directory **Metric/**
	+ Implemented classes [**Num**, **Fractional**, **Real**] for data-types **BishBosh.Metric.**{**CriterionValue.CriterionValue**, **CriterionWeight.CriterionWeight**}, nullifying the requirement for exports.
	+ Replaced the pointless polymorphic payloads in data-types **BishBosh.Metric.**{**CriterionValue.CriterionValue**, **CriterionWeight.CriterionWeight**, **WeightedMeanAndCriterionValues.WeightedMeanAndCriterionValues**} with concrete types.
* Moved **BishBosh.Attribute.RankValues** to **BishBosh.Input**.
* Checked that (with the possible exception of the King) the Queen is configured as the most valuable rank.
* Avoided repeated calls from module **BishBosh.Evaluation.Fitness** to function **IBishBosh.nput.RankValues.calculateMaximumTotalValue** by adding the record-field **BishBosh.Input.EvaluationOptions.getMaximumTotalRankValue**.

## 0.1.2.0
* Features:
	+ Added a Boolean configuration-option **BishBosh.Input.NativeUIOptions.getDepictFigurine** to specify whether the native UI should depict pieces using Unicode figurines rather than merely ASCII letters.
* Efficiency:
	+ Reimplemented function **BishBosh.Cartesian.Abscissa.getAdjacents** to promote memoisation.
	* Made **BishBosh.Cartesian.Coordinates** & **BishBosh.Attribute.Direction** strict.
* Bug-fixes:
	+ Avoided potential divide-by-zero @ **BishBosh.Input.CriteriaWeights.getWeightOfMaterial** / **BishBosh.Input.RankValues.calculateMaximumTotalValue** @.
	+ Guarded against starting from a persisted, but terminated, game.
	+ Added guards to function **BishBosh.Component.Piece.promote**.
	* Polymorphism:
		+ Replaced the polymorphic type **distance** in **BishBosh.Component.Vector**, with two concrete types **BishBosh.Type.Length.[XY]**. Removed the type **BishBosh.Type.Length.Distance**.
		+ Replaced the polymorphic type-parameters **x** & **y** with newtypes to:
			* eliminate the fragile **RULE** pragmas required to switch to memoised function-implementations for specific type-parameters,
			* eliminate chains of **SPECIALISE** pragmas down the call-stack to hot-spots.
			* allow external calls from [**Text.ParserCombinators.Poly.Lazy.runParser**, **Text.ParserCombinators.Parsec.parse**], to access specialised implementations (see previous items),
			* permit unification of the similar type-classes **BishBosh.Component.Zobrist.Hashable[12]D**, & to avoid the requirement for the **MultiParameterTypeClasses** pragma.
* Structural:
	+ Moved the type-class from module **BishBosh.Component.Zobrist** into a new module **BishBosh.StateProperty.Hashable**.
	+ Replaced non-specific type @ (Int, Int) @ coordinates in **BishBosh.Notation.{ICCFNumeric, PureCoordinate, Smith}** with @ (**BishBosh.Type.Length.X**, **BishBosh.Type.Length.Y**) @, & moved common code into a new module **BishBosh.Notation.Notation**.

