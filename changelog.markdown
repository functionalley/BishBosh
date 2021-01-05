# 2018-01-01 Dr. Alistair Ward <bishbosh@functionalley.com>

## 0.0.0.1
* First version of the package.

## 0.0.0.2
Minor changes required to build on Windows.

## 0.0.0.3
Added **Paths_bishbosh** to **Other-modules** section of cabal file.

## 0.0.0.4
Changed references to author's domain-name.

## 0.0.0.5
Added ability to specify the text-encoding used in a PGN-database file.
Updated list of test-compilers.

## 0.0.0.6
Fixed failure to persist game-state after requesting that the game be restarted.
Fixed parsing of **TextEncoding** in **PGNOptions**.
Replaced module **Distribution.Verbosity** with **BishBosh.Input.Verbosity**.

## 0.0.0.7
Rewrote function **BishBosh.Data.RoseTree.countTerminalNodes** in accordance with the suggestions of David Feuer.
Amended function **BishBosh.State.EnPassantAbscissa.mkMaybeEnPassantAbscissa** to guard against exposing one's King after En-passant capture.

## 0.0.0.8
Corrected the parsing of FEN when an Enpassant-destination defined on file **b** was erroneously interpreted as a bishop in the previous **CastleableRooks** field.
Added parent class **BishBosh.Property.ExtendedPositionDescription.EPD** for **Property.ForsythEdwards.FEN**, for which the latter typically has a default implementation of both methods.
