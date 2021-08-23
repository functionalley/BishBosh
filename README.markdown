# **BishBosh**

[![Build Status](https://travis-ci.com/functionalley/BishBosh.svg?branch=master)](https://travis-ci.com/functionalley/BishBosh)
[![Hackage](https://img.shields.io/hackage/v/bishbosh.svg)](https://hackage.haskell.org/package/bishbosh)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Haskell](https://b.repl.ca/v1/language-haskell-yellow.png)](https://haskell.org)

This is "**BishBosh**", a chess-game which can be rendered in a terminal (emulator) using raw ASCII, or used as an engine by [xboard](https://www.gnu.org/software/xboard/).

## Installation

It can be built & installed using:

* [Cabal](https://www.haskell.org/cabal/),
* [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).

## Documentation

The documentation is in the product's "**man/**" directory & all source-code is documented in [haddock](https://www.haskell.org/haddock/).

## License

For information on copying & distributing this package, see the file "**LICENSE**" in the product's installation-directory.

## Bug-reporting

Bug-reports should be emailed to <bishbosh@functionalley.com>.

## Testing

The test-suite can be run using:

* "**cabal configure --enable-tests && cabal build && cabal test --show-details=always**",
* "**stack test**",
* "**make test**" issued from the product's installation-directory.

It has only been tested on **Linux**.

## Author

This application is written & maintained by Dr. Alistair Ward.
