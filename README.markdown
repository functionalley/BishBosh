# **BishBosh**

[![Build Status](https://travis-ci.org/functionalley/BishBosh.svg?branch=master)](https://travis-ci.org/functionalley/BishBosh)
[![Hackage](https://img.shields.io/hackage/v/bishbosh.svg)](https://hackage.haskell.org/package/bishbosh)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Haskell](https://b.repl.ca/v1/language-haskell-yellow.png)](https://haskell.org)

This is "**BishBosh**", a chess-game which can be rendered in a terminal (emulator) using raw ASCII, or used as an engine by **xboard**.

## Installation

It can be built and installed using [Cabal](https://www.haskell.org/cabal/users-guide/installing-packages.html).

## Documentation

The documentation is in "**man/**".

## License

For information on copying and distributing this package, see the file "**LICENSE**" in this directory.

## Bug-reporting

Bug-reports should be emailed to <bishbosh@functionalley.com>.

## Testing

The test-suite can be run using:

    cabal configure --enable-tests;
    cabal build;
    cabal test --show-details=always;

It's currently only been tested on **Linux**.

## Author

This application is written and maintained by Dr. Alistair Ward.
