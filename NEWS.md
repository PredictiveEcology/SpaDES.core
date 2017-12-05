Known issues: https://github.com/PredictiveEcology/SpaDES.core/issues

version 0.1.0.9000
==================

* remove `grDevices` from Imports as it was not used (#1)
* remove `chron` and `CircStats` dependencies
* remove functions `dwrpnorm2` and move to package `SpaDES.tools`
* remove unused function `F()` due to conflicts with `F`/`FALSE`.
* improved download of module data: added new `quickCheck` argument
* improved download of modules: use fuzzy matching
* new option: `spades.switchPkgNamespaces` which allows the user to turn off the `SpaDES` feature that loads and unloads libraries specific to each module. While useful, it slows down computations, in some cases, by a lot.
* fix bug in `zipModule` that omitted the checksum file from being included when `data = FALSE` (#3)
* default `debug` option in `spades()` now uses the package option `spades.debug` and default is set to `FALSE` (#5)
* various other speed improvements and bug fixes

version 0.1.0
=============

* A new package, which takes all core DES functionality out of the `SpaDES` package:

    - see `?SpaDES.core` for an overview

* various speed improvements and bug fixes
