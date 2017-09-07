Known issues: https://github.com/PredictiveEcology/SpaDES.core/issues

version 0.1.0.9000
==================

* remove `grDevices` from Imports as it was not used (#1)
* fix bug in `zipModule` that omitted the checksum file from being included when `data = FALSE` (#3)

version 0.1.0
=============

* A new package, which takes all core DES functionality out of the `SpaDES` package:

    - see `?SpaDES.core` for an overview

* various speed improvements and bug fixes
