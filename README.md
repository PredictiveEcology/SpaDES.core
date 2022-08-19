<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/SpaDES.core)](https://cran.r-project.org/package=SpaDES.core)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/SpaDES.core)](https://cran.r-project.org/package=SpaDES.core)
[![R build status](https://github.com/PredictiveEcology/SpaDES.core/workflows/R-CMD-check/badge.svg)](https://github.com/PredictiveEcology/SpaDES.core/actions)
<!-- badges: end -->

<img align="right" width="80" pad="20" src="https://github.com/PredictiveEcology/SpaDES/raw/master/man/figures/SpaDES.png">

# SpaDES.core

Core functionality for Spatial Discrete Event Simulation (SpaDES)

**Website:** [https://SpaDES.PredictiveEcology.org](https://SpaDES.PredictiveEcology.org)

**Wiki:** [https://github.com/PredictiveEcology/SpaDES/wiki](https://github.com/PredictiveEcology/SpaDES/wiki)

## Installation

### Current stable release

[![R build status](https://github.com/PredictiveEcology/SpaDES.core/workflows/R-CMD-check/badge.svg?branch=master)](https://github.com/PredictiveEcology/SpaDES.core/actions)
[![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/SpaDES.core/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/SpaDES.core?branch=master)

**Install from CRAN:**

```r
install.packages("SpaDES.core")
```

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES.core", dependencies = TRUE) # master
```

### Development version (unstable)

[![R build status](https://github.com/PredictiveEcology/SpaDES.core/workflows/R-CMD-check/badge.svg?branch=development)](https://github.com/PredictiveEcology/SpaDES.core/actions)
[![codecov](https://codecov.io/gh/PredictiveEcology/SpaDES.core/branch/development/graph/badge.svg?token=uz2mzVq1vJ)](https://app.codecov.io/gh/PredictiveEcology/SpaDES.core)
**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES.core", ref = "development", dependencies = TRUE)
```

## Contributions

Please see `CONTRIBUTING.md` for information on how to contribute to this project.
