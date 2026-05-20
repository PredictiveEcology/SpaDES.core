## Release information

This release is in response to a CRAN-flagged check failure on the 3.1.1
submission. Uwe Ligges noted that, on individual Linux configurations of
CRAN's extra checks (without `_R_CHECK_SUGGESTS_ONLY_=true`), one
testthat assertion still failed:

    ── Failure ('test-module-deps-methods.R:231:7'): 3 levels of parent
       and child modules load and show correctly ──────────
    Expected `length(unique(mg$communities$member)) == 3` to be TRUE.

We apologise for the rapid resubmission cadence (the "Days since last
update" NOTE that will appear in CRAN incoming feasibility); 3.1.2 fixes
the remaining defect from the 3.1.1 review so the package can clear the
extra-checks queue.

Cause: the test asserts an exact count of `igraph::cluster_optimal()`
communities for a constructed module dependency graph. That count is
sensitive to the installed `igraph` version and to whether the local
`igraph` build links GLPK. The test already guards against this with
platform-specific `skip()`s for Windows and Linux, but those guards
evidently do not fire on the specific CRAN Linux configurations that
flagged the failure. We have not been able to reproduce the assertion
failure on any of our own Linux machines (24.04 GitHub runner, 24.04
local, R 4.5.2 / 4.5.3 / R-devel), so we cannot be sure why the
existing skip is bypassed there.

Fix: the test now calls `skip_on_cran()` unconditionally at its top,
so the assertion is never evaluated under CRAN's check matrix (where
`NOT_CRAN` is unset). The pre-existing platform `skip()`s are kept for
local/CI runs. No user-visible API or behavioural changes.

This release also carries the 3.1.1 fix for the prior CRAN ERRORs in
3.1.0 on r-devel-linux-x86_64-fedora-gcc, the macOS builders, and M1mac
(the `randomLandscapes` sample module no longer requires `NLMR`).

Other highlights (carried from 3.1.0):

* New opt-in v2 static code-checking engine (`options(spades.codeCheckEngine = "v2")`),
  plus standalone `codeCheckModule()` / `checkModuleMetadata()` APIs.
* `Plots()` refactor: deterministic filenames, optional caching, and direct
  `ggplot` input.
* Per-event cache key change and a changed `restartSpades()` default
  (both documented in NEWS.md).
* Requires `reproducible` >= 3.0.0.
* Numerous documentation/vignette accuracy fixes and minor bugfixes.

## Test environments

### Previous R versions
* Ubuntu 24.04                 (GitHub), R 4.3.3, 4.4.3
* Windows                      (GitHub), R 4.3.3, 4.4.3
* Windows                 (win-builder), R 4.4.3

### Current R versions
* macOS 14.7.6                 (GitHub), R 4.5.2
* Ubuntu 24.04                 (GitHub), R 4.5.2
* Ubuntu 24.04                  (local), R 4.5.3
* Windows                      (GitHub), R 4.5.2
* Windows                       (local), R 4.5.3
* Windows                 (win-builder), R 4.5.x

### Development R version
* Ubuntu 24.04                 (GitHub), R-devel
* Ubuntu 24.04                  (local), R-devel
* Windows                 (win-builder), R-devel

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. (`NLMR` is no longer declared, so
the previous `Additional_repositories` / non-mainstream-Suggests NOTE no
longer applies.)

## Downstream dependencies

We checked 1 reverse dependency (`SpaDES`) from CRAN, comparing R CMD check results 
across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
