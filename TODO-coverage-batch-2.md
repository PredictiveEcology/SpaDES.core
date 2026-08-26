# Overnight coverage work — 2026-08-25/26

## Where things stand

| | coverage |
|---|---|
| start of the coverage effort | 60.98% |
| end of the previous batch (#390) | 76.00% |
| **after tonight** | **80.97%** |

Eleven new test files, ~150 tests. **No production code was changed** — you said
any code change had to be 100% or not made at all, and every candidate fix
either needed a decision from you or was in a destructive code path.

Local suite: `0 failures, 1 skip` (the pre-existing igraph/GLPK skip).

## The 8% you saw is real, but it is not on `development`

`development` still reads 67.32% on codecov. What you were looking at is #394's
branch.

| branch | codecov |
|---|---|
| `development` | 67.32% |
| **#394** `fix/declare-sf-suggests` | **75.64%** |
| **#398** `test/coverage-batch-2` (tonight) | **70.87%** |

#398 is measured against `development`, so most sample-module tests still skip
there. #394 and #398 touch different files; landing both should put codecov near
79%.

## #394 is blocked — and it is not #394's fault

Four of five Windows `R-CMD-check` legs fail on `test-save.R:223`. I re-ran them;
it is deterministic, not a flake.

It is **not** a regression. That test never runs on CI today:

```
development, windows-latest (release): [ FAIL 0 | WARN 0 | SKIP 65 | PASS 1203 ]
  • {sf} is not installed (64): ... 'test-save.R:162:3' ...
```

`caribouMovement` declared `sf` in `reqdPkgs` while `sf` was never in
`DESCRIPTION`, so `simInit()` could not load it on CI and every sample-module
test skipped. Removing the stale `sf` un-skips 64 tests — and
`saveSimList works correctly` turns out to have been broken on Windows all
along: `.grd`-backed rasters do not survive the save/load round trip there.

Filed as **#397**. Your call:

1. `skip_on_os("windows")` on that one test, merge #394 now, fix separately.
   Gets the +8% immediately but re-masks the bug.
2. Fix the Windows round trip first.

I did not choose for you.

## CI on #398

One test of mine broke CI on the first push — the `archiveWrite`/`archiveExtract`
round trip assumed the `archive::` branch, which is not taken when `archive` is
missing or on Windows. Fixed in 98b657d5.

Everything failing since is `test-downloadModule.R:95` returning HTTP 403, filed
as **#399**. It rotates between matrix legs run to run, and every leg has now
passed on at least one run. The reusable workflow does set `GITHUB_PAT`, but the
test took its no-token branch, so the request is going out unauthenticated and
hitting the shared 60/hr IP quota across ~12 concurrent legs.

## Verified bug, deliberately not fixed

**`clearCacheEventsOnly(x = )` clears the wrong cache.** It reads the entries
from `x` but calls `clearCache()` without `x`, so the deletion happens in
`getOption("reproducible.cachePath")`.

Verified both ways: with the option pointed at `x`, 3 cached entries become 1
(the two event entries removed, correctly). Without it, nothing is removed and
nothing is said. The fix is `clearCache(x = x, cacheId = y, ...)` — one
argument — but it makes a *delete* function start deleting somewhere new, so I
left it alone. The tests pin the intended behaviour without asserting the
broken path.

## Other things found, all left alone

1. **`moduleDiagram(type = )`** — `if (grep("plot", type, ...))` evaluates to
   `if (integer(0))`, an error, so the `stop("type must be one of 'rgl', 'tk',
   'Plot' or 'plot'")` below it is unreachable. Wants `grepl()`.
2. **`archiveConvertFileExt()`** — `tools::file_ext("sim.tar.gz")` is `"gz"`, so
   converting to zip yields `sim.tar.zip`. The `gsub` is also unanchored and
   applied to the whole path, so a directory named `gz` gets rewritten too.
   Neither is pinned by a test until it is decided what is right.
3. **Leftover `browser()`** at `simulation-spades.R:2592`, inside
   `allowSequentialCaching1()`. The condition also reads as though it meant
   `length(nextEvent) > 1` rather than `length(nextEvent != ...) > 1`.
4. **`moduleCoverage()`** opens with
   `stop("This is a stub that is not intended for use")` — 32 lines of
   unreachable code behind it, and it is not exported. Finish it or delete it.
5. **`ongoingMemoryThisPid(interval = )`** takes an `interval` argument but the
   loop sleeps by `getOption("spades.memoryUseInterval")`, ignoring it. Only the
   loop bound uses the argument. The tests set the option rather than assert
   this.
6. **`saveSimList()` with module sources outside `projectPath`** — I could not
   get a test to reach that branch reliably, and one attempt failed inside
   `archive::archive_write_files()` with "`files` must be one or more readable
   file paths" that I could not reproduce standalone. Dropped the test rather
   than commit something I could not explain. The branch is still uncovered.

## What got covered

| file | before | after |
|---|---|---|
| `R/memory.R` | **0.0** | 91.9 |
| `R/plotting-diagrams.R` | **0.0** | 88.8 |
| `R/codecheck-rules.R` | 79.9 | 97.2 |
| `R/module-template.R` | 79.9 | 90.1 |
| `R/load.R` | 73.9 | 82.8 |
| `R/saveLoadSimList.R` | 60.8 | 66.1 |
| `R/module-define.R` | 83.0 | 86.8 |
| `R/cache.R` | 81.8 | 83.9 |
| `R/simList-accessors.R` | 84.5 | 87.2 |

No file went down.

New files:

- `test-plotting-diagrams.R` — `ganttStatus`, `.sim2gantt`, all three
  `eventDiagram` methods, `objectDiagram`, `moduleDiagram`, `moduleGraph`
- `test-memory.R` — the `ps` sampler, `memoryUse()`, and the `future.callr`
  setup/teardown around `spades()`
- `test-saveLoadSimList-helpers.R` — archive naming, path relativisation, the
  deep environment cloner, `.wrapAnchors`
- `test-module-template-extras.R` — `openModules()` (with `.fileEdit` mocked),
  `zipModule()`, `checkModulePath()`
- `test-codecheck-rules.R` — the `.ccr_*` rules driven directly
- `test-module-define-extras.R` — `defineParameter()` edge cases
- `test-rasterToMemory.R` — `rasterToMemory` / `rasterCreate`, terra and raster
- `test-outputs-rmDups.R` — `rmDups()`, including the AsIs columns
- `test-clearCacheEventsOnly.R`
- `test-parameters-accessor.R` — `parameters()`, `newObjectsCreated()`,
  `inputArgs<-`

## Still open from before

- #388 `loadSimList()` module re-parse
- #389 file-backed objects outside every named path (related to #397)
- reproducible #575 (ETag / `preProcessCheckURLs`) still open
- SpaDES.tools `release/2.1.3` prepped locally but never pushed
- 3.2.0 release prep is stale — `development` has moved well past `52b83515`;
  needs a fresh `--as-cran` and a `NEWS.md` section
