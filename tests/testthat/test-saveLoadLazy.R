## Round-trip tests for the lazy saveSimList / loadSimList path: one sidecar
## file per object under `<filename sans ext>_lazy/`, restored as promises.
## A minimal `new("simList")` keeps the test fast and isolates the
## lazy mechanism from simInit/spades plumbing.

test_that("lazy saveSimList/loadSimList round-trip restores user objects via promises", {
  skip_if_not_installed("rlang")

  td <- normPath(withr::local_tempdir())
  simPaths <- list(cachePath = td, inputPath = td, outputPath = td,
                   modulePath = td, scratchPath = td, terraPath = td)

  sim <- new("simList")
  paths(sim) <- simPaths
  sim@.xData[["a"]] <- 1:5
  sim@.xData[["b"]] <- letters[1:3]
  sim@.xData[["c"]] <- list(x = 10, y = "z")

  filename <- file.path(td, "sim.rds")
  lazyDir <- file.path(td, "sim_lazy")

  saveSimList(sim, filename = filename, files = FALSE, lazy = TRUE,
              projectPath = td)

  expect_true(file.exists(filename))
  expect_true(dir.exists(lazyDir))
  expect_true(file.exists(file.path(lazyDir, "_manifest.rds")))
  ## one file per object, plus the manifest
  expect_length(dir(lazyDir), 4L)

  loaded <- loadSimList(filename, projectPath = td)

  loadedEnv <- loaded@.xData
  userNms <- c("a", "b", "c")
  expect_true(all(userNms %in% ls(loadedEnv)))
  expect_true(all(rlang::env_binding_are_lazy(loadedEnv, userNms)))

  expect_equal(loaded$a, 1:5)
  expect_equal(loaded$b, letters[1:3])
  expect_equal(loaded$c, list(x = 10, y = "z"))

  expect_false(rlang::env_binding_are_lazy(loadedEnv, "a"))
})

test_that("lazy round-trip materializes a file-backed terra SpatRaster on access", {
  skip_if_not_installed("rlang")
  skip_if_not_installed("terra")

  td <- normPath(withr::local_tempdir())
  simPaths <- list(cachePath = td, inputPath = td, outputPath = td,
                   modulePath = td, scratchPath = td, terraPath = td)

  ## Tiny on-disk raster — kept in place via files = FALSE on save.
  rastFile <- file.path(td, "tiny.tif")
  rastVals <- matrix(1:9, nrow = 3)
  terra::writeRaster(terra::rast(rastVals), rastFile, overwrite = TRUE)

  sim <- new("simList")
  paths(sim) <- simPaths
  sim@.xData[["plain"]] <- 42L
  sim@.xData[["r"]]     <- terra::rast(rastFile)

  filename <- file.path(td, "sim.rds")
  saveSimList(sim, filename = filename, files = FALSE, lazy = TRUE,
              projectPath = td)
  expect_true(dir.exists(file.path(td, "sim_lazy")))
  expect_true(file.exists(rastFile))  # files = FALSE -> backing file untouched

  loaded <- loadSimList(filename, projectPath = td)

  expect_true(all(rlang::env_binding_are_lazy(loaded@.xData, c("plain", "r"))))

  ## Force the raster — promise should resolve to a SpatRaster pointing at
  ## the original on-disk file, with values intact.
  rOut <- loaded$r
  expect_s4_class(rOut, "SpatRaster")
  expect_equal(terra::values(rOut, mat = FALSE),
               terra::values(terra::rast(rastFile), mat = FALSE))
  expect_false(rlang::env_binding_are_lazy(loaded@.xData, "r"))
  expect_true(rlang::env_binding_are_lazy(loaded@.xData, "plain"))
})

test_that("saveSimList does not mutate the caller's simList (Path-wrap leak)", {
  ## Regression: .wrapResiliently used to rebind sim@.xData[[nm]] to Path
  ## objects in place. .xData is an environment, so the caller's live sim
  ## ended up with Path-wrapped rasters after every saveSimList call —
  ## visibly broke the checkpoint module (which calls saveSimList mid-run)
  ## and any code that touched the sim afterwards.
  skip_if_not_installed("terra")

  td <- normPath(withr::local_tempdir())
  simPaths <- list(cachePath = td, inputPath = td, outputPath = td,
                   modulePath = td, scratchPath = td, terraPath = td)

  rastFile <- file.path(td, "tiny.tif")
  terra::writeRaster(terra::rast(matrix(1:9, 3)), rastFile, overwrite = TRUE)

  sim <- new("simList")
  paths(sim) <- simPaths
  sim@.xData[["r"]] <- terra::rast(rastFile)
  sim@.xData[["x"]] <- 42L
  sim@.xData[["nested"]] <- new.env(parent = emptyenv())
  sim@.xData[["nested"]]$inner <- "untouched"

  saveSimList(sim, filename = file.path(td, "sim.rds"),
              files = FALSE, projectPath = td)

  expect_s4_class(sim$r, "SpatRaster")
  expect_false(inherits(sim$r, "Path"))
  expect_identical(sim$x, 42L)
  expect_identical(sim$nested$inner, "untouched")
})

test_that("lazy saveSimList omits the sidecar directory when there is nothing to defer", {
  td <- normPath(withr::local_tempdir())
  simPaths <- list(cachePath = td, inputPath = td, outputPath = td,
                   modulePath = td, scratchPath = td, terraPath = td)

  sim <- new("simList")
  paths(sim) <- simPaths

  filename <- file.path(td, "empty.rds")
  lazyDir <- file.path(td, "empty_lazy")

  saveSimList(sim, filename = filename, files = FALSE, lazy = TRUE,
              projectPath = td)

  expect_true(file.exists(filename))
  expect_false(dir.exists(lazyDir))

  loaded <- loadSimList(filename, projectPath = td)
  expect_s4_class(loaded, "simList")
})

test_that("lazy round-trip defers `mod` objects instead of discarding them", {
  ## Regression: loadSimList() used to *delete* every .modObjs binding when it
  ## saw a lazy save, because .modObjs was assumed to hold only per-module copies
  ## of file-backed objects whose backing files might be gone. `mod` state was
  ## therefore silently lost on every lazy round-trip -- while .reparseModules()
  ## goes out of its way to preserve exactly that state.
  skip_if_not_installed("rlang")

  td <- normPath(withr::local_tempdir())
  simPaths <- list(cachePath = td, inputPath = td, outputPath = td,
                   modulePath = td, scratchPath = td, terraPath = td)

  sim <- new("simList")
  paths(sim) <- simPaths
  sim@.xData[["userObj"]] <- 1:5
  sim@.xData[[".mods"]] <- new.env(parent = emptyenv())
  sim@.xData[[".modObjs"]] <- new.env(parent = emptyenv())
  ## The objects live in .modObjs; .mods carries only the `mod`/`Par` active
  ## bindings that point at them. Build both, as simInit() would -- but leave
  ## `modules(sim)` empty so .reparseModules() has no module folders to find.
  for (m in c("modA", "modB")) {
    sim@.xData[[".mods"]][[m]] <- new.env(parent = asNamespace("SpaDES.core"))
    sim@.xData[[".modObjs"]][[m]] <- new.env(parent = emptyenv())
    SpaDES.core:::makeModActiveBinding(sim = sim, mod = m)
    SpaDES.core:::makeParActiveBinding(sim = sim, mod = m)
  }
  sim@.xData[[".modObjs"]][["modA"]]$scratch <- data.frame(a = 1:100)
  sim@.xData[[".modObjs"]][["modB"]]$other <- "kept"

  filename <- file.path(td, "sim.rds")
  ## suppressWarnings: .wrap.simList() unconditionally rm()s the `mod`/`Par`
  ## bindings from each .mods env, but Copy(objects = 2) only re-creates them for
  ## modules listed in modules(sim) -- empty here, since this fixture has no real
  ## module folders for .reparseModules() to find. A real sim never warns.
  suppressWarnings(
    saveSimList(sim, filename = filename, files = FALSE, lazy = TRUE,
                projectPath = td)
  )

  ## the shell itself must not carry them
  shell <- readRDS(filename)
  expect_length(ls(shell@.xData[[".modObjs"]][["modA"]], all.names = TRUE), 0L)
  expect_false("userObj" %in% ls(shell@.xData, all.names = TRUE))

  loaded <- loadSimList(filename, projectPath = td)

  modAEnv <- loaded@.xData[[".modObjs"]][["modA"]]
  modBEnv <- loaded@.xData[[".modObjs"]][["modB"]]

  ## present, and still unforced
  expect_true("scratch" %in% ls(modAEnv, all.names = TRUE))
  expect_true(rlang::env_binding_are_lazy(modAEnv, "scratch"))
  expect_true(rlang::env_binding_are_lazy(modBEnv, "other"))

  ## and they resolve to what went in
  expect_equal(modAEnv$scratch, data.frame(a = 1:100))
  expect_identical(modBEnv$other, "kept")
  expect_false(rlang::env_binding_are_lazy(modAEnv, "scratch"))

  ## touching one module's object leaves the other's alone
  expect_true(rlang::env_binding_are_lazy(loaded@.xData, "userObj"))
})

test_that("reading simList metadata does not force any lazy object", {
  ## The point of the feature: reload a finished run, read outputs(sim) and
  ## params, and pay nothing for the objects.
  skip_if_not_installed("rlang")

  td <- normPath(withr::local_tempdir())
  simPaths <- list(cachePath = td, inputPath = td, outputPath = td,
                   modulePath = td, scratchPath = td, terraPath = td)

  sim <- new("simList")
  paths(sim) <- simPaths
  sim@.xData[["big1"]] <- 1:1000
  sim@.xData[["big2"]] <- letters

  filename <- file.path(td, "sim.rds")
  saveSimList(sim, filename = filename, files = FALSE, lazy = TRUE, projectPath = td)
  loaded <- loadSimList(filename, projectPath = td)

  invisible(ls(loaded))
  invisible(outputs(loaded))
  invisible(paths(loaded))
  invisible(exists("big1", envir = loaded@.xData, inherits = FALSE))

  expect_true(all(rlang::env_binding_are_lazy(loaded@.xData, c("big1", "big2"))))
})

test_that("lazy handles a value too large for a lazy-load database", {
  ## tools::makeLazyLoadDB cannot store a single value over 2 GB
  ## ("long vectors not supported yet: connections.c"), which is why the
  ## sidecar files exist at all -- `mod` objects are routinely larger.
  skip_on_cran()
  skip_on_ci()
  skip_if_not(identical(Sys.getenv("R_SPADES_RUN_BIG_MEMORY_TESTS"), "true"),
              "set R_SPADES_RUN_BIG_MEMORY_TESTS=true to run (needs ~8 GB RAM)")
  skip_if_not_installed("rlang")

  td <- normPath(withr::local_tempdir())
  simPaths <- list(cachePath = td, inputPath = td, outputPath = td,
                   modulePath = td, scratchPath = td, terraPath = td)

  sim <- new("simList")
  paths(sim) <- simPaths
  sim@.xData[[".modObjs"]] <- new.env(parent = emptyenv())
  sim@.xData[[".modObjs"]][["modA"]] <- new.env(parent = emptyenv())
  sim@.xData[[".modObjs"]][["modA"]]$huge <- raw(2.5 * 1024^3)

  filename <- file.path(td, "sim.rds")
  ## the point: this neither errors nor truncates. tools::makeLazyLoadDB would
  ## fail here with "long vectors not supported yet: connections.c".
  suppressWarnings(
    saveSimList(sim, filename = filename, files = FALSE, lazy = TRUE,
                projectPath = td)
  )
  expect_true(dir.exists(file.path(td, "sim_lazy")))

  loaded <- loadSimList(filename, projectPath = td)
  modAEnv <- loaded@.xData[[".modObjs"]][["modA"]]
  expect_true(rlang::env_binding_are_lazy(modAEnv, "huge"))
  expect_equal(length(modAEnv$huge), 2.5 * 1024^3)
})

## ---------------------------------------------------------------------------
## .DollarNames.simList: $-completion must not materialise a lazy simList.
##
## RStudio types each candidate with `object$name` unless the completions
## already carry an integer `types` attribute. Without one, opening the
## completion popup on a lazily loaded simList reads the entire sidecar.
## ---------------------------------------------------------------------------

test_that(".DollarNames.simList lists names without forcing lazy bindings", {
  skip_if_not_installed("rlang")

  td <- normPath(withr::local_tempdir())
  simPaths <- list(cachePath = td, inputPath = td, outputPath = td,
                   modulePath = td, scratchPath = td, terraPath = td)
  sim <- new("simList")
  paths(sim) <- simPaths
  sim@.xData[["alpha"]] <- 1:5
  sim@.xData[["beta"]]  <- letters[1:3]

  filename <- file.path(td, "sim.rds")
  suppressMessages(saveSimList(sim, filename = filename, lazy = TRUE))
  s <- suppressMessages(loadSimList(filename))

  expect_true(all(rlang::env_binding_are_lazy(s@.xData, c("alpha", "beta"))))

  nms <- utils::.DollarNames(s, pattern = "")
  expect_true(all(c("alpha", "beta") %in% nms))
  ## the point: still unforced afterwards
  expect_true(all(rlang::env_binding_are_lazy(s@.xData, c("alpha", "beta"))))
})

test_that(".DollarNames.simList honours `pattern`", {
  td <- normPath(withr::local_tempdir())
  sim <- new("simList")
  sim@.xData[["alpha"]] <- 1L
  sim@.xData[["beta"]]  <- 2L
  expect_identical(utils::.DollarNames(sim, pattern = "^al"), "alpha")
})

test_that(".DollarNames.simList adds `types` only when something is lazy", {
  skip_if_not_installed("rlang")

  ## No RStudio in a test session, so .rsUnknownCompletionType() is NULL and no
  ## attribute is ever set. Fake the IDE's constant table to exercise the path.
  td <- normPath(withr::local_tempdir())
  simPaths <- list(cachePath = td, inputPath = td, outputPath = td,
                   modulePath = td, scratchPath = td, terraPath = td)
  sim <- new("simList")
  paths(sim) <- simPaths
  sim@.xData[["alpha"]] <- 1:5
  filename <- file.path(td, "sim.rds")
  suppressMessages(saveSimList(sim, filename = filename, lazy = TRUE))

  fake <- new.env()
  assign(".rs.acCompletionTypes", list(UNKNOWN = 99L), envir = fake)
  attach(fake, name = "tools:rstudio", warn.conflicts = FALSE)
  on.exit(detach("tools:rstudio"), add = TRUE)

  s <- suppressMessages(loadSimList(filename))
  lazyNms <- utils::.DollarNames(s, pattern = "")
  ty <- attr(lazyNms, "types")
  expect_true(is.integer(ty))
  expect_identical(length(ty), length(lazyNms))   # RStudio requires both
  expect_true(all(ty == 99L))

  ## once forced, no attribute -- let the IDE compute real icons again
  invisible(as.list(s@.xData))
  expect_null(attr(utils::.DollarNames(s, pattern = ""), "types"))
})

## ---------------------------------------------------------------------------
## loadSimList(fetch = ): the sidecar objects need not be local. `fetch` is
## called with the expected path when an object is absent and must put the
## bytes there. The point is that nothing else changes -- `$`, `[[` and get()
## behave as always, and an object is transferred only if something touches it.
## ---------------------------------------------------------------------------

test_that("fetch is called per object, only on access, and only once", {
  skip_if_not_installed("rlang")
  td <- normPath(withr::local_tempdir())
  simPaths <- list(cachePath = td, inputPath = td, outputPath = td,
                   modulePath = td, scratchPath = td, terraPath = td)
  sim <- new("simList"); paths(sim) <- simPaths
  sim@.xData[["aa"]] <- 1:5
  sim@.xData[["bb"]] <- letters[1:3]
  sim@.xData[["cc"]] <- list(z = 1)

  filename <- file.path(td, "sim.rds")
  suppressMessages(saveSimList(sim, filename = filename, lazy = TRUE))
  lazyDir <- file.path(td, "sim_lazy")

  ## move the objects out of reach, leaving only the manifest
  store <- file.path(td, "store"); dir.create(store)
  objs <- setdiff(dir(lazyDir), .lazyManifestName)
  file.rename(file.path(lazyDir, objs), file.path(store, objs))
  expect_identical(dir(lazyDir), .lazyManifestName)

  seen <- character(0)
  myFetch <- function(path) {
    seen <<- c(seen, basename(path))
    file.copy(file.path(store, basename(path)), path)
  }

  s <- suppressMessages(loadSimList(filename, fetch = myFetch))
  ## binding promises must not fetch anything
  expect_length(seen, 0L)
  expect_true(all(rlang::env_binding_are_lazy(s@.xData, c("aa", "bb", "cc"))))

  expect_identical(s$bb, letters[1:3])
  expect_length(seen, 1L)                                   # exactly one object
  expect_true(all(rlang::env_binding_are_lazy(s@.xData, c("aa", "cc"))))

  ## whatever fetch wrote is the cache: a second access must not re-fetch
  invisible(s$bb)
  expect_length(seen, 1L)

  expect_identical(s$aa, 1:5)
  expect_length(seen, 2L)                                   # `cc` never touched
})

test_that("fetch defaults to NULL and local lazy loading is unchanged", {
  skip_if_not_installed("rlang")
  td <- normPath(withr::local_tempdir())
  simPaths <- list(cachePath = td, inputPath = td, outputPath = td,
                   modulePath = td, scratchPath = td, terraPath = td)
  sim <- new("simList"); paths(sim) <- simPaths
  sim@.xData[["aa"]] <- 1:5
  filename <- file.path(td, "sim.rds")
  suppressMessages(saveSimList(sim, filename = filename, lazy = TRUE))

  s <- suppressMessages(loadSimList(filename))
  expect_true(rlang::env_binding_are_lazy(s@.xData, "aa"))
  expect_identical(s$aa, 1:5)
})

test_that(".readOneLazy only calls fetch when the file is missing", {
  td <- normPath(withr::local_tempdir())
  f <- file.path(td, "obj.rds"); saveRDS(1:3, f)
  called <- FALSE
  expect_identical(.readOneLazy(f, fetch = function(p) called <<- TRUE), 1:3)
  expect_false(called)                       # present -> fetch not called

  unlink(f)
  got <- .readOneLazy(f, fetch = function(p) { called <<- TRUE; saveRDS(1:3, p) })
  expect_true(called)
  expect_identical(got, 1:3)
})

test_that("a lazy save keeps the files behind file-backed objects, so they load on another machine", {
  ## A file-backed object should load like any other R object. Before, `files = FALSE` (as
  ## SpaDES.project::outSave() saves) kept only each object's path: one whose file was in a
  ## cache, scratch or temporary folder loaded as NULL anywhere else, and a list of rasters
  ## failed with "replacement has length zero".
  skip_if_not_installed("rlang")
  skip_if_not_installed("terra")
  mk <- function(root) {
    p <- list(cachePath = "cache", inputPath = "inputs", outputPath = "outputs",
              modulePath = "modules", scratchPath = "scratch", terraPath = "scratch/terra")
    p <- lapply(p, function(x) file.path(root, x))
    lapply(p, dir.create, recursive = TRUE, showWarnings = FALSE)
    p
  }
  here <- normPath(withr::local_tempdir()); there <- normPath(withr::local_tempdir())
  pA <- mk(here); pB <- mk(there)
  r <- terra::rast(nrows = 3, ncols = 3, vals = 1:9)
  w <- function(path) { dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE); terra::writeRaster(r, path) }

  sim <- new("simList")
  paths(sim) <- pA
  sim@.xData[["inCache"]] <- w(file.path(pA$cachePath, "spat_1_2_a.tif"))     # as a cache hit leaves it
  sim@.xData[["inTerra"]] <- w(file.path(pA$terraPath, "spat_1_2_b.tif"))
  sim@.xData[["inOutputs"]] <- w(file.path(pA$outputPath, "climate", "CMD.tif"))
  sim@.xData[["aList"]] <- list(CMD = sim@.xData[["inOutputs"]],
                                MAT = w(file.path(pA$cachePath, "spat_1_2_c.tif")))
  sim@.xData[[".mods"]] <- new.env(parent = emptyenv())
  sim@.xData[[".modObjs"]] <- new.env(parent = emptyenv())
  sim@.xData[[".mods"]][["modA"]] <- new.env(parent = asNamespace("SpaDES.core"))
  sim@.xData[[".modObjs"]][["modA"]] <- new.env(parent = emptyenv())
  SpaDES.core:::makeModActiveBinding(sim = sim, mod = "modA")
  SpaDES.core:::makeParActiveBinding(sim = sim, mod = "modA")
  sim@.xData[[".modObjs"]][["modA"]]$standAge <- w(file.path(pA$terraPath, "spat_1_2_d.tif"))

  filename <- file.path(pA$outputPath, "sim.rds")
  suppressWarnings(saveSimList(sim, filename = filename, files = FALSE, lazy = TRUE, projectPath = here))

  ## the other machine has the simList file and its sidecar directory, and none of `here`
  file.copy(c(filename, file.path(pA$outputPath, "sim_lazy")), pB$outputPath, recursive = TRUE)
  unlink(here, recursive = TRUE)

  loaded <- suppressWarnings(loadSimList(file.path(pB$outputPath, "sim.rds"), projectPath = there,
                                         paths = pB))
  vals <- function(x) as.vector(terra::values(x))
  for (nm in c("inCache", "inTerra", "inOutputs")) {
    expect_s4_class(loaded[[nm]], "SpatRaster")
    expect_identical(vals(loaded[[nm]]), 1:9, label = nm)
  }
  expect_identical(vals(loaded$aList$CMD), 1:9)
  expect_identical(vals(loaded$aList$MAT), 1:9)
  expect_identical(vals(loaded@.xData[[".modObjs"]][["modA"]]$standAge), 1:9)
  ## an object that was in outputPath goes back to (this machine's) outputPath
  expect_true(fs::path_has_parent(terra::sources(loaded$inOutputs), pB$outputPath))
  ## a file from a temporary place (cache, terra's folder) goes only into the sidecar directory,
  ## never into this machine's cache or scratch
  srcs <- c(terra::sources(loaded$inCache), terra::sources(loaded$inTerra), terra::sources(loaded$aList$MAT),
            terra::sources(loaded@.xData[[".modObjs"]][["modA"]]$standAge))
  expect_true(all(fs::path_has_parent(srcs, file.path(pB$outputPath, "sim_lazy"))))
  expect_identical(terra::sources(loaded$aList$CMD), terra::sources(loaded$inOutputs))
})

test_that("a lazy save leaves the caller's file-backed objects where they are", {
  skip_if_not_installed("terra")
  td <- normPath(withr::local_tempdir())
  p <- list(cachePath = file.path(td, "cache"), outputPath = file.path(td, "outputs"),
            inputPath = file.path(td, "inputs"), modulePath = file.path(td, "modules"),
            scratchPath = file.path(td, "scratch"))
  lapply(p, dir.create, recursive = TRUE, showWarnings = FALSE)
  sim <- new("simList")
  paths(sim) <- p
  f <- file.path(p$cachePath, "spat_1_2_a.tif")
  sim@.xData[["inCache"]] <- terra::writeRaster(terra::rast(nrows = 2, ncols = 2, vals = 1:4), f)
  saveSimList(sim, filename = file.path(p$outputPath, "sim.rds"), files = FALSE, lazy = TRUE,
              projectPath = td)
  expect_identical(normPath(terra::sources(sim$inCache)), normPath(f))
  ## loaded on the same machine, with the original still there, it is not pointed back into the cache
  loaded <- loadSimList(file.path(p$outputPath, "sim.rds"), projectPath = td)
  expect_true(fs::path_has_parent(terra::sources(loaded$inCache), file.path(p$outputPath, "sim_lazy")))
  expect_identical(as.vector(terra::values(loaded$inCache)), 1:4)
})
