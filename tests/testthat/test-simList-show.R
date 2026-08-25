## The `show` method for simList prints a fixed set of sections. These assert
## that each section carries its actual content -- names AND values -- not just
## that a heading was printed.

showLines <- function(x) capture.output(show(x))

## text of one ">> <name>:" section, up to the next section heading
showSection <- function(out, name) {
  heads <- grep("^>>", out)
  i <- grep(paste0("^>> ", name), out)
  expect_length(i, 1L)
  nxt <- heads[heads > i]
  to <- if (length(nxt)) nxt[1] - 1L else length(out)
  paste(out[(i + 1L):to], collapse = "\n")
}

modSim <- function(tmpdir, end = 1) {
  mp <- getSampleModules(tmpdir)
  suppressMessages(
    simInit(times = list(start = 0, end = end, timeunit = "year"),
            modules = list("randomLandscapes"),
            paths = list(modulePath = mp))
  )
}

test_that("show prints every section of a simList", {
  testInit()

  out <- showLines(simInit())

  expect_setequal(
    grep("^>>", out, value = TRUE),
    c(">> Simulation dependencies:", ">> Simulation times:", ">> Modules:",
      ">> Objects Loaded:", ">> Objects stored:", ">> Parameters:",
      ">> Completed Events:", ">> Current Event:", ">> Scheduled Events:")
  )
})

test_that("show works on a bare simList", {
  testInit()

  expect_silent(out <- showLines(new("simList")))
  expect_true(any(grepl(">> Simulation times:", out)))
})

test_that("print dispatches to the show method", {
  testInit()

  sim <- simInit()
  expect_identical(capture.output(print(sim)), showLines(sim))
})

test_that("show points the user at depends() for module dependencies", {
  testInit()

  txt <- showSection(showLines(simInit()), "Simulation dependencies")
  expect_match(txt, "depends(sim)", fixed = TRUE)
})

test_that("show reports the actual start, end, current and timeunit", {
  testInit()

  sim <- simInit(times = list(start = 2, end = 7, timeunit = "day"))
  txt <- showSection(showLines(sim), "Simulation times")

  ## column labels ...
  expect_match(txt, "current")
  expect_match(txt, "start")
  expect_match(txt, "end")
  ## ... and the values themselves, on one row
  valueRow <- grep("day", strsplit(txt, "\n")[[1]], value = TRUE)
  expect_length(valueRow, 1L)
  expect_match(valueRow, "\\b2\\b")
  expect_match(valueRow, "\\b7\\b")
})

test_that("show lists each loaded module with its timeunit", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  txt <- showSection(showLines(modSim(tmpdir)), "Modules")

  expect_match(txt, "Name")
  expect_match(txt, "Timeunit")
  ## the module and its timeunit must appear together on one row
  modRow <- grep("randomLandscapes", strsplit(txt, "\n")[[1]], value = TRUE)
  expect_length(modRow, 1L)
  expect_match(modRow, "year")
})

test_that("show lists stored objects with their names and types", {
  testInit()

  sim <- simInit()
  sim$anEagerObject <- 1:5
  sim$aCharacterObject <- "hello"

  txt <- showSection(showLines(sim), "Objects stored")

  ## ls.str() prints "name : type spec", so both name and type must show
  expect_match(txt, "anEagerObject")
  expect_match(txt, "int")
  expect_match(txt, "aCharacterObject")
  expect_match(txt, "chr")
})

test_that("show flags lazily bound objects by name, separately from eager ones", {
  testInit()

  sim <- simInit()
  sim$anEagerObject <- 1:5
  delayedAssign("aLazyObject", 1:5, assign.env = envir(sim))

  txt <- showSection(showLines(sim), "Objects stored")

  lazyLine <- grep("Lazy (not yet loaded)", strsplit(txt, "\n")[[1]],
                   fixed = TRUE, value = TRUE)
  expect_length(lazyLine, 1L)
  expect_match(lazyLine, "aLazyObject")
  ## the eager object must NOT be on the lazy line
  expect_false(grepl("anEagerObject", lazyLine, fixed = TRUE))
  ## but must still be listed
  expect_match(txt, "anEagerObject")
})

test_that("show prints each parameter's module, name and value together", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  txt <- showSection(showLines(modSim(tmpdir)), "Parameters")
  rows <- strsplit(txt, "\n")[[1]]

  expect_match(txt, "Module")
  expect_match(txt, "Parameter")
  expect_match(txt, "Value")

  ## a known parameter of the sample module, with its declared default
  nxRow <- grep("\\bnx\\b", rows, value = TRUE)
  expect_length(nxRow, 1L)
  expect_match(nxRow, "randomLandscapes")
  expect_match(nxRow, "100")

  stackRow <- grep("stackName", rows, value = TRUE)
  expect_length(stackRow, 1L)
  expect_match(stackRow, "landscape")

  ## the .progress core module is deliberately filtered out of this table
  expect_false(any(grepl("\\.progress", rows)))
})

test_that("show reports scheduled events with module, type and time", {
  testInit()

  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))
  sim <- scheduleEvent(sim, 0, "someModule", "someEvent")

  txt <- showSection(showLines(sim), "Scheduled Events")
  ourRow <- grep("someModule", strsplit(txt, "\n")[[1]], value = TRUE)

  expect_length(ourRow, 1L)
  expect_match(ourRow, "someEvent")
})

test_that("show reports completed events with their modules and types after a run", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  out <- suppressMessages(spades(modSim(tmpdir)))
  txt <- showSection(showLines(out), "Completed Events")
  rows <- strsplit(txt, "\n")[[1]]

  expect_match(txt, "moduleName")
  expect_match(txt, "eventType")

  ## the module actually ran its init, and that is visible here
  initRow <- grep("randomLandscapes.*\\binit\\b", rows, value = TRUE)
  expect_gte(length(initRow), 1L)

  ## and the completed table shown matches what completed() holds
  expect_true(all(unique(completed(out)$moduleName) |>
                    vapply(function(m) any(grepl(m, rows)), logical(1))))
})

test_that("show reports an empty scheduled-events queue once the run is done", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  out <- suppressMessages(spades(modSim(tmpdir)))
  txt <- showSection(showLines(out), "Scheduled Events")

  expect_match(txt, "0 rows")
})
