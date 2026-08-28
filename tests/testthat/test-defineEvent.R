test_that("defineEvent creates a correctly named and shaped event function", {
  testInit()

  sim <- simInit()
  e <- new.env(parent = globalenv())

  defineEvent(sim, "grow", moduleName = "m1", code = { sim$size <- sim$size + 1 },
              envir = e)

  expect_true(exists("doEvent.m1.grow", envir = e, inherits = FALSE))
  fn <- get("doEvent.m1.grow", envir = e)
  expect_true(is.function(fn))
  expect_identical(names(formals(fn)), c("sim", "eventTime", "eventType", "priority"))
})

test_that("defineEvent defaults eventName to 'init'", {
  testInit()

  sim <- simInit()
  e <- new.env(parent = globalenv())

  defineEvent(sim, moduleName = "m1", code = { sim }, envir = e)

  expect_true(exists("doEvent.m1.init", envir = e, inherits = FALSE))
})

test_that("defineEvent leaves the code unevaluated until the event runs", {
  testInit()

  sim <- simInit()
  e <- new.env(parent = globalenv())

  # if `code` were evaluated at definition time, this would error immediately
  defineEvent(sim, "boom", moduleName = "m1",
              code = { stop("this code must not run at definition time") },
              envir = e)

  expect_true(exists("doEvent.m1.boom", envir = e, inherits = FALSE))
  expect_error(get("doEvent.m1.boom", envir = e)(sim), "must not run at definition time")
})

test_that("the generated event function runs the code and returns the sim", {
  testInit()

  sim <- simInit()
  sim$size <- 1
  e <- new.env(parent = globalenv())

  defineEvent(sim, "grow", moduleName = "m1", code = { sim$size <- sim$size + 1 },
              envir = e)

  out <- get("doEvent.m1.grow", envir = e)(sim)
  expect_s4_class(out, "simList")
  expect_identical(out$size, 2)
})

test_that("defineEvent appends a return(sim) so an event need not return it explicitly", {
  testInit()

  sim <- simInit()
  sim$touched <- FALSE
  e <- new.env(parent = globalenv())

  # note: no `return(sim)` and no trailing `sim` in the user's code
  defineEvent(sim, "quiet", moduleName = "m1", code = { sim$touched <- TRUE },
              envir = e)

  out <- get("doEvent.m1.quiet", envir = e)(sim)
  expect_s4_class(out, "simList")
  expect_true(out$touched)
})

test_that("defineEvent records the event function's envir and digest on the sim", {
  testInit()

  sim <- simInit()
  e <- new.env(parent = globalenv())

  defineEvent(sim, "grow", moduleName = "m1", code = { sim }, envir = e)

  slotName <- SpaDES.core:::eventFnElementEnvir()
  registry <- sim@.xData[[slotName]]

  expect_true(is.environment(registry))
  expect_true("doEvent.m1.grow" %in% ls(registry))

  entry <- registry[["doEvent.m1.grow"]]
  expect_identical(entry$envir, e)
  expect_true(is.character(entry$digest))
  expect_true(nzchar(entry$digest))
})

test_that("defineEvent registers each event separately for the same module", {
  testInit()

  sim <- simInit()
  e <- new.env(parent = globalenv())

  defineEvent(sim, "init", moduleName = "m1", code = { sim }, envir = e)
  defineEvent(sim, "grow", moduleName = "m1", code = { sim }, envir = e)

  expect_setequal(ls(e), c("doEvent.m1.init", "doEvent.m1.grow"))

  registry <- sim@.xData[[SpaDES.core:::eventFnElementEnvir()]]
  expect_true(all(c("doEvent.m1.init", "doEvent.m1.grow") %in% ls(registry)))
})

test_that("defineEvent returns the sim invisibly", {
  testInit()

  sim <- simInit()
  e <- new.env(parent = globalenv())

  expect_invisible(defineEvent(sim, "grow", moduleName = "m1", code = { sim }, envir = e))

  out <- defineEvent(sim, "grow2", moduleName = "m1", code = { sim }, envir = e)
  expect_s4_class(out, "simList")
})

test_that("events defined with defineEvent run under spades()", {
  skip_on_cran()
  testInit()

  sim <- simInit(times = list(start = 0, end = 2, timeunit = "year"))

  defineEvent(sim, "init", moduleName = "counter", code = {
    sim$n <- 0L
    sim <- scheduleEvent(sim, time(sim), "counter", "tick")
  })

  defineEvent(sim, "tick", moduleName = "counter", code = {
    sim$n <- sim$n + 1L
    sim <- scheduleEvent(sim, time(sim) + 1, "counter", "tick")
  })

  sim <- scheduleEvent(sim, 0, "counter", "init")
  expect_true("init" %in% events(sim)$eventType)

  out <- suppressMessages(spades(sim))

  expect_s4_class(out, "simList")
  expect_identical(out$n, 3L)  # ticks at t = 0, 1, 2
  expect_true(all(c("init", "tick") %in% completed(out)$eventType))
})

test_that("defineEvent registers the event even when a module environment exists", {
  ## Regression guard: placement into sim[[dotMods]][[moduleName]] is not
  ## implemented yet (it needs the .parse* integration). Until it is, the
  ## presence of a module environment must not stop the event being registered,
  ## or the function is defined somewhere nothing can find it.
  testInit()

  sim <- simInit()
  sim[[SpaDES.core:::dotMods]][["m1"]] <- new.env(parent = emptyenv())
  e <- new.env(parent = globalenv())

  defineEvent(sim, "grow", moduleName = "m1", code = { sim }, envir = e)

  registry <- sim@.xData[[SpaDES.core:::eventFnElementEnvir()]]
  expect_true("doEvent.m1.grow" %in% ls(registry))
  expect_identical(registry[["doEvent.m1.grow"]]$envir, e)
})
