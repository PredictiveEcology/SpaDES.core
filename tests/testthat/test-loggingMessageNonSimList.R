## loggingMessage() looks up the `sim` on the call stack for the module prefix. A `sim`
## that is not a simList (a module helper's local variable) must not stop it finding
## the real one, nor make it loop forever.

test_that("loggingMessage skips a non-simList `sim` on the stack and finds the simList", {
  withr::local_options(spades.messagingNumCharsModule = 21)
  setTimeLimit(elapsed = 10, transient = TRUE) # the old code never returned
  on.exit(setTimeLimit(), add = TRUE)

  inner <- function() {
    sim <- 1:3
    loggingMessage("hello")
  }
  outer <- function() {
    sim <- simInit()
    sim[[._txtSimNesting]] <- "modA"
    inner()
  }
  expect_match(outer(), "modA hello$")
})

test_that("loggingMessage with only a non-simList `sim` on the stack returns the message without prefix", {
  withr::local_options(spades.messagingNumCharsModule = 21)
  setTimeLimit(elapsed = 10, transient = TRUE)
  on.exit(setTimeLimit(), add = TRUE)

  f <- function() {
    sim <- 1:3
    loggingMessage("hello")
  }
  expect_no_error(res <- f())
  expect_match(res, "^[A-Za-z]{3}[0-9]{2} [0-9:]{8}  hello$")
})
