test_that(".isCliProgressTick detects progress in dynamic and non-dynamic cli modes", {
  testInit(smcc = FALSE, debug = FALSE)
  skip_if_not_installed("cli")

  # Build condition objects like those reaching the spades()/simInit() message
  # handler: cli output carries the "cliMessage" class, base message() does not.
  cliCond <- function(msg)
    structure(
      class = c("cliMessage", "simpleMessage", "message", "condition"),
      list(message = msg, call = NULL)
    )
  baseCond <- function(msg) simpleMessage(msg)

  # 1. Dynamic-mode frame: a carriage return marks an in-place overwrite, so it is
  #    a tick regardless of condition class.
  cr <- "\rextracting 50%"
  expect_true(.isCliProgressTick(baseCond(cr), cr))

  # 1b. A cursor-control CSI sequence (here: erase-line) also marks a tick, while
  #     a colour/SGR code (ends in 'm') must not.
  csi <- "\x1b[Kextracting"
  expect_true(.isCliProgressTick(cliCond(csi), csi))
  sgr <- "\x1b[31mred regular message\x1b[39m"
  expect_false(.isCliProgressTick(baseCond(sgr), sgr))

  # 2. Non-dynamic cli progress tick: a plain newline-terminated line with no
  #    control characters, emitted while a cli progress bar is active. This is the
  #    case that previously slipped past detection and flooded the logs.
  withr::local_options(cli.dynamic = FALSE, cli.ansi = FALSE,
                       cli.progress_show_after = 0)
  local({
    id <- cli::cli_progress_bar("extracting", total = 100, clear = FALSE,
                                .auto_close = FALSE)
    on.exit(cli::cli_progress_done(id = id), add = TRUE)
    cli::cli_progress_update(id = id, set = 50, force = TRUE)
    plain <- "extracting 50% | ETA: 0s\n"
    expect_false(grepl("\r", plain))                       # truly no control chars
    expect_true(.isCliProgressTick(cliCond(plain), plain)) # but still a tick
  })

  # 3. A cli alert is also class "cliMessage" but has no active progress bar, so it
  #    must NOT be treated as a tick (alerts should pass through, not be throttled).
  expect_equal(cli::cli_progress_num(), 0L)
  alert <- "i a cli alert\n"
  expect_false(.isCliProgressTick(cliCond(alert), alert))

  # 4. A plain base message is never a tick.
  msg <- "a normal log message\n"
  expect_false(.isCliProgressTick(baseCond(msg), msg))
})

test_that("non-dynamic cli progress is throttled, not re-prefixed per frame", {
  testInit(smcc = FALSE, debug = FALSE)
  skip_if_not_installed("cli")

  # Replicate the spades()/simInit() message handler's throttle around a flood of
  # non-dynamic cli progress ticks (no \r, no cursor codes). Before the fix every
  # frame fell through and was prefixed individually; now they collapse to a
  # handful of throttled lines via getOption("spades.progressInterval").
  withr::local_options(cli.dynamic = FALSE, cli.ansi = FALSE,
                       cli.progress_show_after = 0, spades.progressInterval = 0.5)

  pe <- new.env(parent = emptyenv())
  pe$.inProgressBar <- FALSE
  pe$.progressLastShown <- NULL
  shown <- character(0)

  handler <- function(m) {
    msg <- m$message
    if (.isCliProgressTick(m, msg)) {
      clean <- trimws(cli::ansi_strip(msg))
      if (nchar(clean) == 0L) {
        tryCatch(invokeRestart("muffleMessage"), error = function(e) NULL)
        return()
      }
      now <- Sys.time()
      if (!isTRUE(pe$.inProgressBar)) {
        pe$.inProgressBar <- TRUE
        pe$.progressLastShown <- now
        shown[[length(shown) + 1L]] <<- clean
      } else if (as.numeric(now - pe$.progressLastShown) >=
                 getOption("spades.progressInterval", 2)) {
        shown[[length(shown) + 1L]] <<- clean
        pe$.progressLastShown <- now
      }
      tryCatch(invokeRestart("muffleMessage"), error = function(e) NULL)
      return()
    }
    pe$.inProgressBar <- FALSE
    shown[[length(shown) + 1L]] <<- m$message
    tryCatch(invokeRestart("muffleMessage"), error = function(e) NULL)
  }

  nTicks <- 40L
  withCallingHandlers({
    id <- cli::cli_progress_bar("extracting", total = nTicks, clear = FALSE,
                                .auto_close = FALSE)
    for (i in seq_len(nTicks)) {
      cli::cli_progress_update(id = id, set = i, force = TRUE)
      Sys.sleep(0.02)
    }
    cli::cli_progress_done(id = id)
  }, message = handler)

  # Far fewer than nTicks lines survive (one initial + a few throttled updates);
  # the unfixed path would have emitted one prefixed line per tick.
  expect_lt(length(shown), nTicks)
  expect_gt(length(shown), 0L)
})
