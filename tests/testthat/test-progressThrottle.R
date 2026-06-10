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

  # 5. C-level progress bar (e.g. archive::archive_extract(), whose bar lives in
  #    compiled libarchive code). cli's R-level registry never sees it, so
  #    cli_progress_num() stays 0 and case 2 cannot fire; this is the real
  #    archive-extraction flood reported on Windows. The frame is recognised by
  #    its leading Braille spinner glyph (cli's default spinner family).
  expect_equal(cli::cli_progress_num(), 0L)               # no R-level bar active
  brailleTick <- "\u2839 13 extracted | 2.3 GB ( 15 MB/s) | 2m 30.4s\n"
  expect_false(grepl("\r", brailleTick))                  # truly no control chars
  expect_true(.isCliProgressTick(cliCond(brailleTick), brailleTick))

  # 5b. A leading Braille glyph marks a tick even on a *base* (non-"cliMessage")
  #     condition: on Windows the archive frames do NOT carry the "cliMessage"
  #     class, so the Braille test must NOT be gated behind that class (else the
  #     flood gets through). Braille never begins a normal message, so safe.
  expect_true(.isCliProgressTick(baseCond(brailleTick), brailleTick))

  # 5c. Encoding-robustness: the same UTF-8 bytes with an "unknown" Encoding mark
  #     (as commonly produced, incl. on non-UTF-8 Windows locales) are still
  #     recognised, because the test matches the raw Braille UTF-8 bytes
  #     (useBytes = TRUE) rather than a code-point class that depends on the mark.
  brailleUnknown <- brailleTick
  Encoding(brailleUnknown) <- "unknown"
  expect_true(.isCliProgressTick(baseCond(brailleUnknown), brailleUnknown))

  # 5d. A cli alert whose symbol is not a spinner is not a tick, even with no bar.
  tickAlert <- "\u2714 finished extracting\n"             # heavy check mark
  expect_false(.isCliProgressTick(cliCond(tickAlert), tickAlert))

  # 5e. Prefixed C-level frame (nested handlers): the spinner arrives after a
  #     Date-Time-Module-Event prefix, so detection must be unanchored.
  prefixedTick <- paste0(
    "Jun10 14:43:51 simInit/simInit/Bmss_b:.inputObjects ",
    "\u2827 5 extracted | 1.2 GB (308 MB/s) | 4s\n")
  expect_false(grepl("\r", prefixedTick))                 # truly no control chars
  expect_true(.isCliProgressTick(cliCond(prefixedTick), prefixedTick))
  expect_true(.isCliProgressTick(baseCond(prefixedTick), prefixedTick))

  # 6. The blank frame cli emits to close out a C-level bar: an empty cliMessage
  #    is a tick only while a progress bar is already in progress, so the handler
  #    can muffle it instead of printing a bare, prefixed, empty line.
  blank <- "\n"
  oldInBar <- .pkgEnv$.inProgressBar
  on.exit(.pkgEnv$.inProgressBar <- oldInBar, add = TRUE)
  .pkgEnv$.inProgressBar <- TRUE
  expect_true(.isCliProgressTick(cliCond(blank), blank))
  .pkgEnv$.inProgressBar <- FALSE
  expect_false(.isCliProgressTick(cliCond(blank), blank))
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

test_that("C-level (archive-style) progress flood is throttled, not re-prefixed", {
  testInit(smcc = FALSE, debug = FALSE)
  skip_if_not_installed("cli")

  # This is the regression that actually shipped: a flood of Braille-spinner
  # progress frames (e.g. archive::archive_extract()) whose bar lives in compiled
  # code, so cli::cli_progress_num() == 0 and the frames carry no carriage return.
  # The earlier "non-dynamic cli progress" test could not catch it because it
  # registers a real R-level cli_progress_bar(), exercising a different detection
  # branch. Here every frame is exactly the kind that previously slipped past
  # .isCliProgressTick() and got a per-frame Date-Time-Module prefix (the flood).
  # Driven through a faithful replica of the spades()/simInit() handler throttle,
  # which calls the real .isCliProgressTick(): pre-fix it would emit one line per
  # frame (length(shown) == nFrames); post-fix it collapses to a handful.
  withr::local_options(spades.progressInterval = 2)

  expect_equal(cli::cli_progress_num(), 0L)   # no R-level bar: forces the C-level path
  cliCond <- function(msg)
    structure(
      class = c("cliMessage", "simpleMessage", "message", "condition"),
      list(message = msg, call = NULL)
    )

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
    shown[[length(shown) + 1L]] <<- msg
    tryCatch(invokeRestart("muffleMessage"), error = function(e) NULL)
  }

  # 40 realistic archive frames, all carrying a leading Braille spinner, emitted
  # in rapid succession (no \r, no active R-level bar) -- exactly the flood.
  nFrames <- 40L
  frames <- sprintf("\u2839 %d extracted | %.1f GB ( 15 MB/s) | 2m %d.4s\n",
                    seq_len(nFrames), seq_len(nFrames) / 10, seq_len(nFrames))
  withCallingHandlers(
    for (f in frames) message(cliCond(f)),
    message = handler
  )

  # Every frame is a Braille tick, so with spades.progressInterval = 2s and a
  # rapid burst only the first survives; pre-fix all 40 would have been prefixed.
  expect_lt(length(shown), nFrames)
  expect_gt(length(shown), 0L)
  expect_true(all(grepl("^[\u2800-\u28FF]", shown)))   # what survived really were ticks
})
