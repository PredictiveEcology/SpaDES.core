## The event-flow figure. Coordinates are inches and text sizes are points, laid out for a given
## `width` (by default the PDF text width, the narrowest output), so text keeps its point size in
## a PDF and is only scaled up on the web.

utils::globalVariables(c(
  "x", "y", "xend", "yend", "id", "fill", "col", "lty", "label", "hjust", "text", "top", "band",
  "dx", "kind", "labelW", "detailW", "priority"
))

.ef_colors <- list(
  setup = "#dbe9f6", start = "#e8f0f8", cycle = "#e3f1e3", end = "#ece5f4", times = "#f3eee4",
  off = "#f1f1f1", edge = "#01665e", text = "#2b2121", detail = "grey30", grey = "grey60",
  context = "grey93"
)

.ef_pt <- function(x) x / ggplot2::.pt

.ef_orNA <- function(a, b) if (length(a) != 1 || is.na(a)) b else a

## The font family for the figure: option `spades.eventFlowFont` (default "Noto Sans") if that
## font is installed, otherwise the generic sans-serif family.
.ef_font <- function() {
  fam <- getOption("spades.eventFlowFont", "Noto Sans")
  if (!is.character(fam) || length(fam) != 1 || is.na(fam)) return("sans")
  if (fam %in% c("sans", "serif", "mono")) return(fam)
  if (requireNamespace("systemfonts", quietly = TRUE) &&
      fam %in% systemfonts::system_fonts()$family) fam else "sans"
}

## width in inches of the widest line of each string, as ragg will draw it
.ef_tw <- function(s, pt, family = .ef_font(), bold = FALSE) {
  if (!length(s)) return(0)
  k <- if (bold) 1.07 else 1
  vapply(s, function(x) {
    lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
    if (!length(lines)) return(0)
    k * max(systemfonts::string_width(lines, family = family, size = pt, res = 72)) / 72
  }, numeric(1), USE.NAMES = FALSE)
}

## greedy word wrap to a width in inches
.ef_wrap <- function(s, maxW, pt, family = .ef_font()) {
  vapply(s, function(x) {
    if (is.na(x) || !nzchar(x)) return(.ef_orNA(x, ""))
    words <- strsplit(x, " ", fixed = TRUE)[[1]]
    lines <- character()
    cur <- ""
    for (w in words) {
      cand <- if (nzchar(cur)) paste(cur, w) else w
      if (nzchar(cur) && .ef_tw(cand, pt, family) > maxW) {
        lines <- c(lines, cur)
        cur <- w
      } else {
        cur <- cand
      }
    }
    paste(c(lines, cur), collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
}

.ef_nlines <- function(s) lengths(regmatches(s, gregexpr("\n", s))) + 1L

.ef_lineh <- function(pt) pt * 1.25 / 72 # line height in inches

.ef_rrect <- function(xmin, xmax, ymin, ymax, r = 0.05, n = 6) {
  rr <- min(r, (xmax - xmin) / 2, (ymax - ymin) / 2)
  th <- seq(0, pi / 2, length.out = n)
  cx <- c(xmax - rr, xmin + rr, xmin + rr, xmax - rr)
  cy <- c(ymax - rr, ymax - rr, ymin + rr, ymin + rr)
  do.call(rbind, lapply(1:4, function(k) {
    a <- th + (k - 1) * pi / 2
    data.frame(x = cx[k] + rr * cos(a), y = cy[k] + rr * sin(a))
  }))
}

## Polygons are drawn in the order added: geom_polygon() draws groups in sorted id order, hence
## the zero-padded ids.
.ef_polys <- function() {
  e <- new.env()
  e$items <- list()
  e$add <- function(p, fill, col, lty = "solid") {
    p$id <- sprintf("%05d", length(e$items) + 1L)
    p$fill <- fill
    p$col <- col
    p$lty <- lty
    e$items[[length(e$items) + 1L]] <- p
  }
  e$df <- function() do.call(rbind, e$items)
  e
}

## ---- rows of the timestep-cycle figure ------------------------------------------------------

## `fl` is one flow of a moduleEventFlow object, `tu` its time unit and `context` a list of
## moduleEventFlow objects of other modules.
.ef_flowRows <- function(fl, tu, context = list(), expandInit = NA, offOnly = NULL) {
  ev <- fl$events
  live <- Filter(function(e) e$status %in% c("on", "cond"), ev)
  off <- Filter(function(e) e$status == "off", ev)
  if (!is.null(offOnly)) off <- ev[intersect(offOnly, names(ev))]
  rec <- Filter(function(e) isTRUE(e$recurring), live)
  once <- Filter(function(e) !isTRUE(e$recurring), live)
  ## when a one-off event runs: only at the start, only at the end, or at other set times
  when <- function(e) {
    f <- e$firsts
    w <- ifelse(f$timeType == "end", "end",
                ifelse(f$timeType == "start" | (f$timeType == "at" & f$timeValue %in% 0), "start",
                       "times"))
    if (all(w == "start")) "start" else if (all(w == "end")) "end" else "times"
  }
  atStart <- Filter(function(e) when(e) == "start", once)
  atTimes <- Filter(function(e) when(e) == "times", once)
  atEnd <- Filter(function(e) when(e) == "end", once)
  ## init-only modules show the module functions init calls, in order, when there are two or more
  if (is.na(expandInit)) expandInit <- length(rec) == 0
  expandInit <- expandInit && length(fl$initCalls) >= 2

  row <- function(band, label, detail = "", priority = NA, status = "on", kind = "event",
                  font = "plain") {
    data.frame(band = band, label = label, detail = detail, priority = priority,
               status = status, kind = kind, font = font, stringsAsFactors = FALSE)
  }
  rows <- list()
  add <- function(r) rows[[length(rows) + 1L]] <<- r
  if (isTRUE(fl$hasInputObjects)) {
    add(row("setup", ".inputObjects",
            "during simInit: supplies a default for each input not provided by the user or another module"))
  }
  calls <- if (length(fl$initCalls)) paste0(fl$initCalls, "()", collapse = ", ") else ""
  add(row("setup", "init", if (expandInit) "calls, in order:" else
    if (nzchar(calls)) paste("calls", calls) else ""))
  if (expandInit) {
    for (f in fl$initCalls) add(row("setup", paste0(f, "()"), kind = "step", font = "mono"))
  }
  ord <- function(l) l[order(vapply(l, function(e) .ef_orNA(e$priority, 5), numeric(1)), names(l))]
  for (e in ord(atStart)) {
    add(row("start", e$event, .ef_timingText(e, tu), e$priority, e$status))
  }
  cyc <- lapply(rec, function(e) {
    row("cycle", e$event, .ef_timingText(e, tu), e$priority, e$status)
  })
  for (cf in context) {
    ctu <- if (length(cf$timeunit) == 1 && !is.na(cf$timeunit)) cf$timeunit else "time unit"
    for (e in Filter(function(e) isTRUE(e$recurring) && e$status == "on" && !e$plotsave,
                     cf$default$events)) {
      cyc[[length(cyc) + 1]] <- row("cycle", e$event,
        paste0(cf$module, " event; ", .ef_intervalText(e, ctu)), e$priority, "on", "context")
    }
  }
  if (length(cyc)) {
    cyc <- do.call(rbind, cyc)
    add(cyc[order(cyc$priority, cyc$kind == "context", cyc$label), ])
  }
  for (e in ord(atTimes)) {
    add(row("times", e$event, .ef_timingText(e, tu), e$priority, e$status))
  }
  for (e in ord(atEnd)) {
    add(row("end", e$event, .ef_timingText(e, tu), e$priority, e$status))
  }
  ## events that the default parameters switch off, grouped by the setting that would run them
  if (length(off)) {
    why <- vapply(off, function(e) .ef_orNA(e$cond, "unknown"), character(1))
    for (w in unique(why)) {
      add(row("off", paste(names(off)[why == w], collapse = ", "),
              if (startsWith(w, "depends on")) w else paste("runs if", w), kind = "off"))
    }
  }
  rows <- do.call(rbind, rows)
  attr(rows, "unscheduled") <- names(Filter(function(e) e$status == "unscheduled", ev))
  rows
}

## rows for every run mode: the shared set-up, then each mode's bands, then what is off in all
.ef_modeRows <- function(x, context = list(), expandInit = NA) {
  tu <- x$timeunit
  if (is.null(x$modes)) {
    rows <- .ef_flowRows(x$default, tu, context, expandInit)
    rows$mode <- NA_character_
    return(rows)
  }
  anyRec <- any(vapply(x$modes, function(f) {
    any(vapply(f$events, function(e) isTRUE(e$recurring) && e$status != "off", logical(1)))
  }, logical(1)))
  if (is.na(expandInit)) expandInit <- !anyRec
  offAll <- Reduce(intersect, lapply(x$modes, function(f) names(f$status)[f$status == "off"]))
  base <- .ef_flowRows(x$default, tu, context, expandInit, offOnly = offAll)
  out <- base[base$band == "setup", ]
  out$mode <- NA_character_
  for (m in names(x$modes)) {
    r <- .ef_flowRows(x$modes[[m]], tu, context, expandInit = FALSE)
    r <- r[!(r$band %in% c("setup", "off")), ]
    if (!nrow(r)) {
      r <- data.frame(band = "start", label = "(nothing scheduled)", detail = "", priority = NA,
                      status = "on", kind = "event", font = "plain")
    }
    r$mode <- m
    out <- rbind(out, r)
  }
  off <- base[base$band == "off", ]
  if (nrow(off)) {
    off$mode <- NA_character_
    out <- rbind(out, off)
  }
  attr(out, "unscheduled") <- attr(base, "unscheduled")
  out
}

## ---- the figure ------------------------------------------------------------------------------

## `x`: a moduleEventFlow object; `context`: character vector of module directories whose
## recurring, non-plot events are drawn in grey, for their order relative to this module's.
## Returns invisibly the file, the figure height in inches and the laid-out rows.
.ef_plotFlow <- function(x, file, width = 4.65, context = NULL) {
  for (pkg in c("ggplot2", "ragg", "systemfonts")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required to draw an event flow; please install it.")
    }
  }
  fam <- .ef_font()
  ctx <- lapply(context, function(d) {
    d <- normalizePath(d, mustWork = FALSE)
    tryCatch(moduleEventFlow(basename(d), dirname(d)), error = function(e) NULL)
  })
  ctx <- Filter(Negate(is.null), ctx)

  fl <- x$default
  rows <- .ef_modeRows(x, ctx)
  tu <- if (length(x$timeunit) == 1 && !is.na(x$timeunit)) x$timeunit else "time unit"
  p <- x$modeParam
  bandTitle <- c(
    setup = "Set-up: simInit and the init event",
    start = "Once, at the start",
    cycle = paste0("Each ", tu, ", in priority order (lower numbers first)"),
    times = "At other set times",
    end = "Once, at the end",
    off = if (is.null(p)) "Off with the default parameters" else
      paste0("Off with the default parameters, in every ", p)
  )
  ptL <- 7.5 # event names
  ptD <- 7 # details
  ptB <- 7.5 # band titles
  inset <- 0.12 # bands inside a run-mode frame
  rows$dx <- ifelse(is.na(rows$mode), 0, inset)
  rows$bkey <- ifelse(is.na(rows$mode), rows$band, paste(rows$mode, rows$band, sep = "|"))

  evLab <- rows$label[rows$kind %in% c("event", "context")]
  boxW <- max(0.9, min(0.42 * width, max(.ef_tw(evLab, ptL, fam)) + 0.20))
  xs <- list(band0 = 0.04, band1 = width - 0.04)
  xs$loop <- xs$band0 + 0.11
  xs$chip0 <- xs$band0 + 0.20
  xs$chip1 <- xs$chip0 + 0.30
  xs$box0 <- xs$chip1 + 0.06
  xs$box1 <- xs$box0 + boxW
  xs$det <- xs$box1 + 0.09
  stepLab <- rows$label[rows$kind == "step"]
  xs$step0 <- xs$box0 + 0.10
  xs$step1 <- min(xs$band1 - 0.10, xs$step0 + max(0.8, .ef_tw(stepLab, ptL, "mono") + 0.25))
  xc <- (xs$box0 + xs$box1) / 2

  rows$labelW <- rows$label
  rows$detailW <- ""
  ev <- rows$kind %in% c("event", "context")
  rows$detailW[ev] <- mapply(function(d, w) .ef_wrap(d, w, ptD, fam), rows$detail[ev],
                             xs$band1 - 0.06 - xs$det - 2 * rows$dx[ev])
  off <- rows$kind == "off"
  rows$labelW[off] <- .ef_wrap(rows$label[off], xs$det - 0.10 - xs$chip0, ptD, fam)
  rows$detailW[off] <- .ef_wrap(rows$detail[off], xs$band1 - 0.06 - xs$det, ptD, fam)
  rows$h <- pmax(ifelse(rows$kind %in% c("step", "off"), 0.20, 0.26),
                 pmax(.ef_nlines(rows$detailW), .ef_nlines(rows$labelW)) * .ef_lineh(ptD) + 0.09)

  title <- .ef_wrap(paste0(x$module, ": event flow"), width - 0.10, 10, fam)
  sub <- "as read from the module code, with its default parameters"
  if (!is.null(p)) sub <- paste0(sub, "; one frame per value of ", p)
  sub <- .ef_wrap(sub, width - 0.10, ptD, fam)
  titleY <- -0.05
  subY <- titleY - .ef_nlines(title) * .ef_lineh(10) - 0.02
  y <- subY - .ef_nlines(sub) * .ef_lineh(ptD) - 0.08

  bands <- list()
  frames <- list()
  notes <- list()
  arr <- list()
  rows$y <- NA_real_
  keys <- unique(rows$bkey)
  for (j in seq_along(keys)) {
    b <- keys[j]
    i <- which(rows$bkey == b)
    m <- rows$mode[i[1]]
    prevMode <- if (j > 1) rows$mode[rows$bkey == keys[j - 1]][1] else NA
    if (!is.na(m) && !identical(m, prevMode)) {
      ## entering a run-mode frame: the fork after set-up, or "or" between frames
      if (is.na(prevMode)) {
        notes[[length(notes) + 1]] <- data.frame(x = xc + 0.10, y = y - 0.17, hjust = 0,
                                                 label = paste0("then, depending on ", p, ":"))
        arr[[length(arr) + 1]] <- data.frame(x = xc, y = y, yend = y - 0.34)
        y <- y - 0.34
      } else {
        notes[[length(notes) + 1]] <- data.frame(x = xc, y = y - 0.14, hjust = 0.5, label = "or")
        y <- y - 0.28
      }
      frames[[m]] <- data.frame(mode = m, top = y, bottom = NA_real_)
      y <- y - 0.26
    } else if (j > 1) {
      y <- y - if (rows$band[i[1]] == "off") 0.12 else 0.20
    }
    top <- y
    y <- y - 0.23
    for (k in i) {
      rows$y[k] <- y - rows$h[k] / 2
      y <- y - rows$h[k]
    }
    y <- y - 0.06
    bands[[b]] <- data.frame(bkey = b, band = rows$band[i[1]], mode = m, top = top, bottom = y,
                             dx = rows$dx[i[1]])
    nextMode <- if (j < length(keys)) rows$mode[rows$bkey == keys[j + 1]][1] else NA
    if (!is.na(m) && !identical(m, nextMode)) {
      y <- y - 0.07
      frames[[m]]$bottom <- y
    }
  }
  bands <- do.call(rbind, bands)
  frames <- if (length(frames)) do.call(rbind, frames)
  notes <- if (length(notes)) do.call(rbind, notes)
  unsched <- attr(rows, "unscheduled")

  for (j in seq_len(nrow(bands) - 1)) {
    if (bands$band[j + 1] == "off") next
    if (!identical(bands$mode[j], bands$mode[j + 1])) next
    arr[[length(arr) + 1]] <- data.frame(x = xc + bands$dx[j], y = bands$bottom[j],
                                         yend = bands$top[j + 1])
  }
  arr <- if (length(arr)) do.call(rbind, arr) else
    data.frame(x = numeric(), y = numeric(), yend = numeric())

  ## legend, flowed into rows that fit the width
  leg <- data.frame(
    key = c("on", "cond", "context"),
    text = c(if (is.null(p)) "runs with the default parameters" else
               paste0("runs with the default parameters (for that ", p, ")"),
             "runs only under some data or settings", "another module's event, shown for order")
  )
  present <- unique(rows$status[rows$kind == "event"])
  leg <- leg[leg$key %in% c(present, if (any(rows$kind == "context")) "context"), ]
  lx <- xs$band0 + 0.02
  ly <- y - 0.20
  leg$x <- leg$y <- NA_real_
  for (j in seq_len(nrow(leg))) {
    wj <- 0.34 + .ef_tw(leg$text[j], ptD, fam) + 0.18
    if (lx + wj > width && lx > xs$band0 + 0.05) {
      lx <- xs$band0 + 0.02
      ly <- ly - 0.19
    }
    leg$x[j] <- lx
    leg$y[j] <- ly
    lx <- lx + wj
  }
  y <- (if (nrow(leg)) min(leg$y) else y) - 0.13
  if (length(unsched)) {
    unschedTxt <- .ef_wrap(paste0("Defined but not scheduled by this module: ",
                                  paste(unsched, collapse = ", ")), width - 0.10, 6.5, fam)
    unschedY <- y
    y <- y - .ef_nlines(unschedTxt) * .ef_lineh(6.5) - 0.04
  }
  height <- -y + 0.04

  COL <- .ef_colors
  P <- .ef_polys()
  for (i in seq_len(NROW(frames))) {
    P$add(.ef_rrect(xs$band0, xs$band1, frames$bottom[i], frames$top[i], r = 0.09), "white",
          "grey55")
  }
  for (i in seq_len(nrow(bands))) {
    P$add(.ef_rrect(xs$band0 + bands$dx[i], xs$band1 - bands$dx[i], bands$bottom[i],
                    bands$top[i], r = 0.07), COL[[bands$band[i]]] %||% COL$start, NA)
  }
  boxStyle <- function(status, kind) {
    list(fill = if (kind == "context") COL$context else "white",
         col = if (kind == "context") COL$grey else COL$edge,
         lty = if (status == "cond") "22" else "solid")
  }
  for (i in seq_len(nrow(rows))) {
    r <- rows[i, ]
    if (r$kind == "off") next
    if (r$kind == "step") {
      P$add(.ef_rrect(xs$step0, xs$step1, r$y - r$h / 2 + 0.025, r$y + r$h / 2 - 0.025,
                      r = 0.035), "white", "#7fa9c9")
      next
    }
    if (!is.na(r$priority)) {
      P$add(.ef_rrect(xs$chip0 + r$dx, xs$chip1 + r$dx, r$y - 0.075, r$y + 0.075, r = 0.075),
            "white", "grey55")
    }
    s <- boxStyle(r$status, r$kind)
    P$add(.ef_rrect(xs$box0 + r$dx, xs$box1 + r$dx, r$y - 0.095, r$y + 0.095), s$fill, s$col,
          s$lty)
  }
  for (j in seq_len(nrow(leg))) {
    s <- if (leg$key[j] == "context") boxStyle("on", "context") else boxStyle(leg$key[j], "event")
    P$add(.ef_rrect(leg$x[j], leg$x[j] + 0.28, leg$y[j] - 0.06, leg$y[j] + 0.06, r = 0.03),
          s$fill, s$col, s$lty)
  }
  polys <- P$df()

  st <- rows[rows$kind == "step", ]
  stepArr <- if (nrow(st) > 1) {
    data.frame(x = xs$step0 + 0.08, y = utils::head(st$y - st$h / 2 + 0.025, -1),
               yend = utils::tail(st$y + st$h / 2 - 0.025, -1))
  }
  loops <- do.call(rbind, lapply(unique(rows$bkey[rows$band == "cycle"]), function(b) {
    cy <- rows[rows$bkey == b, ]
    lo <- min(cy$y) - if (nrow(cy) == 1) 0.05 else 0
    hi <- max(cy$y) + if (nrow(cy) == 1) 0.05 else 0
    data.frame(x = c(xs$chip0 - 0.02, xs$loop, xs$loop, xs$chip0 - 0.02) + cy$dx[1],
               y = c(lo, lo, hi, hi), id = b)
  }))

  txt <- rows[rows$kind %in% c("event", "context"), ]
  aes <- ggplot2::aes
  g <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = polys,
                          aes(x, y, group = id, fill = fill, colour = col, linetype = lty),
                          linewidth = 0.3) +
    ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() +
    ggplot2::scale_linetype_identity() +
    ggplot2::annotate("text", x = xs$band0, y = titleY, label = title, hjust = 0, vjust = 1,
                      size = .ef_pt(10), fontface = "bold", colour = COL$text,
                      lineheight = 0.95, family = fam) +
    ggplot2::annotate("text", x = xs$band0, y = subY, label = sub, hjust = 0, vjust = 1,
                      size = .ef_pt(ptD), colour = COL$detail, lineheight = 0.95,
                      family = fam) +
    ggplot2::geom_text(data = bands, aes(x = xs$band0 + dx + 0.12, y = top - 0.115,
                                         label = bandTitle[band]),
                       hjust = 0, size = .ef_pt(ptB), fontface = "bold", colour = COL$text,
                       family = fam) +
    ggplot2::geom_text(data = txt, aes(x = xc + dx, y = y, label = labelW,
                                       colour = ifelse(kind == "context", "grey40", COL$text),
                                       fontface = ifelse(kind == "context", "italic", "plain")),
                       size = .ef_pt(ptL), family = fam) +
    ggplot2::geom_text(data = rows[!is.na(rows$priority), ],
                       aes(x = (xs$chip0 + xs$chip1) / 2 + dx, y = y,
                           label = .ef_fmtNum(priority)),
                       size = .ef_pt(6.5), colour = "grey25", family = fam) +
    ggplot2::geom_text(data = rows[nzchar(rows$detailW) & ev, ],
                       aes(x = xs$det + dx, y = y, label = detailW), hjust = 0,
                       size = .ef_pt(ptD), lineheight = 0.95, colour = COL$detail,
                       family = fam) +
    ggplot2::geom_text(data = rows[off, ], aes(x = xs$chip0, y = y, label = labelW), hjust = 0,
                       size = .ef_pt(ptD), lineheight = 0.95, colour = "grey35", family = fam) +
    ggplot2::geom_text(data = rows[off, ], aes(x = xs$det, y = y, label = detailW), hjust = 0,
                       size = .ef_pt(ptD), lineheight = 0.95, colour = "grey35", family = fam) +
    ggplot2::geom_text(data = st, aes(x = xs$step0 + 0.18, y = y, label = label), hjust = 0,
                       size = .ef_pt(ptL), family = "mono", colour = COL$text) +
    ggplot2::geom_segment(data = arr, aes(x = x, xend = x, y = y, yend = yend),
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.055, "in"),
                                                 type = "closed"),
                          linewidth = 0.4, colour = "grey35") +
    ggplot2::geom_text(data = leg, aes(x = x + 0.34, y = y, label = text), hjust = 0,
                       size = .ef_pt(ptD), colour = COL$detail, family = fam) +
    ggplot2::coord_fixed(xlim = c(0, width), ylim = c(-height, 0), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0),
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA))
  if (!is.null(frames)) {
    lab <- paste0(p, " = \"", frames$mode, "\"",
                  ifelse(frames$mode %in% x$modeDefault, " (the default)", ""))
    g <- g + ggplot2::annotate("text", x = xs$band0 + 0.12, y = frames$top - 0.13, label = lab,
                               hjust = 0, size = .ef_pt(8), fontface = "bold", colour = COL$edge,
                               family = fam)
  }
  if (!is.null(notes)) {
    g <- g + ggplot2::geom_text(data = notes, aes(x = x, y = y, label = label, hjust = hjust),
                                size = .ef_pt(ptL), fontface = "italic", colour = "grey30",
                                family = fam)
  }
  if (!is.null(stepArr) && nrow(stepArr)) {
    g <- g + ggplot2::geom_segment(data = stepArr, aes(x = x, xend = x, y = y, yend = yend),
                                   arrow = ggplot2::arrow(length = ggplot2::unit(0.035, "in"),
                                                          type = "closed"),
                                   linewidth = 0.3, colour = "grey50")
  }
  if (!is.null(loops)) {
    g <- g + ggplot2::geom_path(data = loops, aes(x, y, group = id), linewidth = 0.4,
                                colour = "grey35",
                                arrow = ggplot2::arrow(length = ggplot2::unit(0.055, "in"),
                                                       type = "closed"))
  }
  if (length(unsched)) {
    g <- g + ggplot2::annotate("text", x = xs$band0 + 0.02, y = unschedY, label = unschedTxt,
                               hjust = 0, vjust = 1, size = .ef_pt(6.5), colour = "grey40",
                               lineheight = 0.95, family = fam)
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(file, g, width = width, height = height, units = "in", dpi = 300,
                  device = ragg::agg_png, bg = "white")
  invisible(list(file = file, height = height, rows = rows))
}

## ---- staging and failure notes ---------------------------------------------------------------

## Where to write a figure and how to refer to it. As SpaDES.docs::stageFigure() does, when a
## staging directory is set the figure is written under the knitr output directory and referred
## to relative to it.
.ef_figurePath <- function(path) {
  stageDir <- knitr::opts_knit$get("SpaDES.docs.stageDir")
  if (length(stageDir) == 1 && !is.na(stageDir) && nzchar(stageDir)) {
    ref <- file.path(stageDir, path)
    outDir <- knitr::opts_knit$get("output.dir")
    write <- file.path(if (length(outDir) == 1) outDir else ".", ref)
  } else {
    ref <- write <- path
  }
  dir.create(dirname(write), recursive = TRUE, showWarnings = FALSE)
  list(write = write, ref = ref)
}

.ef_note <- function(x) {
  msg <- if (inherits(x, "condition")) conditionMessage(x) else as.character(x)
  ## one line of emphasis: a parse error runs over several lines (with a caret
  ## line), and an asterisk in it would end the emphasis
  msg <- gsub("\\*", "\\\\*", gsub("\\s+", " ", trimws(msg)))
  knitr::asis_output(paste0("*Event flow not available: ", msg, "*"))
}

#' Include a module's event flow figure in a knitr document
#'
#' Draws the event flow of a module (see [moduleEventFlow()] and
#' [eventDiagram()]) from a chunk of a knitr (e.g., Rmarkdown or Quarto) document, and
#' includes the figure. When a document build stages its figures in a directory
#' (the knitr option `SpaDES.docs.stageDir`), the figure is written there under
#' the knitr output directory; otherwise it is written to `file`. Other modules'
#' events can be shown in grey, for their order relative to this module's, by
#' setting the knitr option `SpaDES.core.eventFlowContext` to their directories
#' (see the `context` argument of [eventDiagram()]).
#'
#' @param x A `moduleEventFlow` object, or the error raised when reading it
#'   failed, which gives a note in place of the figure.
#' @param file Character string. The figure's file name, relative to the staging
#'   directory when there is one.
#' @param width Numeric. The width of the figure in inches.
#'
#' @return The result of [knitr::include_graphics()], or, if `x` is an error or
#'   the module has no events, a note to print in the document.
#'
#' @export
#' @seealso [moduleEventFlow()], [eventDiagram()]
includeModuleEventFlow <- function(x, file, width = 4.65) {
  if (inherits(x, "condition")) return(.ef_note(x))
  if (length(x$read$branches) == 0) {
    return(knitr::asis_output("*This module has no events.*"))
  }
  fp <- .ef_figurePath(file)
  context <- knitr::opts_knit$get("SpaDES.core.eventFlowContext")
  res <- tryCatch(.ef_plotFlow(x, fp$write, width = width, context = context),
                  error = function(e) e)
  if (inherits(res, "error")) return(.ef_note(res))
  knitr::include_graphics(fp$ref, error = FALSE)
}
