utils::globalVariables("name")

## One figure per piece of a module's code (an event, a function init calls, or .inputObjects),
## drawn like a function in IDEF0 functional modelling: the objects it only reads enter from the
## left, the objects it updates (reads, then writes) pass straight through, the objects it creates
## leave on the right, and the parameters it reads enter from the top. The rows run diagonally
## (reads at top left, updates in the middle, creations at bottom right), so no row holds an input
## and an unrelated output side by side.

## `x`: a moduleEventFlow object; `figDir`: where the figures are written; `titles = FALSE` when
## the document's own heading names the code (an embedded figure).
## Returns the files, named by unit id, with the figure heights in inches as attribute "heights".
.ef_plotObjects <- function(x, figDir, width = 4.65, titles = TRUE) {
  for (pkg in c("ggplot2", "ragg", "systemfonts")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required to draw an event flow; please install it.")
    }
  }
  fam <- .ef_font()
  COL <- .ef_colors
  aes <- ggplot2::aes
  modName <- x$read$name
  dir.create(figDir, recursive = TRUE, showWarnings = FALSE)
  files <- character()
  heights <- numeric()
  used <- character()

  for (cl in .ef_codeUnits(x)) {
    upd <- sort(intersect(cl$reads, cl$writes))
    inOnly <- sort(setdiff(cl$reads, cl$writes))
    outOnly <- sort(setdiff(cl$writes, cl$reads))
    if (!length(c(upd, inOnly, outOnly))) next
    pars <- sort(cl$params)

    ## the event box takes the width the labels leave, down to a minimum (else smaller text)
    gap <- 0.30
    for (ptO in c(7, 6.5, 6)) {
      lw <- max(0.4, .ef_tw(c(inOnly, upd), ptO, fam))
      rw <- max(0.4, .ef_tw(c(upd, outOnly), ptO, fam))
      boxW <- width - 0.08 - lw - rw - 2 * gap
      if (boxW >= 0.85) break
    }
    boxW <- min(boxW, 1.6)
    x0 <- (width - (lw + rw + boxW + 2 * gap)) / 2
    xL <- x0 + lw # right edge of the left labels
    ev0 <- xL + gap
    ev1 <- ev0 + boxW
    xR <- ev1 + gap # left edge of the right labels
    rh <- .ef_lineh(ptO) + 0.05
    secH <- 0.17

    y <- -0.05
    title <- sub <- ""
    titleY <- subY <- y
    if (titles) {
      title <- .ef_wrap(paste0(modName, ": ", cl$title), width - 0.08, 9.5, fam)
      sub <- .ef_wrap(cl$sub, width - 0.08, 7, fam)
      y <- y - .ef_nlines(title) * .ef_lineh(9.5) - 0.02
      subY <- y
      y <- y - .ef_nlines(sub) * .ef_lineh(7) - 0.10
    }

    ## parameters enter from the top (dot-prefixed ones are SpaDES's plotting, saving and
    ## caching settings: listed last, grey)
    parTxt <- NULL
    if (length(pars)) {
      parW <- min(width - 0.08, max(boxW + 2 * gap, 2.4))
      dot <- startsWith(pars, ".")
      parTxt <- if (any(!dot)) .ef_wrap(paste(pars[!dot], collapse = "  "), parW, 6.5, fam) else ""
      dotTxt <- if (any(dot)) .ef_wrap(paste(pars[dot], collapse = "  "), parW, 6, fam) else ""
      parTop <- y
      y <- y - 0.14 - (if (nzchar(parTxt)) .ef_nlines(parTxt) * .ef_lineh(6.5) else 0)
      dotY <- y
      if (nzchar(dotTxt)) y <- y - .ef_nlines(dotTxt) * .ef_lineh(6) - 0.02
      parBottom <- y - 0.02
      y <- y - 0.20 # the arrow into the box
    }
    boxTop <- y
    y <- y - 0.24 # the code's name, inside the top of the box

    rows <- list()
    labs <- list()
    section <- function(label, side, names, kind) {
      labs[[length(labs) + 1L]] <<- data.frame(
        x = if (side == "left") x0 else xR, y = y - secH / 2, label = label
      )
      y <<- y - secH
      for (o in names) {
        rows[[length(rows) + 1L]] <<- data.frame(name = o, y = y - rh / 2, kind = kind)
        y <<- y - rh
      }
      y <<- y - 0.04
    }
    if (length(inOnly)) section("reads", "left", inOnly, "in")
    if (length(upd)) section("updates (reads, then writes)", "left", upd, "upd")
    if (length(outOnly)) section("writes", "right", outOnly, "out")
    rows <- do.call(rbind, rows)
    labs <- do.call(rbind, labs)
    boxBottom <- y + 0.02
    height <- -boxBottom + 0.08

    P <- .ef_polys()
    P$add(.ef_rrect(ev0, ev1, boxBottom, boxTop, r = 0.06), "white", COL$edge)
    polys <- P$df()
    inR <- rows[rows$kind == "in", ]
    upR <- rows[rows$kind == "upd", ]
    outR <- rows[rows$kind == "out", ]
    arrIn <- if (nrow(inR)) data.frame(x = xL + 0.05, xend = ev0, y = inR$y)
    arrOut <- if (nrow(outR)) data.frame(x = ev1, xend = xR - 0.05, y = outR$y)
    thru <- if (nrow(upR)) data.frame(x = xL + 0.05, xend = xR - 0.05, y = upR$y)
    txtL <- rbind(inR, upR)
    txtR <- rbind(upR, outR)
    ah <- ggplot2::arrow(length = ggplot2::unit(0.045, "in"), type = "closed")
    xm <- (ev0 + ev1) / 2

    g <- ggplot2::ggplot() +
      ggplot2::geom_polygon(data = polys, aes(x, y, group = id, fill = fill, colour = col),
                            linewidth = 0.35) +
      ggplot2::scale_fill_identity() +
      ggplot2::scale_colour_identity() +
      ggplot2::annotate("text", x = 0.04, y = titleY, label = title, hjust = 0, vjust = 1,
                        size = .ef_pt(9.5), fontface = "bold", colour = COL$text,
                        lineheight = 0.95, family = fam) +
      ggplot2::annotate("text", x = 0.04, y = subY, label = sub, hjust = 0, vjust = 1,
                        size = .ef_pt(7), colour = COL$detail, lineheight = 0.95, family = fam) +
      ggplot2::annotate("text", x = xm, y = boxTop - 0.12,
                        label = .ef_wrap(cl$title, boxW - 0.08, 8, fam), size = .ef_pt(8),
                        fontface = "bold", colour = COL$edge, lineheight = 0.9, family = fam) +
      ggplot2::geom_text(data = labs, aes(x = x, y = y, label = label), hjust = 0,
                         size = .ef_pt(6.5), fontface = "italic", colour = "grey40",
                         family = fam) +
      ggplot2::geom_text(data = txtL, aes(x = xL, y = y, label = name), hjust = 1,
                         size = .ef_pt(ptO), colour = COL$text, family = fam) +
      ggplot2::geom_text(data = txtR, aes(x = xR, y = y, label = name), hjust = 0,
                         size = .ef_pt(ptO), colour = COL$text, family = fam)
    seg <- function(g, d, lw, colour) {
      if (is.null(d)) return(g)
      g + ggplot2::geom_segment(data = d, aes(x = x, xend = xend, y = y, yend = y),
                                arrow = ah, linewidth = lw, colour = colour)
    }
    g <- seg(g, arrIn, 0.35, "#4a7fb0")
    g <- seg(g, thru, 0.6, "#3f8f5a")
    g <- seg(g, arrOut, 0.35, "#b07a3a")
    if (!is.null(parTxt)) {
      g <- g +
        ggplot2::annotate("text", x = xm, y = parTop, label = "parameters", vjust = 1,
                          size = .ef_pt(6.5), fontface = "italic", colour = "grey40",
                          family = fam) +
        ggplot2::annotate("text", x = xm, y = parTop - 0.13, label = parTxt, vjust = 1,
                          size = .ef_pt(6.5), colour = COL$text, lineheight = 0.95,
                          family = fam) +
        ggplot2::annotate("text", x = xm, y = dotY, label = dotTxt, vjust = 1,
                          size = .ef_pt(6), colour = "grey50", lineheight = 0.95,
                          family = fam) +
        ggplot2::annotate("segment", x = xm, xend = xm, y = parBottom, yend = boxTop,
                          arrow = ah, linewidth = 0.35, colour = "grey45")
    }
    g <- g +
      ggplot2::coord_fixed(xlim = c(0, width), ylim = c(-height, 0), expand = FALSE) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0),
                     plot.background = ggplot2::element_rect(fill = "white", colour = NA))

    ## ids that sanitize alike ("a b", "a_b") must not share a file
    base <- paste0(modName, "_io_", gsub("[^A-Za-z0-9_.-]", "_", cl$id))
    nm <- base
    k <- 1L
    while (nm %in% used) {
      k <- k + 1L
      nm <- paste0(base, "_", k)
    }
    used <- c(used, nm)
    f <- file.path(figDir, paste0(nm, ".png"))
    ggplot2::ggsave(f, g, width = width, height = height, units = "in", dpi = 300,
                    device = ragg::agg_png, bg = "white")
    files[cl$id] <- f
    heights[cl$id] <- height
  }
  structure(files, heights = heights)
}

#' Include a module's input/output figures in a knitr document
#'
#' For a chunk with `results = "asis"` of a knitr (e.g., Rmarkdown or Quarto) document: draws
#' one figure per piece of the module's code (see [objectDiagram()]) and writes, for each, a
#' heading, the line saying when it runs, and the figure. Figures of events the module no
#' longer has are deleted from `figDir`. When a document build stages its figures in a
#' directory (the knitr option `SpaDES.docs.stageDir`), the figures are written there under
#' the knitr output directory.
#'
#' @param x A `moduleEventFlow` object, or the error raised when reading it failed, which
#'   gives a note in place of the figures.
#' @param figDir Character string. The directory for the figures.
#' @param level Integer. The heading level of the heading above each figure.
#'
#' @return `NULL`, invisibly; the markdown is written to the document.
#'
#' @export
#' @seealso [objectDiagram()], [includeModuleEventFlow()]
moduleEventFigures <- function(x, figDir = "figures", level = 4) {
  if (inherits(x, "condition")) {
    cat(.ef_note(x), "\n")
    return(invisible(NULL))
  }
  fp <- .ef_figurePath(figDir)
  files <- tryCatch(.ef_plotObjects(x, fp$write, titles = FALSE), error = function(e) e)
  if (inherits(files, "error")) {
    cat(.ef_note(files), "\n")
    return(invisible(NULL))
  }
  ## figures of events that no longer exist; compared by file name, as list.files()
  ## expands a leading `~` that `files` keeps
  old <- list.files(fp$write, pattern = "\\.png$", full.names = TRUE)
  old <- old[startsWith(basename(old), paste0(x$read$name, "_io_"))]
  unlink(old[!basename(old) %in% basename(files)])

  h <- attr(files, "heights")
  latex <- knitr::is_latex_output()
  ## keep each heading with its figure: \Needspace (needspace) asks for the figure's height
  if (latex) knitr::knit_meta_add(list(rmarkdown::latex_dependency("needspace")))
  for (cl in .ef_codeUnits(x)) {
    if (!cl$id %in% names(files)) next
    if (latex) cat(sprintf("\n\n```{=latex}\n\\Needspace{%.2fin}\n```\n", h[[cl$id]] + 0.7))
    cat("\n\n", strrep("#", level), " ", gsub("_", "\\\\_", cl$title), " {-}\n\n", sep = "")
    cat(cl$sub, "\n\n", sep = "")
    cat("![](", file.path(fp$ref, basename(files[[cl$id]])), "){width=100%}\n\n", sep = "")
  }
  invisible(NULL)
}
