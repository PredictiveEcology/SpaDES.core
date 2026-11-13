## Which objects each piece of a module's code reads and writes, and the
## module's parameters it uses. Only objects in the module's metadata
## (expectsInput / createsOutput) are listed. The uses come from the code-check
## collectors, run over the parsed files already held by `.ef_readModule()`.

## The module's own functions reachable from `e` through calls to its own
## functions, in the order they are first found.
.ef_reachable <- function(e, mod) {
  seen <- character()
  todo <- .ef_moduleCalls(e, mod)
  while (length(todo)) {
    f <- todo[1]
    todo <- todo[-1]
    if (f %in% seen) next
    seen <- c(seen, f)
    todo <- c(todo, setdiff(.ef_moduleCalls(mod$fns[[f]]$body, mod), seen))
  }
  seen
}

## The top-level function an event's code sits in, found from the line range
## recorded for the branch; NULL if it has none (a branch that is not a block).
.ef_branchFn <- function(b, mod) {
  lines <- attr(b, "lines")
  if (is.null(lines) || anyNA(lines)) return(NULL)
  hit <- Filter(function(f) {
    identical(f$file, attr(b, "file")) && f$first <= lines[1] && f$last >= lines[2]
  }, mod$fns)
  if (!length(hit)) return(NULL)
  names(hit)[which.min(vapply(hit, function(f) f$last - f$first, numeric(1)))]
}

## Every sim access and parameter use in the module's files.
.ef_moduleUses <- function(mod) {
  parts <- lapply(mod$parsed, function(p) {
    rbind(.cc_collect_simAccess(p), .cc_collect_params(p, mod$name))
  })
  do.call(rbind, parts)
}

## The reads, writes and parameters of one piece of code: the uses inside the
## branch itself (`own`) and those in the module functions in `fns`.
.ef_unitIO <- function(uses, mod, branch = NULL, fns = character(), own = TRUE) {
  keep <- uses$fn %in% fns
  if (own && !is.null(branch)) {
    fn <- .ef_branchFn(branch, mod)
    lines <- attr(branch, "lines")
    if (!is.null(fn)) {
      keep <- keep | (uses$fn %in% fn & !is.na(uses$line) &
                        uses$line >= lines[1] & uses$line <= lines[2])
    }
  }
  u <- uses[keep, , drop = FALSE]
  nm <- function(kinds) unique(u$name[u$kind %in% kinds & !is.na(u$name)])
  p <- u[u$kind == "param" & !is.na(u$name) & (is.na(u$module) | u$module == mod$name), ]
  list(reads = nm("sim_get"), writes = nm("sim_assign"),
       params = intersect(unique(p$name), names(mod$meta$params)))
}

## When a one-off event runs: only at the start, only at the end, or at other
## set times.
.ef_band <- function(e) {
  if (isTRUE(e$recurring)) return("cycle")
  f <- e$firsts
  w <- ifelse(f$timeType == "end", "end",
              ifelse(f$timeType == "start" | (f$timeType == "at" & f$timeValue %in% 0),
                     "start", "times"))
  if (all(w == "start")) "start" else if (all(w == "end")) "end" else "times"
}

#' The pieces of a module's code in run order, with the objects and parameters
#' each uses
#'
#' `.inputObjects`, then `init` (one unit per module function it calls when it
#' calls two or more), then the events that run in some run mode: by run mode
#' (the default value first), then at the start, each time step, at other set
#' times and at the end, then by priority, as in the event-flow figure.
#'
#' Static reading cannot order reads and writes inside one piece of code, so a
#' read of an object the same code writes counts only if the object is a
#' declared input or earlier code wrote it. `.inputObjects` is shown with its
#' writes only: it reads its inputs just to see whether they were supplied.
#'
#' @param x A `moduleEventFlow`.
#' @return A list with one element per unit: `id`, `title`, `label`, `group`,
#'   `mode` (`NA` unless the event runs in only some run modes), `sub` (when
#'   it runs), `reads`, `writes` and `params`.
#' @noRd
.ef_codeUnits <- function(x) {
  mod <- x$read
  meta <- mod$meta
  known <- union(meta$inputs, meta$outputs)
  uses <- .ef_moduleUses(mod)
  tu <- if (length(x$timeunit) == 1 && !is.na(x$timeunit)) x$timeunit else "time step"
  grpName <- c(start = "at start", cycle = paste("each", tu), times = "set times", end = "at end")
  flows <- if (is.null(x$modes)) list(x$default) else x$modes
  units <- list()
  written <- character()
  add <- function(id, title, label, group, sub, io, mode = NA_character_) {
    w <- intersect(io$writes, known)
    ## .inputObjects reads its inputs only to check whether they were supplied
    r <- if (id == ".inputObjects") character() else intersect(io$reads, known)
    r <- r[!(r %in% w) | r %in% meta$inputs | r %in% written]
    written <<- union(written, w)
    units[[length(units) + 1L]] <<- list(
      id = id, title = title, label = label, group = group, mode = mode, sub = sub,
      reads = r, writes = w, params = io$params
    )
  }

  if (x$default$hasInputObjects) {
    f <- mod$fns[[".inputObjects"]]
    fns <- c(".inputObjects", .ef_reachable(f$body, mod))
    add(".inputObjects", ".inputObjects", ".inputObjects", "set-up",
        "during simInit: defaults for inputs not supplied",
        .ef_unitIO(uses, mod, fns = fns, own = FALSE))
  }

  init <- mod$branches[["init"]]
  calls <- x$default$initCalls
  n <- length(calls)
  if (n >= 2) {
    for (k in seq_len(n)) {
      f <- calls[k]
      add(paste0("init_", f), paste0(f, "()"), paste0("init: ", f, "()"), "set-up",
          sprintf("init event, step %d of %d", k, n),
          .ef_unitIO(uses, mod, fns = c(f, .ef_reachable(call(f), mod)), own = FALSE))
    }
    ## the init code outside the functions it calls
    own <- .ef_unitIO(uses, mod, branch = init)
    if (length(intersect(c(own$reads, own$writes), known))) {
      add("init_own", "init (own code)", "init (own code)", "set-up",
          "the init event's own code, outside the functions it calls", own)
    }
  } else if (!is.null(init)) {
    add("init", "init", "init", "set-up", "the init event",
        .ef_unitIO(uses, mod, branch = init, fns = .ef_reachable(init, mod)))
  }

  ## the events that run in some run mode, in the order of the event-flow figure
  rows <- list()
  for (m in seq_along(flows)) {
    fl <- flows[[m]]
    for (e in Filter(function(e) e$status %in% c("on", "cond"), fl$events)) {
      pr <- if (is.na(e$priority)) 5 else e$priority
      rows[[length(rows) + 1L]] <- list(
        event = e$event, mode = if (is.null(x$modes)) NA_character_ else names(flows)[m],
        band = .ef_band(e), priority = pr, sub = .ef_timingText(e, x$timeunit)
      )
    }
  }
  if (length(rows)) {
    key <- data.frame(
      mode = match(vapply(rows, `[[`, "", "mode"), names(flows)),
      band = match(vapply(rows, `[[`, "", "band"), names(grpName)),
      priority = vapply(rows, `[[`, 0, "priority"),
      event = vapply(rows, `[[`, "", "event"), stringsAsFactors = FALSE
    )
    key$mode[is.na(key$mode)] <- 1L
    rows <- rows[order(key$mode, key$band, key$priority, key$event)]
  }
  evs <- unique(vapply(rows, `[[`, "", "event"))
  for (ev in evs) {
    mine <- Filter(function(r) identical(r$event, ev), rows)
    modes <- vapply(mine, `[[`, "", "mode")
    ## an event that runs in every run mode belongs to none in particular
    mode <- if (is.null(x$modes) || length(modes) == length(flows)) {
      NA_character_
    } else {
      paste(modes, collapse = ", ")
    }
    first <- mine[[1]]
    sub <- first$sub
    if (!is.na(mode)) sub <- paste0(x$modeParam, " = \"", mode, "\": ", sub)
    add(ev, ev, ev, grpName[[first$band]], sub,
        .ef_unitIO(uses, mod, branch = mod$branches[[ev]], fns = .ef_reachable(mod$branches[[ev]], mod)),
        mode = mode)
  }
  units
}

#' The object index: one row per object traced to code
#'
#' The code that writes it (creates or updates it) and the code that only
#' reads it, each in run order. Attributes: `group` (one of "Inputs", "Inputs
#' the module also updates", "Outputs", per row) and `notes` (what is left
#' out).
#'
#' @param x A `moduleEventFlow`.
#' @return A `data.frame` with columns `Object`, `Written by` and `Read by`.
#' @noRd
.ef_objectIndex <- function(x) {
  meta <- x$read$meta
  units <- .ef_codeUnits(x)
  flows <- if (is.null(x$modes)) list(x$default) else x$modes
  onEv <- unique(unlist(lapply(flows, function(f) names(f$status)[f$status %in% c("on", "cond")])))
  offEv <- setdiff(names(x$default$status)[x$default$status != "unscheduled"], c(onEv, "init"))
  grpLevels <- c("Inputs", "Inputs the module also updates", "Outputs")
  ins <- meta$inputs
  outs <- meta$outputs
  objGroup <- c(
    stats::setNames(rep(grpLevels[1], length(setdiff(ins, outs))), setdiff(ins, outs)),
    stats::setNames(rep(grpLevels[2], length(intersect(ins, outs))), intersect(ins, outs)),
    stats::setNames(rep(grpLevels[3], length(setdiff(outs, ins))), setdiff(outs, ins))
  )
  touches <- vapply(units, function(u) length(c(u$reads, u$writes)) > 0, logical(1))
  emptyUnits <- vapply(units[!touches], `[[`, "", "label")
  units <- units[touches]
  label <- vapply(units, function(u) {
    if (u$id == ".inputObjects") {
      ".inputObjects (default)"
    } else if (!is.na(u$mode)) {
      paste0(u$label, " (", u$mode, ")")
    } else {
      u$label
    }
  }, character(1))
  writtenBy <- lapply(names(objGroup), function(o) {
    label[vapply(units, function(u) o %in% u$writes, logical(1))]
  })
  readBy <- lapply(names(objGroup), function(o) {
    label[vapply(units, function(u) o %in% u$reads && !(o %in% u$writes), logical(1))]
  })
  traced <- lengths(writtenBy) + lengths(readBy) > 0
  untraced <- names(objGroup)[!traced]
  ## order by group, then by the first unit (other than .inputObjects) to touch it
  firstUnit <- vapply(names(objGroup), function(o) {
    k <- which(vapply(units, function(u) o %in% c(u$reads, u$writes), logical(1)) &
                 vapply(units, `[[`, "", "id") != ".inputObjects")
    if (length(k)) k[1] else 0L
  }, integer(1))
  keep <- which(traced)
  keep <- keep[order(match(objGroup[keep], grpLevels), firstUnit[keep], tolower(names(objGroup)[keep]))]
  out <- data.frame(
    Object = names(objGroup)[keep],
    `Written by` = vapply(writtenBy[keep], paste, "", collapse = ", "),
    `Read by` = vapply(readBy[keep], paste, "", collapse = ", "),
    check.names = FALSE, row.names = NULL, stringsAsFactors = FALSE
  )
  attr(out, "group") <- unname(objGroup[keep])
  attr(out, "notes") <- c(
    if (length(offEv)) {
      paste0("Not shown, off with the default parameters: ", paste(offEv, collapse = ", "), ".")
    },
    if (length(emptyUnits)) {
      paste0("Not shown, touching no object in the metadata: ",
             paste(emptyUnits, collapse = ", "), ".")
    },
    if (length(untraced)) {
      paste0("Not traced to any code (e.g. reached indirectly): ",
             paste(untraced, collapse = ", "), ".")
    }
  )
  out
}

#' Show a module's object index as a table in a knitr document
#'
#' The objects a module reads and writes, with the code that writes each and the
#' code that only reads it (see [moduleObjects()] with `byEvent = TRUE`), as a
#' table for a knitr (e.g., Rmarkdown or Quarto) document. The table follows the
#' output format: a long table in LaTeX (object names are never wrapped or
#' hyphenated, and the table breaks across pages), an HTML table, or, for any
#' other format, a pipe table with the groups as bold rows. Notes on what is
#' left out come below the table. Needs the `knitr` package, and `kableExtra`
#' for LaTeX and HTML output.
#'
#' @param x A `moduleEventFlow` object (see [moduleEventFlow()]), or the error
#'   raised when reading it failed, which gives a note in place of the table.
#' @param caption Character string. The table caption, or `NULL` for none.
#' @param lead Character string. Text to put before the table, such as a sentence
#'   that refers to it, or `NULL` for none. It is left out when there is no table,
#'   so a cross-reference to the table never dangles.
#'
#' @return A table to print in the document (a `knitr_kable`), or, if `x` is an
#'   error, a note. With `lead`, the table is returned printed, after the text.
#'
#' @export
#' @rdname kableModuleObjects
kableModuleObjects <- function(x, caption = NULL, lead = NULL) {
  if (inherits(x, "condition")) return(.ef_note(x))
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is needed to make the table; please install it.")
  }
  ## never fail the knit: an error making the table gives a note in its place
  tryCatch(.ef_kableObjects(x, caption, lead), error = function(e) .ef_note(e))
}

## The table of kableModuleObjects() for the moduleEventFlow `x`.
.ef_kableObjects <- function(x, caption, lead) {
  tab <- .ef_objectIndex(x)
  grp <- attr(tab, "group")
  notes <- paste(attr(tab, "notes"), collapse = " ")
  fmt <- if (knitr::is_latex_output()) {
    "latex"
  } else if (knitr::is_html_output()) {
    "html"
  } else {
    "pipe"
  }
  if (!nrow(tab)) {
    ## never fail the knit: say so, and give the notes
    msg <- "*No objects in this module's metadata are read or written by its code.*"
    return(knitr::asis_output(paste(c(msg, "", notes, ""), collapse = "\n")))
  }
  if (fmt == "pipe") {
    parts <- lapply(unique(grp), function(g) {
      head <- stats::setNames(data.frame(paste0("**", g, "**"), "", ""), names(tab))
      rbind(head, tab[grp == g, ])
    })
    k <- knitr::kable(do.call(rbind, parts), format = "pipe", row.names = FALSE,
                      caption = caption)
    return(.ef_lead(knitr::asis_output(paste(c(k, "", notes, ""), collapse = "\n")), lead))
  }
  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Package 'kableExtra' is needed to make the table; please install it.")
  }
  k <- kableExtra::kbl(tab, format = fmt, booktabs = TRUE, longtable = fmt == "latex",
                       linesep = "", caption = caption, row.names = FALSE)
  k <- kableExtra::kable_styling(k, latex_options = "repeat_header", full_width = FALSE,
                                 position = "left", font_size = if (fmt == "latex") 8)
  if (fmt == "latex") {
    ## Object names are code: column 1 is left to LaTeX to size, so a name never
    ## wraps or hyphenates; columns 2 and 3 share the rest of the line.
    ## kableExtra substitutes the spec with a regex, so each backslash is written
    ## four times, and column_spec() takes one column at a time.
    k <- kableExtra::column_spec(k, 1, latex_column_spec = ">{\\\\raggedright\\\\arraybackslash}l")
    for (j in 2:3) {
      k <- kableExtra::column_spec(
        k, j, latex_column_spec = ">{\\\\raggedright\\\\arraybackslash}p{0.25\\\\linewidth}"
      )
    }
  }
  r <- rle(grp)
  k <- kableExtra::pack_rows(k, index = stats::setNames(r$lengths, r$values))
  if (nzchar(notes)) {
    k <- kableExtra::footnote(k, general = notes, general_title = "", threeparttable = TRUE)
  }
  .ef_lead(k, lead)
}

## Put `lead` before the table `k`. The table is printed here, as knitr would print
## it, so the result keeps what printing adds (kableExtra's HTML dependencies).
.ef_lead <- function(k, lead) {
  if (is.null(lead)) return(k)
  out <- knitr::knit_print(k)
  knitr::asis_output(paste0(lead, "\n\n", out), meta = attr(out, "knit_meta"))
}
