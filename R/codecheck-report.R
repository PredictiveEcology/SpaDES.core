## Code-check reporting v2.
##
## Findings -> grouped tables printed via cli, plus a structured data.frame
## return value for tests / programmatic use. Matches the visual style of
## the rest of SpaDES.core (cli colours).

## Public: print findings as one or more tables, grouped by check class.
## Returns the findings invisibly.
.cc_report <- function(findings, module = NULL, file = NULL,
                       quiet = FALSE) {
  if (nrow(findings) == 0) {
    if (!quiet) {
      message(cli::col_magenta(paste0(module %||% "(module)",
                                      ": module code appears clean")))
    }
    return(invisible(findings))
  }

  ## group: which broad bucket does each rule belong to?
  groups <- c(
    out_declared_unused      = "outputObjects",
    out_used_undeclared      = "outputObjects",
    in_declared_unused       = "inputObjects",
    in_used_undeclared       = "inputObjects",
    in_no_default            = "inputObjects",
    param_declared_unused    = "parameters",
    param_used_undeclared    = "parameters",
    param_used_other_module  = "parameters",
    unresolved_accessor      = "unresolved",
    must_return_sim          = "module functions",
    must_assign_to_sim       = "module functions",
    module_named_object      = "module functions",
    conflicting_fn_unqualified = "globals",
    clashing_module_fn       = "module functions",
    codetools                = "codetools"
  )
  findings$group <- groups[findings$id] %||% "other"

  if (quiet) return(invisible(findings))

  ## header per module
  cli::cli_rule(left = cli::col_cyan(module %||% findings$module[1]))

  sevTag <- function(sev) switch(sev,
                                 error   = cli::col_red("[error]"),
                                 warning = cli::col_magenta("[warn] "),
                                 note    = cli::col_blue("[note] "),
                                 info    = cli::col_silver("[info] "))
  loc <- function(r) if (!is.na(r$line)) {
    sprintf(" (%s:%d:%d)", basename(r$file %||% file %||% ""), r$line, r$col)
  } else ""
  ## blank a finding's own name to a placeholder, so hits that differ only in
  ## that name (e.g. `scale`/`levels`, or inputs `cloudFolderID`/`ecoregionRst`)
  ## collapse under one header
  generic <- function(text, name) {
    if (is.na(text) || is.na(name) || !nzchar(name)) return(text)
    gsub(name, "<name>", text, fixed = TRUE)
  }

  for (g in unique(findings$group)) {
    sub <- findings[findings$group == g, , drop = FALSE]
    cli::cli_text(cli::col_yellow(paste0("\u2022 ", g)))
    gmsg <- vapply(seq_len(nrow(sub)),
                   function(i) generic(sub$message[i], sub$name[i]), character(1))
    gsug <- vapply(seq_len(nrow(sub)),
                   function(i) generic(sub$suggestion[i], sub$name[i]), character(1))
    key <- paste(sub$id, gmsg, gsug, sep = "\r")
    for (k in unique(key)) {
      idx <- which(key == k)
      r0 <- sub[idx[1], ]
      tag <- sevTag(r0$severity)
      if (length(idx) == 1L) {
        msgLoc <- paste0(r0$message, loc(r0))
        cli::cli_text("  {tag} {msgLoc}")
        if (!is.na(r0$suggestion)) {
          cli::cli_text(cli::col_silver("        \u21aa  {r0$suggestion}"))
        }
      } else {
        ## one header for the shared issue, then one line per hit
        header <- gmsg[idx[1]]
        cli::cli_text("  {tag} {header}")
        for (j in idx) {
          r <- sub[j, ]
          nm <- if (!is.na(r$name)) paste0("`", r$name, "`") else ""
          detail <- trimws(paste0(nm, loc(r)))
          cli::cli_text(cli::col_silver("         {detail}"))
        }
        if (!is.na(gsug[idx[1]])) {
          cli::cli_text(cli::col_silver("        \u21aa  {gsug[idx[1]]}"))
        }
      }
    }
  }
  invisible(findings)
}
