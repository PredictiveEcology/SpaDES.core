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

  for (g in unique(findings$group)) {
    sub <- findings[findings$group == g, , drop = FALSE]
    cli::cli_text(cli::col_yellow(paste0("\u2022 ", g)))
    for (i in seq_len(nrow(sub))) {
      r <- sub[i, ]
      sevTag <- switch(r$severity,
                       error   = cli::col_red("[error]"),
                       warning = cli::col_magenta("[warn] "),
                       note    = cli::col_blue("[note] "),
                       info    = cli::col_silver("[info] "))
      loc <- if (!is.na(r$line)) {
        sprintf(" (%s:%d:%d)",
                basename(r$file %||% file %||% ""), r$line, r$col)
      } else ""
      cli::cli_text("  {sevTag} {r$message}{loc}")
      if (!is.na(r$suggestion)) {
        cli::cli_text(cli::col_silver("        \u21aa  {r$suggestion}"))
      }
    }
  }
  invisible(findings)
}
