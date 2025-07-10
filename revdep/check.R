# pak::pak("r-lib/revdepcheck")
library("revdepcheck")

options(repos = c(
  PE = "https://predictiveecology.r-universe.dev",

  ## note this is counter to the "Canonical CRAN.r-project.org" for CRAN packages,
  ## but revdep/ directory is .Rbuildignored and this is used for revdep checks only.
  CRAN = paste0("https://", "cloud.", "r-project.", "org")
))

revdep_reset()
revdep_check(num_workers = getOption("Ncpus", 2), quiet = FALSE, timeout = 30*60) ## 30 mins
revdep_report_cran() ## update cran-comments with this output

### email maintainers of revdep packages (need to edit: `revdep/email.yml`)
# revdep_email(type = "broken") ## will send via gmail
# revdep_email(type = "failed") ## will send via gmail
