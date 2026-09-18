## moduleRmdToVignette() moves a module's documentation .Rmd into the package
## rendition's vignettes/, so pkgdown can publish it as an article
## (PredictiveEcology/SpaDES-modules#40).

## The .Rmd newModule() writes, plus the bibliography file its header points at.
## The template resolves the module by relative path, so there is nothing here to
## stand in for the directory the module was created in.
writeTemplateRmd <- function(name, dir) {
  md <- file.path(dir, name)
  dir.create(file.path(md, "citations"), recursive = TRUE, showWarnings = FALSE)
  tmpl <- readLines(file.path(.pkgEnv[["templatePath"]], "module.Rmd.template"))
  writeLines(whisker::whisker.render(tmpl, list(author = "A B", date = "01 January 2026",
                                                name = name, name2 = gsub("_", "-", name))),
             file.path(md, paste0(name, ".Rmd")))
  writeLines(character(0), file.path(md, "citations", paste0("references_", name, ".bib")))
  md
}

test_that("moduleRmdToVignette() repoints the module path in R code only, and keeps the bookdown format", {
  d <- file.path(tempdir(), paste0("rmdVig", .rndstr(len = 4)))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  md <- writeTemplateRmd("modV", d)

  out <- moduleRmdToVignette("modV", md)
  expect_identical(normalizePath(out), normalizePath(file.path(md, "vignettes", "modV.Rmd")))
  vig <- readLines(out)

  ## R code: the inline `r ` expressions in the header and authors line, and every
  ## table chunk (inputs, parameters, packages, outputs), resolve the module from
  ## vignettes/
  expect_false(any(grepl("path = '..')", vig, fixed = TRUE)))
  expect_length(grep("path = '../..')", vig, fixed = TRUE), 2)
  expect_length(grep('<- SpaDES.core::module[A-Za-z]+\\("modV", "\\.\\./\\.\\."\\)', vig), 4)
  expect_false(any(grepl('<- SpaDES.core::module[A-Za-z]+\\("modV", "\\.\\."\\)', vig)))
  ## the template's prose shows the same call as advice to the reader; it is not
  ## code, and keeps what it says
  expect_true(any(grepl('`downloadData("modV", "..")` may be sufficient', vig, fixed = TRUE)))

  ## header: pkgdown keeps bookdown's format, so \@ref(tab:...) still resolves
  delim <- which(vig == "---")
  header <- vig[(delim[1] + 1):(delim[2] - 1)]
  expect_true(all(c("pkgdown:", "  as_is: true") %in% header))
  expect_true(any(grepl("bookdown::html_document2", header, fixed = TRUE)))

  ## the bibliography travels with it, and the module's own .Rmd is unchanged
  expect_true(file.exists(file.path(md, "vignettes", "citations", "references_modV.bib")))
  expect_length(grep("path = '..')", readLines(file.path(md, "modV.Rmd")), fixed = TRUE), 2)
})

test_that("moduleRmdToVignette() leaves an existing pkgdown entry alone", {
  d <- file.path(tempdir(), paste0("rmdVig", .rndstr(len = 4)))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  md <- file.path(d, "modW")
  dir.create(md, recursive = TRUE)
  writeLines(c("---", "title: modW", "pkgdown:", "  as_is: false", "---", "", "Text."),
             file.path(md, "modW.Rmd"))

  vig <- readLines(moduleRmdToVignette("modW", md))
  expect_identical(sum(grepl("^pkgdown:", vig)), 1L)
  expect_true("  as_is: false" %in% vig)
})

test_that("moduleRmdToVignette() stops without an .Rmd, or without a YAML header", {
  d <- file.path(tempdir(), paste0("rmdVig", .rndstr(len = 4)))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  md <- file.path(d, "modX")
  dir.create(md, recursive = TRUE)

  expect_error(moduleRmdToVignette("modX", md), "does not exist")
  writeLines(c("# modX", "", "---", "not a header", "---"), file.path(md, "modX.Rmd"))
  expect_error(moduleRmdToVignette("modX", md), "no YAML header")
})

test_that("the vignette renders from vignettes/, with its tables and cross-references", {
  skip_on_cran()
  skip_if_not_installed("curl")  # skip_if_offline() errors, not skips, without it
  skip_if_offline()  # the template's setup chunk downloads a badge
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("bookdown")
  skip_if_not_installed("kableExtra")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc is not available")

  d <- file.path(tempdir(), paste0("rmdVig", .rndstr(len = 4)))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  dir.create(d, recursive = TRUE)
  withr::local_options(spades.moduleDocument = FALSE)
  suppressMessages(newModule("modR", d, open = FALSE, unitTests = FALSE))
  md <- writeTemplateRmd("modR", d)

  out <- moduleRmdToVignette("modR", md)
  html <- suppressWarnings(suppressMessages(
    rmarkdown::render(out, output_dir = file.path(d, "html"), quiet = TRUE, envir = new.env())))
  page <- paste(readLines(html, warn = FALSE), collapse = "\n")

  ## the metadata tables are only there if the module path resolved
  expect_match(page, "<table")
  expect_match(page, 'href="#tab:moduleInputs-modR"', fixed = TRUE)
  expect_false(grepl("@ref(", page, fixed = TRUE))
})
