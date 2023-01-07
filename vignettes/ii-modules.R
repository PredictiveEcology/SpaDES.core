## ----setup, include=FALSE-----------------------------------------------------
SuggestedPkgsNeeded <- c("DiagrammeR", "NLMR", "SpaDES.tools", "knitr")
hasSuggests <- all(sapply(SuggestedPkgsNeeded, require, character.only = TRUE, quietly = TRUE))
useSuggests <- !(tolower(Sys.getenv("_R_CHECK_DEPENDS_ONLY_")) == "true")

knitr::opts_chunk$set(eval = hasSuggests && useSuggests)

options("spades.moduleCodeChecks" = FALSE,
        "spades.useRequire" = FALSE)


## ----module-metadata, echo=FALSE, results="asis"------------------------------
cat(c(
  "```",
  "## sample module metadata for the default `caribouMovement` module",
  readLines(system.file("sampleModules", "caribouMovement", "caribouMovement.R", package = "SpaDES.core"))[10:45],
  "```"
  ), sep = "\n")

