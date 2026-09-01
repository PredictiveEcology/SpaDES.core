#' Where an object's value comes from
#'
#' A module lists the objects it needs in `expectsInput`. This page describes
#' where the value for each of those objects actually comes from, and how a
#' module should ask.
#'
#' @details
#'
#' There are four possible sources. Three are always available; the fourth has
#' to be turned on.
#'
#' \enumerate{
#'   \item **The user**, in the `simInit()` call, using `objects =` or `inputs =`.
#'   \item **The module developer**, in the module's own `.inputObjects` function.
#'     This is the fallback: it should only supply a value when nothing else will.
#'   \item **Another module**, which lists the same object name in its
#'     `createsOutput` and runs earlier.
#'   \item **Another module's `init` event, run early**, if
#'     `options(spades.allowInitDuringSimInit = TRUE)`. Unlike the third source,
#'     the object is a real value that a `.inputObjects` function can use while
#'     working out its own default. See [spadesOptions()].
#' }
#'
#' @section Asking where a value will come from:
#'
#' Use [suppliedElsewhere()]. It answers the question "is anybody else going to
#' provide this?", so a module can skip work that would be wasted:
#'
#' ```
#' .inputObjects <- function(sim) {
#'   if (!suppliedElsewhere("flammability", sim)) {
#'     sim$flammability <- prepInputs(url = extractURL("flammability"), ...)
#'   }
#'   return(invisible(sim))
#' }
#' ```
#'
#' With `returnWhere = TRUE` it reports each source separately, as
#' `userSupplied`, `prevDotInputObjects` and `inFutureInit`.
#'
#' @section Why `is.null()` is not enough:
#'
#' It is tempting to write `if (is.null(sim$flammability))` instead. That is
#' wrong whenever another module is going to create the object, because that
#' module has not run yet when `.inputObjects` happens. `sim$flammability` really
#' is `NULL` at that moment, so the test passes and the module downloads or
#' computes a value that is overwritten a moment later. Nothing fails and no
#' warning appears; the only symptom is wasted work, which for a `sourceURL` is a
#' wasted download on every run. [suppliedElsewhere()] is what tells "nobody has
#' this" apart from "it is on its way".
#'
#' @section Deferring to the user in an init event:
#'
#' Nothing in `SpaDES` decides between a user-supplied object and a
#' module-produced one. A module gives way to the user only because its
#' `.inputObjects` was written to. An `init` event normally assigns the objects
#' it lists in `createsOutput` without checking, since creating them is its job,
#' so it gives way to nobody. If you want a module's output to step aside for a
#' value the user supplied, say so, using the same test one level up:
#'
#' ```
#' init = {
#'   if (!suppliedElsewhere("studyArea", sim)) {
#'     sim$studyArea <- ...   # only work it out if nobody supplied one
#'   }
#' }
#' ```
#'
#' @section Each object is resolved on its own:
#'
#' None of this is decided per module. Every object name is resolved
#' independently, so in one run a module can take `ageMap` from the user,
#' `biomassMap` from its own default, and `landCover` from another module. There
#' is no such thing as a module being "in default mode".
#'
#' This is what lets a project grow gradually. Begin with one module and no
#' arguments: every object falls back to the developer's defaults and it runs on
#' its own. Supply one object yourself, and only that one changes source. Add a
#' module that creates another one, and that one changes too. The other modules
#' are untouched, and no module's code has to change, because the same test that
#' used to supply a default now simply declines to.
#'
#' It is also why one module can be both a standalone tool and part of a large
#' collection of modules. In the first case every object falls back to a default;
#' in the second most come from other modules. The module cannot tell the
#' difference, and does not need to.
#'
#' @seealso [suppliedElsewhere()], [expectsInput()], [createsOutput()],
#'   [simInit()], [spadesOptions()], [objectSynonyms()]
#'
#' @name objectProvenance
#' @rdname objectProvenance
#' @author Eliot McIntire
NULL
