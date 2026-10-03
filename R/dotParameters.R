#' Dot parameters: the framework-aware module parameters
#'
#' Module parameters whose names start with a dot (`.plots`, `.useCache`, ...)
#' are generic run settings that SpaDES.core, or a package built on it,
#' recognises. They are ordinary parameters: a module declares one with
#' [defineParameter()] (the module template declares most of them) and reads it
#' with `P(sim)$.name`. SpaDES.core reads some of them itself, without any module
#' code, as listed below.
#'
#' Setting a value: `params = list(<module> = list(.plots = "png"))` sets it for
#' one module. `params = list(.globals = list(.plots = "png"))` sets it for every
#' module that defines a parameter of that name. The exceptions are `.plots`,
#' `.plotInterval`, `.saveInitialTime`, `.saveInterval`, `.useCache`,
#' `.useCacheArgs`, `.useCloud`, `.useParallel` and `.rep`, which SpaDES.core knows:
#' `.globals` sets those in every module, whether or not it defines them.
#' A value given for a module in `params` wins over `.globals`.
#' [spades()] arguments `.plots` and `.saveInitialTime` override the
#' parameters of the same name in all modules for that call.
#'
#' @section Parameters used by SpaDES.core:
#'
#' The "default" is the value when the module declares the parameter
#' with the module template, or, where stated, the value SpaDES.core fills in.
#'
#' \tabular{lll}{
#'   **Parameter** \tab **Default** \tab **Effect**\cr
#'   `.plots` \tab `"screen"` (template) \tab Types of output [Plots()] makes
#'     (`"screen"`, `"object"`, `"raw"`, or a file type such as `"png"`);
#'     `NA` for none. `spades(sim, .plots = )` sets it in all modules.
#'     Read by [Plots()] and [anyPlotting()].\cr
#'   `.plotInitialTime` \tab `start(sim)` (template); `NA` if the module does not
#'     set it \tab Time of the first plot event; the module schedules it with
#'     `scheduleEvent()`. `NA` switches screen plotting off in [Plots()]. [Plots()]
#'     only looks at it if the module declares it. `spades(sim, .plots = )` without
#'     `"screen"` sets it to `NA`.\cr
#'   `.plotInterval` \tab `NA` \tab Time between plot events, used by the module's
#'     own plot event.\cr
#'   `.saveInitialTime` \tab `NA` \tab Time of the first save event of the module.
#'     `spades(sim, .saveInitialTime = )` overrides it in all modules.
#'     See [saveFiles()].\cr
#'   `.saveInterval` \tab `NA` \tab Time between save events.\cr
#'   `.saveObjects` \tab none \tab Names of the `sim` objects that [saveFiles()]
#'     saves when called from the module's own save event.\cr
#'   `.savePath` \tab none \tab Not used by SpaDES.core code. It is exempt from
#'     [checkParams()]'s "parameter not used in module" message, as are
#'     `.saveObjects` and `.seed`.\cr
#'   `.seed` \tab `list()` (template) \tab Named list, one seed per event,
#'     e.g. `list(init = 123)`. [spades()] calls `set.seed()` for that event,
#'     then restores the random number stream afterwards.\cr
#'   `.useCache` \tab `FALSE` (template) \tab `TRUE` caches every event of the
#'     module including `.inputObjects`; a character vector caches only those
#'     events (e.g. `c("init", ".inputObjects")`); a time (`POSIXt`) caches
#'     all events and forces them to re-run if the cache entry is older. Turned
#'     off for all modules by `options(spades.useCache = "off")`. Not part of
#'     the cache key. See [clearCacheEventsOnly()] and
#'     `vignette("iii-cache", package = "SpaDES.core")`.\cr
#'   `.useCacheArgs` \tab none \tab Named list keyed by event name (or
#'     `".inputObjects"`) of extra arguments for [reproducible::Cache()] for that
#'     event, e.g. `cacheId`, `useCloud`, `cloudFolderID`. Entries made with
#'     `quote()`, e.g. `quote(P(sim)$.useCloud)`, are evaluated in the module's
#'     context. Not part of the cache key.\cr
#'   `.useCloud` \tab none \tab Not read by SpaDES.core directly. It is a known
#'     name so that `.globals` can set it, and it is not part of the cache key.
#'     Modules use it in `.useCacheArgs`, as above.\cr
#'   `.useParallel` \tab none \tab Number of cores, or whether to use parallel
#'     processing. A known name only: SpaDES.core never reads it; the module does.\cr
#'   `.showSimilar` \tab none (`reproducible.showSimilar` is used) \tab `TRUE`
#'     asks [reproducible::Cache()] to report how a cache miss differs from the
#'     nearest cache entry, for this module's cached events. Not part of the cache key.\cr
#'   `.progress` \tab text bar when interactive \tab Not a module parameter: a
#'     list with `type` and `interval` in `params`, set with
#'     `spades(sim, progress = )`, for the progress bar. Ignored by `.globals`.\cr
#' }
#'
#' The module template ([newModule()]) declares `.plots`, `.plotInitialTime`,
#' `.plotInterval`, `.saveInitialTime`, `.saveInterval`, `.studyAreaName`, `.seed`
#' and `.useCache`, and has a commented `.useCacheArgs`. [simInit()] sets the four
#' time parameters `.plotInitialTime`, `.plotInterval`, `.saveInitialTime`
#' and `.saveInterval` to `NA` in every module that has not set them,
#' whether or not the module declares them.
#'
#' @section Parameters set by SpaDES.project:
#'
#' These are not read by SpaDES.core. `SpaDES.project::setupProject()` puts them in
#' `params$.globals`, so each module receives them only if it declares a parameter
#' of the same name.
#'
#' \tabular{lll}{
#'   **Parameter** \tab **Default** \tab **Effect**\cr
#'   `.studyAreaName` \tab `NA` (template) \tab Human-readable name of the study
#'     area, e.g. from `reproducible::studyAreaName()`; `setupProject()` fills
#'     `.globals$.studyAreaName`. The module template declares it.\cr
#'   `.rep` \tab none \tab Replicate number. `setupProject()` sets
#'     `.globals$.rep` from a `.rep` entry, e.g. the `.rep` column of an
#'     experiment table. SpaDES.core knows it, so `.globals` sets it in every
#'     module; a module that uses the replicate declares a `.rep` parameter, e.g.
#'     `fireSense_spreadFit` and `fireSense_spreadPredict`.\cr
#' }
#'
#' @seealso [defineParameter()], [simInit()], [spades()], [Plots()], [saveFiles()],
#'   [newModule()]
#' @aliases SpaDES-dot-parameters
#' @name dotParameters
#' @rdname dotParameters
NULL
