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
#' module, with this rule: a dot parameter is *universal* if SpaDES.core itself
#' acts on it for every module, without module code. `.globals` sets universal
#' parameters in every module, declared or not. All others are *declared only*:
#' `.globals` sets them only in modules that declare them. A value given for a
#' module in `params` wins over `.globals`.
#' [spades()] arguments `.plots` and `.saveInitialTime` override the
#' parameters of the same name in all modules for that call.
#'
#' @section Parameters used by SpaDES.core:
#'
#' The "default" is the value when the module declares the parameter
#' with the module template, or, where stated, the value SpaDES.core fills in.
#'
#' \tabular{llll}{
#'   **Parameter** \tab **Tier** \tab **Default** \tab **Effect**\cr
#'   `.neverCache` \tab declared only \tab none \tab Events (names, may include
#'     `".inputObjects"`) that are never cached, because they run for their side effects.
#'     Wins over `.useCache` in every form, including `TRUE` from `.globals`; a
#'     message says so once per module and event. Declared only: the module owns it.
#'     Not part of the cache key.\cr
#'   `.plots` \tab universal \tab `"screen"` (template) \tab Types of output [Plots()] makes
#'     (`"screen"`, `"object"`, `"raw"`, or a file type such as `"png"`);
#'     `NA` for none. `spades(sim, .plots = )` sets it in all modules.
#'     Read by [Plots()] and [anyPlotting()]. It is part of the
#'     cache key: an event loaded from the cache does not run, so it makes no new
#'     plots and a changed `.plots` must re-run the event.\cr
#'   `.plotInitialTime` \tab declared only \tab `start(sim)` (template); `NA` if the module does not
#'     set it \tab Time of the first plot event; the module schedules it with
#'     `scheduleEvent()`. `NA` switches screen plotting off in [Plots()]. [Plots()]
#'     only looks at it if the module declares it. `spades(sim, .plots = )` without
#'     `"screen"` sets it to `NA`.\cr
#'   `.plotInterval` \tab declared only \tab `NA` \tab Time between plot events, used by the module's
#'     own plot event.\cr
#'   `.saveInitialTime` \tab declared only \tab `NA` \tab Time of the first save event of the module.
#'     `spades(sim, .saveInitialTime = )` overrides it in all modules.
#'     See [saveFiles()].\cr
#'   `.saveInterval` \tab declared only \tab `NA` \tab Time between save events.\cr
#'   `.saveObjects` \tab declared only \tab none \tab Names of the `sim` objects that [saveFiles()]
#'     saves when called from the module's own save event.\cr
#'   `.savePath` \tab declared only \tab none \tab Not used by SpaDES.core code. It is exempt from
#'     [checkParams()]'s "parameter not used in module" message, as are
#'     `.saveObjects` and `.seed`.\cr
#'   `.seed` \tab universal \tab `list()` (template) \tab Named list, one seed per event,
#'     e.g. `list(init = 123)`. [spades()] calls `set.seed()` for that event,
#'     then restores the random number stream afterwards. Part of the cache key,
#'     since it changes results.\cr
#'   `.useCache` \tab universal \tab `FALSE` (template) \tab `TRUE` caches every event of the
#'     module including `.inputObjects`; a character vector caches only those
#'     events (e.g. `c("init", ".inputObjects")`); a time (`POSIXt`) caches
#'     all events and forces them to re-run if the cache entry is older. Turned
#'     off for all modules by `options(spades.useCache = "off")`. Not part of
#'     the cache key. See [clearCacheEventsOnly()] and
#'     `vignette("iii-cache", package = "SpaDES.core")`.\cr
#'   `.useCacheArgs` \tab universal \tab none \tab Named list keyed by event name (or
#'     `".inputObjects"`) of extra arguments for [reproducible::Cache()] for that
#'     event, e.g. `cacheId`, `useCloud`, `cloudFolderID`. Entries made with
#'     `quote()`, e.g. `quote(P(sim)$.useCloud)`, are evaluated in the module's
#'     context. Not part of the cache key.\cr
#'   `.useCloud` \tab declared only \tab none \tab Not read by SpaDES.core directly, so declared only.
#'     It is not part of the cache key. Modules use it in `.useCacheArgs`, as above.\cr
#'   `.useParallel` \tab declared only \tab none \tab Number of cores, or whether to use parallel
#'     processing. SpaDES.core never reads it; the module does.\cr
#'   `.showSimilar` \tab universal \tab none (`reproducible.showSimilar` is used) \tab `TRUE`
#'     asks [reproducible::Cache()] to report how a cache miss differs from the
#'     nearest cache entry, for this module's cached events. Read for every module
#'     (`.runEvent`, `.inputObjects`). Not part of the cache key.\cr
#'   `.progress` \tab not a module parameter \tab text bar when interactive \tab Not a module parameter: a
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
#' `params$.globals`. Both are declared only: a module receives them only if it
#' declares a parameter of the same name.
#'
#' \tabular{llll}{
#'   **Parameter** \tab **Tier** \tab **Default** \tab **Effect**\cr
#'   `.studyAreaName` \tab declared only \tab `NA` (template) \tab Human-readable name of the study
#'     area, e.g. from `reproducible::studyAreaName()`; `setupProject()` fills
#'     `.globals$.studyAreaName`. The module template declares it.\cr
#'   `.rep` \tab declared only \tab none \tab Replicate number. `setupProject()` sets
#'     `.globals$.rep` from a `.rep` entry, e.g. the `.rep` column of an
#'     experiment table. A module that uses the replicate declares a `.rep`
#'     parameter, e.g.
#'     `fireSense_spreadFit` and `fireSense_spreadPredict`.\cr
#' }
#'
#' @seealso [defineParameter()], [simInit()], [spades()], [Plots()], [saveFiles()],
#'   [newModule()]
#' @aliases SpaDES-dot-parameters
#' @name dotParameters
#' @rdname dotParameters
NULL
