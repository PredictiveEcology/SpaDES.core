##  SpaDES.core/R/SpaDES-core-package.R by Alex M Chubaty and Eliot J B McIntire
##  Copyright (C) 2015-2025 His Majesty the King in Right of Canada,
##  as represented by the Minister of Natural Resources Canada
##

#' Categorized overview of the `SpaDES.core` package
#'
#' \if{html}{\figure{SpaDES.png}{options: width=100 alt="SpaDES logo" style="float: right;"}}
#' \if{latex}{\figure{SpaDES.png}{options: width=0.5in}}
#'
#' @description
#' `SpaDES.core` is a framework for building spatial discrete-event systems
#' from re-usable modules. A simulation is held in a single `simList` object,
#' modules schedule events that change the `simList`, and the events run in
#' time order until the simulation ends.
#'
#' Below is a categorized list of the functions you will use most often.
#' Each entry is a short, plain-language description; click through for full
#' details.
#'
#' Bug reports: <https://github.com/PredictiveEcology/SpaDES.core/issues>
#'
#' Module repository: <https://github.com/PredictiveEcology/SpaDES-modules>
#'
#' Wiki: <https://github.com/PredictiveEcology/SpaDES/wiki>
#'
#' @section 1 Initialize and run a simulation:
#'
#' \tabular{ll}{
#'   [simInit()] \tab Set up a new simulation.\cr
#'   [simInit2()] \tab Like [simInit()], but takes all arguments as a single list.\cr
#'   [spades()] \tab Run the simulation that `simInit()` set up.\cr
#'   [simInitAndSpades()] \tab Convenience: `simInit()` then `spades()` in one call.\cr
#'   [simInitAndSpades2()] \tab Like [simInitAndSpades()], but takes all arguments as a single list.\cr
#'   [restartSpades()] \tab Resume a simulation after an error or interruption.\cr
#'   [restartOrSimInitAndSpades()] \tab Try to restart from a saved state; otherwise start fresh.\cr
#' }
#'
#' @section 2 Events:
#'
#' Events are the building blocks of a simulation. Modules schedule events
#' that run at a given time and may schedule further events.
#'
#' \tabular{ll}{
#'   [scheduleEvent()] \tab Schedule an event to run at a given simulation time.\cr
#'   [scheduleConditionalEvent()] \tab Schedule an event that runs when a condition becomes `TRUE`.\cr
#'   [conditionalEvents()] \tab List the conditional events currently waiting.\cr
#'   [doEvent()] \tab Internal dispatcher; usually not called directly.\cr
#'   [defineEvent()] \tab Define a new event type inside a module.\cr
#' }
#'
#' @section 3 The `simList` object:
#'
#' The `simList` holds everything about a simulation: parameters, modules,
#' the event queue, the simulation times, and all the data objects modules
#' read and write. Its objects live in an environment, so updates happen
#' in place rather than by copying.
#'
#' \tabular{ll}{
#'   \link{simList-class} \tab The `simList` class definition.\cr
#'   [envir()] \tab The environment that holds the simulation's objects.\cr
#'   \code{\link[SpaDES.core:Copy]{Copy()}} \tab Make a true (deep) copy of a `simList`.\cr
#' }
#'
#' @section 4 `simList` accessors:
#'
#' Functions to read and write the various parts of a `simList`. All getters
#' have matching setters (e.g., `params(sim) <- ...`).
#'
#' \subsection{4.1 Parameters}{
#'   \tabular{ll}{
#'      [params()] \tab All simulation parameters, as a nested list.\cr
#'      [P()] \tab Get a parameter for the current module without naming it.\cr
#'      [globals()] \tab Simulation-wide (global) parameters.\cr
#'      [paramCheckOtherMods()] \tab Compare a parameter's value across modules.\cr
#'   }
#' }
#'
#' \subsection{4.2 Paths}{
#'   Where files are read from and written to.
#'
#'   \tabular{ll}{
#'      [paths()] \tab All paths used by the simulation.\cr
#'      [getPaths()], [setPaths()] \tab Get or set all paths in one call.\cr
#'      [cachePath()] \tab Where cached results are stored.\cr
#'      [modulePath()] \tab Where modules are loaded from.\cr
#'      [inputPath()] \tab Where input files are read from.\cr
#'      [outputPath()] \tab Where output files are written.\cr
#'      [dataPath()] \tab Where a module finds its `data/` folder.\cr
#'      [figurePath()] \tab Where figures are saved.\cr
#'      [logPath()] \tab Where simulation logs are written.\cr
#'      [scratchPath()] \tab Disposable working directory.\cr
#'      [rasterPath()], [terraPath()] \tab Temporary raster directories.\cr
#'   }
#' }
#'
#' \subsection{4.3 Simulation times}{
#'   \tabular{ll}{
#'      [time()] \tab The current simulation time.\cr
#'      [start()], [end()] \tab Start and end times.\cr
#'      [times()] \tab All times (current, start, end) at once.\cr
#'      [timeunit()] \tab The time unit of the simulation (or of a module).\cr
#'      [timeunits()] \tab The time units of all modules in the simulation.\cr
#'      [minTimeunit()], [maxTimeunit()] \tab The shortest and longest time unit in use.\cr
#'      [convertTimeunit()] \tab Convert a number between time units.\cr
#'      [elapsedTime()] \tab How much real time each module and event used.\cr
#'   }
#'
#'   Helpers for writing durations: [dsecond()], [dmin()], [dhour()], [dday()],
#'   [dweek()], [dmonth()], [dyear()].
#' }
#'
#' \subsection{4.4 Event queues}{
#'   \tabular{ll}{
#'      [events()] \tab The queue of scheduled events.\cr
#'      [current()] \tab The event being run right now.\cr
#'      [completed()] \tab Events that have already run.\cr
#'   }
#' }
#'
#' \subsection{4.5 Objects in the simulation}{
#'   \tabular{ll}{
#'      [objs()] \tab All data objects in the simulation environment.\cr
#'      [ls()], [objects()] \tab Names of those objects (base R methods).\cr
#'      [ls.str()] \tab Names plus a short summary of each object's structure.\cr
#'   }
#' }
#'
#' \subsection{4.6 Modules in the simulation}{
#'   \tabular{ll}{
#'      [modules()] \tab The modules loaded in this simulation.\cr
#'      [depends()] \tab Each module's declared dependencies.\cr
#'      [packages()] \tab R packages required by the modules.\cr
#'   }
#' }
#'
#' @section 5 Building a module:
#'
#' \subsection{5.1 Module metadata}{
#'   Every module declares its name, parameters, inputs and outputs using
#'   these constructors, called inside [defineModule()].
#'
#'   \tabular{ll}{
#'      [defineModule()] \tab Top-level metadata constructor.\cr
#'      [defineParameter()] \tab Declare one parameter with a default.\cr
#'      [expectsInput()] \tab Declare one input the module reads from `sim`.\cr
#'      [createsOutput()] \tab Declare one output the module writes to `sim`.\cr
#'      [bindrows()] \tab Combine the rows above into the metadata table.\cr
#'   }
#' }
#'
#' \subsection{5.2 Reading module metadata back}{
#'   \tabular{ll}{
#'      [moduleMetadata()] \tab The whole metadata block.\cr
#'      [moduleParams()], [moduleInputs()], [moduleOutputs()] \tab Just one slice.\cr
#'      [moduleObjects()] \tab All objects a module reads or writes.\cr
#'      [moduleVersion()] \tab The declared version.\cr
#'      [moduleDefaults] \tab Default values used when metadata is missing.\cr
#'      [parameters()], [inputObjects()], [outputObjects()] \tab Read these on a `simList`.\cr
#'      [citation()], [documentation()], [reqdPkgs()] \tab Other metadata accessors.\cr
#'   }
#' }
#'
#' \subsection{5.3 Helpers inside event functions}{
#'   \tabular{ll}{
#'      [currentModule()] \tab The name of the running module.\cr
#'      [suppliedElsewhere()] \tab `TRUE` if another module already provides an object.\cr
#'      [checkObject()] \tab Confirm a required object exists in `sim`.\cr
#'      [findObjects()] \tab Look up objects across modules.\cr
#'   }
#' }
#'
#' \subsection{5.4 Authoring and packaging modules}{
#'   \tabular{ll}{
#'      [newModule()] \tab Create a new module from a template.\cr
#'      [newModuleCode()], [newModuleDocumentation()], [newModuleTests()] \tab
#'         Generate individual parts of a module.\cr
#'      [copyModule()] \tab Copy a module to a new location and (optionally) rename it.\cr
#'      [openModules()] \tab Open one or more module files in the editor.\cr
#'      [zipModule()] \tab Zip up a module for distribution.\cr
#'      [convertToPackage()] \tab Turn a module into an installable R package.\cr
#'      [newProject()] \tab Create a new project that contains modules.\cr
#'   }
#' }
#'
#' @section 6 Module dependencies and diagrams:
#'
#' `simInit()` works out which modules depend on which. These functions
#' inspect that graph.
#'
#' \tabular{ll}{
#'   [depsEdgeList()] \tab Edge list of object dependencies between modules.\cr
#'   [depsGraph()] \tab The same, as an `igraph`.\cr
#'   [moduleDiagram()] \tab A simplified picture of which module feeds which.\cr
#'   [objectDiagram()] \tab A detailed picture, by individual object.\cr
#'   [eventDiagram()] \tab Gantt chart of events that ran in a completed simulation.\cr
#' }
#'
#' @section 7 Caching:
#'
#' Caching makes a workflow reproducible and skips re-running steps that
#' have not changed. Caching uses the `reproducible` package; SpaDES adds
#' shortcuts for caching at the `spades`, module, event, and function level.
#'
#' \tabular{ll}{
#'   [reproducible::Cache()] \tab Cache any function call.\cr
#'   [reproducible::showCache()] \tab Inspect what is in the cache.\cr
#'   [reproducible::clearCache()] \tab Remove cached entries.\cr
#'   [reproducible::keepCache()] \tab Keep only the entries you name.\cr
#' }
#'
#' Inside a module's metadata you can set the parameter `.useCache` to `TRUE`
#' (cache the whole module), a character vector (cache only those events),
#' or use `.useCacheArgs` to pin per-event arguments to [reproducible::Cache()].
#' See the caching vignette: `vignette("iii-cache", package = "SpaDES.core")`.
#'
#' @section 8 Plotting:
#'
#' Plotting goes through `quickPlot::Plot()`, which is optimised for fast
#' redraws within a simulation. SpaDES.core adds the [Plots()] wrapper that
#' lets a module choose at runtime whether to render to screen, a PNG file,
#' or no output at all.
#'
#' \tabular{ll}{
#'   [Plots()] \tab Module-friendly wrapper for any plotting function.\cr
#'   [anyPlotting()] \tab `TRUE` if the current `.plots` setting will produce any output.\cr
#' }
#'
#' @section 9 Code checking:
#'
#' When `simInit()` runs, it can statically check each module's code against
#' the metadata: every `sim$x` and parameter access is checked to be sure it
#' matches the declared inputs, outputs, and parameters.
#'
#' \tabular{ll}{
#'   [codeCheckModule()] \tab Run the checks on a module on disk (no `simInit()` needed).\cr
#' }
#'
#' Controlled by these options:
#' \tabular{ll}{
#'   `spades.moduleCodeChecks` \tab Turn the in-`simInit()` checks on (`TRUE`) or off (`FALSE`).\cr
#'   `spades.codeCheckEngine` \tab `"v1"` (default) or `"v2"` (new, structured output).\cr
#' }
#'
#' @section 10 Persistence and recovery:
#'
#' \tabular{ll}{
#'   [saveSimList()] \tab Save a `simList` to disk.\cr
#'   [loadSimList()] \tab Load one back.\cr
#'   [zipSimList()], [unzipSimList()] \tab Save/load a `simList` together with its files.\cr
#'   [saveState()] \tab Snapshot the state during a run.\cr
#'   [restartR()] \tab Restart R (for example, to free memory) and continue.\cr
#'   [saveFiles()], [loadFiles()] \tab Save or load files described by `outputs()` / `inputs()`.\cr
#'   [checksums()] \tab Verify (or write) checksums for a module's data files.\cr
#' }
#'
#' @section 11 Memory monitoring:
#'
#' \tabular{ll}{
#'   [memoryUse()] \tab Memory used by each event in a finished simulation.\cr
#'   [memoryUseThisSession()] \tab Memory used by the current R session.\cr
#' }
#'
#' See `vignette("iv-advanced", package = "SpaDES.core")` for how to enable
#' the background sampler with `options(spades.memoryUseInterval = ...)`.
#'
#' @section 12 Module repository and downloads:
#'
#' \tabular{ll}{
#'   [getModuleVersion()] \tab Get the latest version number on the repository.\cr
#'   [getSampleModules()] \tab Copy the bundled sample modules somewhere usable.\cr
#' }
#'
#' @section 13 Sample modules included with the package:
#'
#' Three small modules in `inst/sampleModules/`, plus a module group that
#' loads all three:
#'
#' \tabular{ll}{
#'   `randomLandscapes` \tab Generates a `SpatRaster` stack of random landscape layers.\cr
#'   `fireSpread` \tab Simulates fire ignition and spread on those layers.\cr
#'   `caribouMovement` \tab Agent-based caribou movement (correlated random walk).\cr
#'   `SpaDES_sampleModules` \tab Module group that loads all three.\cr
#' }
#'
#' Get a copy with `getSampleModules(tempdir())`.
#'
#' @section 14 Package options:
#'
#' Many behaviours are configurable through R options (e.g., default paths,
#' how much output to print, code-checking, parallelism). See [spadesOptions()]
#' for the full list with defaults and descriptions; the defaults are also
#' returned as a named list by calling `spadesOptions()`.
#'
#' @seealso [spadesOptions()]
#'
#' @import igraph
#' @import methods
#' @rdname SpaDES.core-package
"_PACKAGE"
