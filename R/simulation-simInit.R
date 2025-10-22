utils::globalVariables(c(".", "Package", "hasVersionSpec"))

#' Initialize a new simulation
#'
#' Create a new simulation object, the `sim` object (a `simList`).
#' This object is implemented using an `environment` where all objects and functions are placed.
#' Since environments in `R` are pass by reference, "putting" objects in
#' the `sim` object does no actual copy.
#' The `simList` also stores all parameters, and other important simulation
#' information, such as times, paths, modules, and module load order.
#' See more details below.
#'
#' \subsection{Calling this `simInit` function does the following:}{
#'   \tabular{lll}{
#'   **What** \tab **Details** \tab **Argument(s) to use** \cr
#'   fills `simList` slots \tab places the arguments `times`,
#'     `params`, `modules`, `paths` into equivalently named
#'     `simList` slots \tab `times`,
#'     `params`, `modules`, `paths`\cr
#'   sources all module files \tab places all function definitions in the
#'     `simList`, specifically, into a sub-environment of the main
#'     `simList` environment: e.g., `sim$<moduleName>$function1`
#'     (see section on **Scoping**) \tab `modules` \cr
#'   copies objects \tab from the global environment to the
#'     `simList` environment \tab `objects` \cr
#'   loads objects \tab from disk into the `simList` \tab `inputs` \cr
#'   schedule object loading/copying \tab Objects can be loaded into the
#'     `simList` at any time during a simulation  \tab `inputs` \cr
#'   schedule object saving \tab Objects can be saved to disk at any arbitrary
#'     time during the simulation. If specified here, this will be in addition
#'     to any saving due code inside a module (i.e., a module may manually
#'     run `write.table(...)` \tab `outputs` \cr
#'   schedules "init" events \tab from all modules (see [events()])
#'        \tab automatic  \cr
#'   assesses module dependencies \tab via the inputs and outputs identified in their
#'     metadata. This gives the order of the `.inputObjects` and `init`
#'     events. This can be overridden by `loadOrder`. \tab automatic \cr
#'   determines time unit \tab takes time units of modules
#'       and how they fit together \tab `times` or automatic \cr
#'   runs `.inputObjects` functions \tab from every module
#'     *in the module order as determined above* \tab automatic \cr
#'   }
#' }
#'
#' `params` can only contain updates to any parameters that are defined in
#' the metadata of modules. Take the example of a module named, `Fire`, which
#' has a parameter named `.plotInitialTime`. In the metadata of that module,
#' it says `TRUE`. Here we can override that default with:
#' `list(Fire=list(.plotInitialTime=NA))`, effectively turning off plotting.
#' Since this is a list of lists, one can override the module defaults for multiple
#' parameters from multiple modules all at once, with say:
#' `list(Fire = list(.plotInitialTime = NA, .plotInterval = 2),
#'            caribouModule = list(N = 1000))`.
#'
#' The `params` list can contain a list (named `.globals`) of named objects
#' e.g., `.globals = list(climateURL = "https:\\something.com")` entry. Any and every
#' module that has a parameter with that name (in this case `climateURL`) will be
#' overridden with this value as passed.
#'
#' `params` can be used to set the seed for a specific event in a module. This
#' is done using the normal `params` argument, specifying `.seed` as a list
#' where the elements are a numeric for the seed and the name is the event. Since
#' parameters must be specific to a module, this creates a module and event specific
#' seed e.g., `params = list(moduleName = list(.seed = list(init = 123)))` will
#' set the `init` event of module named `moduleName` to 123. The RN stream
#' will be reset to its state prior to the `set.seed` call after the event.
#'
#' We implement a discrete event simulation in a more modular fashion so it is
#' easier to add modules to the simulation. We use S4 classes and methods,
#' and fast lists to manage the event queue.
#'
#' `paths` specifies the location of the module source files,
#' the data input files, and the saving output files. If no paths are specified
#' the defaults are as follows:
#'
#' \itemize{
#'   \item `cachePath`: `getOption("reproducible.cachePath")`;
#'
#'   \item `inputPath`: `getOption("spades.inputPath")`;
#'
#'   \item `modulePath`: `getOption("spades.modulePath")`;
#'
#'   \item `outputPath`: `getOption("spades.outputPath")`.
#' }
#'
#' @section Parsing and Checking Code:
#'
#' The `simInit` function will attempt to find usage of `sim$xxx` or `sim[['xxx']]`
#'  on either side of the assignment (`<-`) operator.
#' It will compare these to the module metadata, specifically `inputObjects` for cases where
#' objects or "gotten" from the `simList` and `outputObjects` for cases where objects are
#' assigned to the `simList.`
#'
#' It will also attempt to find potential, common function name conflicts with things like
#' `scale` and `stack` (both in \pkg{base} and \pkg{raster}), and
#' `Plot` (in \pkg{quickPlot} and some modules).
#'
#' *This code checking is young and may get false positives and false negatives,
#' i.e., miss things*.
#' It also takes computational time, which may be undesirable in operational code.
#' To turn off checking (i.e., if there are too many false positives and negatives), set
#' `options(spades.moduleCodeChecks = FALSE)`.
#'
#' @section Caching:
#'
#' Using caching with `SpaDES` is vital when building re-usable and reproducible content.
#' Please see the vignette dedicated to this topic.
#'
#' @note
#' Since the objects in the `simList` are passed-by-reference, it is useful
#' to create a copy of the initialized `simList` object prior to running
#' the simulation (e.g., `mySimOut <- spades(Copy(mySim))`).
#' This ensures you retain access to the original objects, which would otherwise
#' be overwritten/modified during the simulation.
#'
#' @note
#' The user can opt to run a simpler `simInit` call without inputs, outputs, and times.
#' These can be added later with the accessor methods (See example).
#' These are not required for initializing the simulation via `simInit`.
#' All of `modules`, `paths`, `params`, and `objects` are needed
#' for successful initialization.
#'
#' @param times A named list of numeric simulation start and end times
#'        (e.g., `times = list(start = 0.0, end = 10.0, timeunit = "year")`),
#'        with the final optional element, `timeunit`, overriding the default
#'        time unit used in the simulation which is the "smallest time unit" across all
#'        modules. See examples.
#'
#' @param params A list of lists of the form `list(moduleName=list(param1=value, param2=value))`.
#' See details.
#'
#' @param modules A named list of character strings specifying the names of modules to be loaded
#' for the simulation.
#' Note: the module name should correspond to the R source file from which the module is loaded.
#' Example: a module named "caribou" will be sourced form the file \file{caribou.R},
#' located at the specified `modulePath(simList)` (see below).
#'
#' @param objects (optional) A vector of object names (naming objects
#'                that are in the calling environment of
#'                the `simInit`, which is often the
#'                `.GlobalEnv` unless used programmatically.
#'                NOTE: this mechanism will
#'                fail if object name is in a package dependency), or
#'                a named list of data objects to be
#'                passed into the `simList` (more reliable).
#'                These objects will be accessible
#'                from the `simList` as a normal list, e.g,. `mySim$obj`.
#'
#' @param paths  An optional named list with up to 4 named elements,
#' `modulePath`, `inputPath`, `outputPath`, and `cachePath`.
#' See details. NOTE: Experimental feature now allows for multiple `modulePath`s
#' to be specified in a character vector. The modules will be searched for sequentially
#' in the first `modulePath`, then if it doesn't find it, in the second etc.
#'
#' @param inputs A `data.frame`. Can specify from 1 to 6
#' columns with following column names: `objectName` (character, required),
#' `file` (character), `fun` (character), `package` (character),
#' `interval` (numeric), `loadTime` (numeric).
#' See [inputs()] and vignette("ii-modules") section about inputs.
#'
#' @param outputs A `data.frame`. Can specify from 1 to 5
#' columns with following column names: `objectName` (character, required),
#' `file` (character), `fun` (character), `package` (character),
#' `saveTime` (numeric) and `eventPriority` (numeric). If
#' `eventPriority` is not set, it defaults to `.last()`. If `eventPriority`
#' is set to a low value, e.g., 0, 1, 2 and `saveTime` is `start(sim)`,
#' it should give "initial conditions".
#'
#' See [outputs()] and
#' `vignette("ii-modules")` section about outputs.
#'
#' @param loadOrder  An optional character vector of module names specifying the order in
#'                   which to load the modules. If not specified, the module
#'                   load order will be determined automatically.
#'
#' @param notOlderThan A time, as in from `Sys.time()`. This is passed into
#'                     the `Cache` function that wraps `.inputObjects`.
#'                     If the module uses the `.useCache` parameter and it is
#'                     set to `TRUE` or `".inputObjects"`,
#'                     then the `.inputObjects` will be cached.
#'                     Setting `notOlderThan = Sys.time()` will cause the
#'                     cached versions of `.inputObjects` to be refreshed,
#'                     i.e., rerun.
#' @param ... An alternative way to pass `objects`, i.e., they can just be named
#'   arguments rather than in a `objects = list(...)`. It can also be any
#'   `options` that begins with `spades`, `reproducible` or `Require`, i.e.,
#'   those identified in `spadesOptions()`,
#'   `reproducibleOptions()` or `RequireOptions()`.
#'   These will be assigned to the equivalent option *during* the `simInit` and `spades`
#'   calls only, i.e., they will revert after the `simInit` or `spades` calls
#'   are complete. NOTE: these are not passed to the `simList` per se, i.e., they are
#'   not be available in the `simList` during either
#'   the `simInit` or `spades` calls via `sim$xxx`, though they will be returned to the `simList`
#'   at the end of each of these calls (so that the next call to e.g., `spades` can
#'   see them). For convenience, these can be supplied without their package prefix,
#'   e.g., `lowMemory` can be specified instead of `spades.lowMemory`. In cases that
#'   share option name (`reproducible.verbose` and `Require.verbose` both exist),
#'   passing `verbose = FALSE` will set both. Obviously this may cause unexpected
#'   problems if a module is also expecting a value.
#'
#' @return A `simList` simulation object, pre-initialized from values
#' specified in the arguments supplied.
#'
#' @seealso [spades()], [defineModule()] to get help on metadata elements,
#' [times()], [params()], [objs()], [paths()],
#' [modules()], [inputs()], [outputs()]
#'
#' @author Alex Chubaty and Eliot McIntire
#' @export
#' @include environment.R
#' @include module-dependencies-class.R
#' @include simList-class.R
#' @include simulation-parseModule.R
#' @include priority.R
#' @importFrom data.table setDTthreads
#' @importFrom reproducible basename2
#' @importFrom Require Require trimVersionNumber modifyList2
#' @importFrom utils compareVersion sessionInfo
#' @rdname simInit
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
#'             San Francisco, CA: No Starch Press, Inc..
#'             Retrieved from <https://nostarch.com/artofr.htm>
#'
#' @examples
#' \donttest{ # Tests take several seconds
#' if (requireNamespace("SpaDES.tools", quietly = TRUE) &&
#' requireNamespace("NLMR", quietly = TRUE)) {
#' opts <- options("spades.moduleCodeChecks" = FALSE, "spades.useRequire" = FALSE)
#' if (!interactive()) opts <- append(opts, options("spades.plots" = NA,
#'                                                  "spades.debug" = FALSE))
#'
#' mySim <- simInit(
#'  times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'  params = list(
#'    .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'  ),
#'  modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'  paths = list(modulePath = getSampleModules(tempdir()))
#' )
#' spades(mySim) # shows plotting
#'
#' # Change more parameters, removing plotting
#' mySim <- simInit(
#'  times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'  params = list(
#'    .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#'    fireSpread = list(.plotInitialTime = NA)
#'  ),
#'  modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'  paths = list(modulePath = getSampleModules(tempdir()))
#' )
#' outSim <- spades(mySim)
#'
#' # A little more complicated with inputs and outputs
#' mapPath <- system.file("maps", package = "quickPlot")
#' mySim <- simInit(
#'   times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'   params = list(
#'     .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'   ),
#'   modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'   paths = list(modulePath = getSampleModules(tempdir()),
#'                outputPath = tempdir()),
#'   inputs = data.frame(
#'     files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
#'     functions = "rast",
#'     package = "terra",
#'     loadTime = 1,
#'     stringsAsFactors = FALSE),
#'   outputs = data.frame(
#'     expand.grid(objectName = c("caribou","landscape"),
#'     saveTime = 1:2,
#'     stringsAsFactors = FALSE)))
#'
#'  # Use accessors for inputs, outputs
#'  mySim2 <- simInit(
#'    times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#'      randomLandscapes = list(nx = 10, ny = 10)
#'      ),
#'    paths = list(
#'      modulePath = getSampleModules(tempdir()),
#'      outputPath = tempdir()
#'    )
#'  )
#'
#'  # add by accessor is equivalent
#'  inputs(mySim2) <- data.frame(
#'      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
#'      functions = "rast",
#'      package = "terra",
#'      loadTime = 1,
#'      stringsAsFactors = FALSE)
#'  outputs(mySim2) <- data.frame(
#'      expand.grid(objectName = c("caribou", "landscape"),
#'      saveTime = 1:2,
#'      stringsAsFactors = FALSE))
#'  all.equal(mySim, mySim2) # TRUE
#'
#'  # Use accessors for times -- does not work as desired because times are
#'  #   adjusted to the input timeunit during simInit
#'  mySim2 <- simInit(
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'    ),
#'    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'    paths = list(modulePath = getSampleModules(tempdir()),
#'                 outputPath = tempdir()),
#'    inputs = data.frame(
#'      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
#'      functions = "rast",
#'      package = "terra",
#'      loadTime = 1,
#'      stringsAsFactors = FALSE),
#'    outputs = data.frame(
#'      expand.grid(objectName = c("caribou","landscape"),
#'      saveTime = 1:2,
#'      eventPriority = c(0,10), # eventPriority 0 may give "initial" conditions
#'      stringsAsFactors = FALSE))
#'  )
#'
#'  # add times by accessor fails all.equal test because "year" was not
#'  #   declared during module loading, so month became the default
#'  times(mySim2) <- list(current = 0, start = 0.0, end = 2.0, timeunit = "year")
#'  all.equal(mySim, mySim2) # fails because time units are all different, so
#'                           # several parameters that have time units in
#'                           # "months" because they were loaded that way
#'  params(mySim)$fireSpread$.plotInitialTime
#'  params(mySim2)$fireSpread$.plotInitialTime
#'  events(mySim) # load event is at time 1 year
#'  events(mySim2) # load event is at time 1 month, reported in years because of
#'                 #   update to times above
#' options(opts)
#'
#' }
#' }
#'
setGeneric(
  "simInit",
  function(times, params, modules, objects, paths, inputs, outputs, loadOrder,
           notOlderThan = NULL, ...) {
    standardGeneric("simInit")
})

#' @rdname simInit
setMethod(
  "simInit",
  signature(
    times = "list",
    params = "list",
    modules = "list",
    objects = "list",
    paths = "list",
    inputs = "data.frame",
    outputs = "data.frame",
    loadOrder = "character"
  ),
  definition = function(times,
                        params,
                        modules,
                        objects,
                        paths,
                        inputs,
                        outputs,
                        loadOrder,
                        notOlderThan, ...) {

    dots <- list(...)
    if (is.null(dots[[._txtStartClockTime]]))
      assign(._txtStartClockTime, Sys.time())
      # ._startClockTime <- Sys.time()
    else
      assign(._txtStartClockTime, dots[[._txtStartClockTime]])
      # ._startClockTime <- dots[[._txtStartClockTime]]
    dots[[._txtStartClockTime]] <- NULL
    dotNames <- setdiff(...names(), ._txtStartClockTime)


    # loggingMessage helpers
    ._simNestingLocal <- simNestingSetup(...) # checks in call stack for "sim"
    assign(._txtSimNesting, ._simNestingLocal)

    # create  <- List object for the simulation
    sim <- new("simList")
    sim@.xData[[._txtStartClockTime]] <- get(._txtStartClockTime, inherits = FALSE)
    sim$._simInitElapsedTime <- 0

    # loggingMessage helpers
    sim[[._txtSimNesting]] <- ._simNestingLocal

    opt <- options("encoding" = "UTF-8")
    if (isTRUE(getOption("spades.allowSequentialCaching"))) {
      opt <- append(opt, options(reproducible.showSimilarDepth = 6))
    }

    on.exit({
      options(opt)
      sim <- elapsedTimeInSimInit(get(._txtStartClockTime, inherits = FALSE), sim)
      ._startClockTimeLocal <- Sys.time()
      sim@.xData[[._txtStartClockTime]] <- NULL
      dt <- difftime(._startClockTimeLocal, ._startClockTimeLocal - sim$._simInitElapsedTime)
      message("Elapsed time for simInit: ", format(dt, format = "auto"))
    }, add = TRUE)

    paths <- lapply(paths, function(p) {
      checkPath(p, create = TRUE)
    })

    if (length(dotNames)) {
      objects <- append(objects, dots)

      # set the options; then set them back on exit
      optsFromDots <- dealWithOptions(objects = objects, sim = sim, dotNames = dotNames)
      if (!is.null(optsFromDots$optsPrev)) {
        # remove from `objects` as these should not be there
        objects <- objects[optsFromDots$keepObjNames]
        on.exit({
          # reset options in session
          options(optsFromDots$optsPrev)
          # put them back in simList for reassessment during spades
          if (exists("sim", inherits = FALSE))
            list2env(optsFromDots$optionsAsProvided, envir = envir(sim))
        }, add = TRUE)
      }
    }

    objNames <- names(objects)
    if (length(objNames) != length(objects)) {
      stop(
        "Please pass a named list or character vector of object names whose values",
        "can be found in the parent frame of the simInit() call"
      )
    }

    # user modules
    modulesLoaded <- list()

    # If this is being run inside a module, then it needs to know
    sc <- sys.calls()
    simPrev <- .grepSysCalls(sc, ".runEvent")

    # if (length(simPrev) > 0) {
    #   sim[["._simPrevs"]] <- append(sys.frames()[tail(simPrev, 1)], sim[["._simPrevs"]])
    # } else {
    #   sim[["._simPrevs"]] <- list()
    # }

    # add project/session info -- use list to allow subsequent addition (e.g., git, spatial libs)
    if (getOption("spades.sessionInfo", TRUE))
      sim@.xData[["._sessionInfo"]] <- list(
        sessionInfo = utils::sessionInfo(),
        timestamp = Sys.time()
      )

    # Make a temporary place to store parsed module files
    sim@.xData[[".parsedFiles"]] <- new.env(parent = emptyenv())
    on.exit(rm(".parsedFiles", envir = sim@.xData), add = TRUE)

    # paths
    oldGetPaths <- .paths()
    do.call(setPaths, paths)
    on.exit({
      do.call(setPaths, append(list(silent = TRUE), oldGetPaths))
    }, add = TRUE)
    paths(sim) <- paths #paths accessor does important stuff

    names(modules) <- unlist(modules)
    adjustModuleNameSpacing(modules) # changes options("spades.messagingNumCharsModule")

    # Check that modules exist in paths$modulePath
    modulePaths <- .checkModuleDirsAndFiles(modules = modules, modulePath = sim@paths$modulePath)

    # identify childModules, recursively
    childModules <- .identifyChildModules(sim = sim, modules = modulePaths)
    modules <- as.list(unique(unlist(childModules))) # flat list of all modules
    names(modules) <- unlist(modules)
    modules <- lapply(modules, basename2)

    moduleNames <- names(modules)
    names(moduleNames) <- moduleNames

    # Test that all child module files and dirs exist
    childModDirsExist <- lapply(moduleNames, dir.exists)
    childModMainFiles <- file.path(moduleNames, paste0(basename2(moduleNames), ".R"))
    names(childModMainFiles) <- basename2(moduleNames)
    childModFilesExist <- file.exists(childModMainFiles)
    if (any(!childModFilesExist)) {
      stop(childModMainFiles[!childModFilesExist],
           " does not exist; please create one or diagnose. ",
           "  Some possible causes:\n  ",
           "- git submodule not initiated?\n  ",
           "- the module was not created using 'newModule(...)' so is missing key files.")
    }

    modules <- modules[!sapply(modules, is.null)] |>
      lapply(`attributes<-`, list(parsed = FALSE))

    # parameters for core modules
    dotParamsReal <- list(".saveInterval",
                          ".saveInitialTime",
                          ".plotInterval",
                          ".plotInitialTime")
    dotParamsChar <- list(".savePath", ".saveObjects", ".seed")
    dotParams <- append(dotParamsChar, dotParamsReal)

    sim@modules <- modules  ## will be updated below

    mBase <- basename2(unlist(modules))

    # Load packages
    if (getOption("spades.loadReqdPkgs", TRUE)) {
      reqdPkgs <- packages(modules = sim@modules,
                           filenames = file.path(names(sim@modules), paste0(mBase, ".R")),
                           paths = paths(sim)$modulePath,
                           envir = sim@.xData[[".parsedFiles"]])
      loadPkgs(reqdPkgs) # does unlist internally
    }
    # From here, capture messaging and prepend it
    withCallingHandlers({
      simDTthreads <- getOption("spades.DTthreads", 1L)
      message("Using setDTthreads(", simDTthreads, "). To change: 'options(spades.DTthreads = X)'.")
      origDTthreads <- setDTthreads(simDTthreads)
      on.exit(setDTthreads(origDTthreads), add = TRUE)

      allTimeUnits <- FALSE

      ## run this only once, at the highest level of the hierarchy, so before the parse tree happens
      parentChildGraph <- as.data.frame(buildParentChildGraph(sim, modules(sim), childModules = childModules))

      timeunits <- findSmallestTU(sim, modulePaths, childModules)

      if (!is.null(times$unit)) {
        message(
          paste0(
            "times contains \'unit\', rather than \'timeunit\'. ",
            "Using \"", times$unit, "\" as timeunit"
          )
        )
        times$timeunit <- times$unit
        times$unit <- NULL
      }

      ## Get correct time unit now that modules are loaded
      timeunit(sim) <- if (!is.null(times$timeunit)) {
        #sim@simtimes[["timeunit"]] <- if (!is.null(times$timeunit)) {
        times$timeunit
      } else {
        minTimeunit(timeunits)
      }

      timestep <- inSeconds(sim@simtimes[["timeunit"]], sim@.xData)
      times(sim) <- list(
        current = times$start * timestep,
        start = times$start * timestep,
        end = times$end * timestep,
        timeunit = sim@simtimes[["timeunit"]]
      )

      ## START OF simInit overrides for inputs, then objects
      if (NROW(inputs)) {
        inputs <- .fillInputRows(inputs, startTime = start(sim))
      }

      ## used to prevent .inputObjects from loading if object is passed in by user.
      sim$.userSuppliedObjNames <- c(objNames, inputs$objectName)

      ## for now, assign only some core & global params
      sim@params$.globals <- params$.globals

      # core modules
      core <- .pkgEnv$.coreModules
      # remove the restartR module if it is not used. This is easier than adding it because
      #   the simInit is not run again during restarts, so it won't hit this again. That
      #   is problematic for restartR situation, but not for "normal" situation.
      if (is.null(params$.restartR$.restartRInterval) &&
          getOption("spades.restartRInterval", 0) == 0) {
        core <- setdiff(core, "restartR")
        # .pkgEnv$.coreModules <- core
      } else {
        restartDir <- checkAndSetRestartDir(sim = sim)
      }

      ## add core module name to the loaded list (loaded with the package)
      modulesLoaded <- append(modulesLoaded, core)

      # ## source module metadata and code files
      # lapply(names(modules(sim)), function(m) moduleVersion(m, sim = sim,
      #                                                envir = sim@.xData[[".parsedFiles"]]))

      ## do multi-pass if there are parent modules; first for parents, then for children
      all_parsed <- FALSE
      while (!all_parsed) {
        sim <- .parseModule(sim,
                            as.list(sim@modules),
                            userSuppliedObjNames = sim$.userSuppliedObjNames,
                            envir = sim@.xData[[".parsedFiles"]],
                            notOlderThan = notOlderThan, params = params,
                            objects = objects, paths = paths)
        if (length(.unparsed(sim@modules)) == 0) {
          all_parsed <- TRUE
        }
      }

      # push globals onto parameters within each module
      if (length(sim@params$.globals)) {
        sim <- updateParamsFromGlobals(sim, dontUseGlobals = params)
      }

      ## add name to depends
      if (!is.null(names(sim@depends@dependencies))) {
        names(sim@depends@dependencies) <- sim@depends@dependencies |>
          lapply(function(x) x@name) |>
          unlist()
      }

      ## load core modules
      for (c in core) {
        # schedule each module's init event:
        #.refreshEventQueues()
        sim <- scheduleEvent(sim, start(sim, unit = sim@simtimes[["timeunit"]]),
                             c, "init", .first() - 1)
      }

      ## assign user-specified non-global params, while
      ## keeping defaults for params not specified by user
      omit <- c(which(core == "load"), which(core == "save"))
      pnames <- unique(c(paste0(".", core[-omit]), names(sim@params)))

      if (is.null(params$.progress) || any(is.na(params$.progress))) {
        params$.progress <- .pkgEnv$.progressEmpty
      }

      tmp <- list()
      lapply(pnames, function(x) {
        tmp[[x]] <<- suppressWarnings(modifyList2(sim@params[[x]], params[[x]]))
      })
      sim@params <- tmp

      ## check user-supplied load order & init dependencies
      sim@.xData$._ranInitDuringSimInit <- character()
      missingInLoadOrder <- setdiff(sim@modules, loadOrder)

      if (!all(length(loadOrder),
               all(sim@modules %in% loadOrder),
               all(loadOrder %in% sim@modules))) {
        # This is the call to Init with allowInitDuringSimInit
        sim <- resolveDepsRunInitIfPoss(sim, modules, paths, params, objects, inputs, outputs)
        if (length(sim@completed))
          sim@.xData$._ranInitDuringSimInit <- setdiff(completed(sim)$module, .coreModules())
        loadOrderPoss <- unlist(unname(sim@modules))
        if (length(missingInLoadOrder)) {
          if (any(match(loadOrder, loadOrderPoss) != seq_along(loadOrder))) {
            warning("loadOrder argument is used, but does not have all the modules in it; ",
                    "setting modules in loadOrder first, with remaining modules place after... ",
                    "this may be incorrect behaviour and should likely be changed")
            modsAfter <- setdiff(loadOrderPoss, loadOrder)
            loadOrderPoss <- c(loadOrder, modsAfter)
          }
        }
        loadOrder <- loadOrderPoss
      }

      mBase <- basename2(unlist(sim@modules))
      loadOrderBase <- basename2(loadOrder)
      names(loadOrder) <- names(unlist(sim@modules))[na.omit(match(mBase, loadOrderBase))]
      loadOrder[] <- loadOrderBase
      loadOrderNames <- names(loadOrder)

      ## This is a quick override so that .runInputObjects has access to these synonynms
      if (!is.null(objects$objectSynonyms)) {
        sim[[objSynName]] <- objects$objectSynonyms
        sim <- .checkObjectSynonyms(sim)
      }

      # Make local activeBindings to mod
      makeSimListActiveBindings(sim)
      # lapply(as.character(sim@modules), function(mod) {
      #   makeModActiveBinding(sim = sim, mod = mod)
      # })
      #
      # lapply(sim@modules, function(mod) {
      #   makeParActiveBinding(sim = sim, mod = mod)
      # })

      ## load user-defined modules
      # browser(expr = exists("._simInit_4"))
      if (!(all(unlist(lapply(debug, identical, FALSE))))) {
        # .pkgEnv[[".spadesDebugFirst"]] <- TRUE
        sim[["._spadesDebugWidth"]] <- c(9, 10, 9, 13)
      }

      for (m in loadOrder) {
        mFullPath <- loadOrderNames[match(m, loadOrder)]

        needInitAndInputObjects <- TRUE
        if (length(sim@.xData$._ranInitDuringSimInit)) {
          if (m %in% sim@.xData$._ranInitDuringSimInit)
            needInitAndInputObjects <- FALSE
        }

        ## run .inputObjects() for each module
        if (needInitAndInputObjects)
          if (isTRUE(getOption("spades.dotInputObjects", TRUE))) {
            if (is.character(getOption("spades.covr", FALSE))) {
              mod <- getOption("spades.covr")
              tf <- tempfile()
              if (is.null(notOlderThan)) notOlderThan <- "NULL"
              cat(file = tf, paste0("simOut <- .runModuleInputObjects(sim, '", m,
                                    "', notOlderThan = ", notOlderThan, ")"))
              # cat(file = tf, paste('spades(sim, events = ',capture.output(dput(events)),', .plotInitialTime = ', .plotInitialTime, ')', collapse = "\n"))
              # unlockBinding(mod, sim$.mods)
              if (length(objects))
                list2env(objects, envir(sim))
              sim[[dotMods]][[mod]]$sim <- sim
              aa <- covr::environment_coverage(sim[[dotMods]][[mod]], test_files = tf)
              sim <- sim[[dotMods]][[mod]]$sim
              rm(list = "sim", envir = sim[[dotMods]][[mod]])
              if (is.null(.pkgEnv$._covr)) .pkgEnv$._covr <- list()
              .pkgEnv$._covr <- append(.pkgEnv$._covr, list(aa))
            } else {
              sim <- .runModuleInputObjects(sim, m, objects, notOlderThan)
              cur <- list(eventTime = sim@simtimes$current, moduleName = m, eventType = ".inputObjects", eventPriority = 0)
              sim <- appendCompleted(sim, cur)
            }
          }

        ## schedule each module's init event:
        if (needInitAndInputObjects)
          sim <- scheduleEvent(sim, sim@simtimes[["start"]], m, "init", .first())

        ### add module name to the loaded list
        names(m) <- mFullPath
        modulesLoaded <- append(modulesLoaded, m)

        ### add NAs to any of the dotParams that are not specified by user
        # ensure the modules sublist exists by creating a tmp value in it
        if (is.null(sim@params[[m]])) {
          sim@params[[m]] <- list(.tmp = NA_real_)
        }

        ## add the necessary values to the sublist
        for (x in dotParamsReal) {
          if (is.null(sim@params[[m]][[x]])) {
            sim@params[[m]][[x]] <- NA_real_
          } else if (isTRUE(all(is.na(sim@params[[m]][[x]])))) {
            if (length(sim@params[[m]][[x]]) > 1)
              sim@params[[m]][[x]] <- NA_real_
          }
        }

        ## remove the tmp value from the module sublist
        sim@params[[m]]$.tmp <- NULL

        ### Currently, everything in dotParamsChar is being checked for NULL
        ### values where used (i.e., in save.R).
      }

      ## check that modules all loaded correctly and store result
      if (all(append(core, loadOrderBase) %in% basename2(unlist(modulesLoaded)))) {
        modules(sim) <- modulesLoaded
      } else {
        stop("There was a problem loading some modules.")
      }

      ## Add the data.frame as an attribute
      attr(sim@modules, "modulesGraph") <- parentChildGraph

      ## END OF MODULE PARSING AND LOADING
      if (length(objects)) {
        if (is.list(objects)) {
          if (length(objNames) == length(objects)) {
            # don't override all objects with user supplied objects if the init events have
            #   been run
            if (isTRUE(getOption("spades.allowInitDuringSimInit"))) {
              inputObjectsAllMods <- inputObjects(sim)
              if (is(inputObjectsAllMods, "list"))
                inputObjectsAllMods <- inputObjectsAllMods |> rbindlist() #@depends@dependencies[names()]@inputObjects[["objectName"]]
              inputObjectsAllMods <- unique(inputObjectsAllMods$objectName)
              objectNamesToUse <- inputObjectsAllMods[inputObjectsAllMods %in% sim$.userSuppliedObjNames]
              objectsToUse <- objects[objectNamesToUse]
              objectsToUse <- objectsToUseUpdatesFromPrevInits(sim, objectsToUse)
            } else {
              objectsToUse <- objects
            }

            objs(sim) <- objectsToUse
          } else {
            stop(
              paste(
                "objects must be a character vector of object names",
                "to retrieve from the .GlobalEnv, or a named list of",
                "objects"
              )
            )
          }
        } else {
          newInputs <- data.frame(
            objectName = objNames,
            loadTime = as.numeric(sim@simtimes[["current"]]),
            stringsAsFactors = FALSE
          ) |>
            .fillInputRows(startTime = start(sim))
          inputs(sim) <- newInputs
        }
      }

      ## load files in the filelist
      if (NROW(inputs) | NROW(inputs(sim))) {
        inputs(sim) <- rbind(inputs(sim), inputs)
        if (NROW(events(sim)[moduleName == "load" &
                             eventType == "inputs" &
                             eventTime == start(sim)]) > 0) {
          sim <- doEvent.load(sim, sim@simtimes[["current"]], "inputs")
          events(sim) <- events(sim)[!(eventTime == time(sim) &
                                         moduleName == "load" &
                                         eventType == "inputs"), ]
        }
        if (any(events(sim)[["eventTime"]] < start(sim))) {
          warning(
            paste0(
              "One or more objects in the inputs filelist was ",
              "scheduled to load before start(sim). ",
              "It is being be removed and not loaded. To ensure loading, loadTime ",
              "must be start(sim) or later. See examples using ",
              "loadTime in ?simInit"
            )
          )
          events(sim) <- events(sim)[eventTime >= start(sim)]
        }
      }

      if (length(outputs)) {
        outputs(sim) <- outputs
      }

      ## check the parameters supplied by the user
      checkParams(sim, dotParams, unlist(sim@paths[["modulePath"]]))
      #sim <- elapsedTimeInSimInit(._startClockTime, sim)
      #._startClockTime <- Sys.time()
    },
    message = function(m) {
      message(loggingMessage(m$message, prefix = prefixSimInit))
      # This will "muffle" the original message
      tryCatch(invokeRestart("muffleMessage"), error = function(e) NULL)
    },
    warning = function(w) {
      if (grepl("In .+:", w$message)) {
        warningSplitOnColon(w)
        invokeRestart("muffleWarning")
      }
      # This is a box mishap
      if (isTRUE(any(grepl("'package:stats' may not be available when loading",
                           w$message)))) {
        invokeRestart("muffleWarning")
      }
    }
    )

    return(invisible(sim))
})

## Only deal with objects as character
#' @rdname simInit
setMethod(
  "simInit",
  signature(
    times = "ANY",
    params = "ANY",
    modules = "ANY",
    objects = "character",
    paths = "ANY",
    inputs = "ANY",
    outputs = "ANY",
    loadOrder = "ANY"
  ),
  definition = function(times,
                        params,
                        modules,
                        objects,
                        paths,
                        inputs,
                        outputs,
                        loadOrder,
                        notOlderThan, ...) {
    namesMatchCall <- names(match.call())
    namesMatchCall <- setdiff(namesMatchCall, ...names())
    li <- Map(x = namesMatchCall[-1], function(x) eval(parse(text = x)))
    # li <- lapply(namesMatchCall[-1], function(x) eval(parse(text = x)))
    # names(li) <- namesMatchCall[-1]
    # find the simInit call that was responsible for this, get the objects
    #   in the environment of the parents of that call, and pass them to new
    #   environment.
    li$objects <- .findObjects(objects)
    names(li$objects) <- objects

    li <- .fillInSimInit(li, namesMatchCall)

    sim <- simInit(times = li$times, params = li$params,
                   modules = li$modules, objects = li$objects,
                   paths = li$paths, inputs = li$inputs,
                   outputs = li$outputs, loadOrder = li$loadOrder,
                   notOlderThan = li$notOlderThan, ...)

    # sim <- do.call("simInit", args = li, quote = TRUE)

    return(invisible(sim))
})

## Only deal with modules as character vector
#' @rdname simInit
setMethod(
  "simInit",
  signature(
    times = "ANY",
    params = "ANY",
    modules = "character",
    objects = "ANY",
    paths = "ANY",
    inputs = "ANY",
    outputs = "ANY",
    loadOrder = "ANY"
  ),
  definition = function(times,
                        params,
                        modules,
                        objects,
                        paths,
                        inputs,
                        outputs,
                        loadOrder,
                        notOlderThan, ...) {
    namesMatchCall <- names(match.call())
    namesMatchCall <- setdiff(namesMatchCall, ...names())
    li <- Map(x = namesMatchCall[-1], function(x) eval(parse(text = x)))
    # names(li) <- namesMatchCall[-1]
    li$modules <- as.list(modules)

    li <- .fillInSimInit(li, namesMatchCall)

    sim <- simInit(times = li$times, params = li$params,
                   modules = li$modules, objects = li$objects,
                   paths = li$paths, inputs = li$inputs,
                   outputs = li$outputs, loadOrder = li$loadOrder,
                   notOlderThan = li$notOlderThan, ...)

    return(invisible(sim))
  }
)

###### individual missing elements
#' @rdname simInit
setMethod(
  "simInit",
  signature(
    times = "ANY",
    params = "ANY",
    modules = "ANY",
    objects = "ANY",
    paths = "ANY",
    inputs = "ANY",
    outputs = "ANY",
    loadOrder = "ANY"
  ),
  definition = function(times,
                        params,
                        modules,
                        objects,
                        paths,
                        inputs,
                        outputs,
                        loadOrder,
                        notOlderThan, ...) {
    # browser(expr = exists("._simInit_1"))
    namesMatchCall <- names(match.call())
    namesMatchCall <- setdiff(namesMatchCall, ...names())

    li <- Map(x = namesMatchCall[-1], function(x) eval(parse(text = x)))
    li <- .fillInSimInit(li, namesMatchCall)

    expectedClasses <- c("list",
                         "list",
                         "list",
                         "list",
                         "list",
                         "data.frame",
                         "data.frame",
                         "character")
    listNames <- names(li)
    expectedOrder <- c("times",
                       "params",
                       "modules",
                       "objects",
                       "paths",
                       "inputs",
                       "outputs",
                       "loadOrder")
    ma <- match(expectedOrder, listNames)
    li <- li[ma]

    correctArgs <- (sapply(seq_along(li), function(x) {
      is(li[[x]], expectedClasses[x])
    }))
    if (!all(correctArgs)) {
      plural <- (sum(!correctArgs) > 1) + 1
      expectedDF <- apply(data.frame(arg = names(li), expectedClasses), 1, paste, collapse = " = ")
      stop("simInit is incorrectly specified. ", " The ", paste(names(li)[!correctArgs], collapse = ", "), " argument",
           c("", "s")[plural], " ", c("is", "are")[plural], " specified incorrectly.",
           c(" It is", " They are")[plural], " expected to be ",
           paste(expectedDF[!correctArgs], collapse = ", "))
    }

    expectedInnerClasses <- list(times = list(start = "numeric",
                                              end = "numeric",
                                              timeunit = "character"),
                         params = "list",
                         modules = "character",
                         objects = "ANY",
                         paths = "character",
                         inputs = "ANY",
                         outputs = "ANY",
                         loadOrder = "ANY")
    neic <- names(expectedInnerClasses)
    names(neic) <- neic
    namesInner <- lapply(neic, function(x) NULL)
    # browser(expr = exists("._simInit_2"))
    correctArgsInner <- unlist(lapply(seq_along(li), function(x) {
      # browser(expr = exists("._simInit_3"))
      if (isTRUE(is(li[[x]], "list")) &&
          isTRUE(all(expectedInnerClasses[[x]] != "ANY"))) {
        if (is(expectedInnerClasses[[x]], "list")) {
          items <- if (length(names(li[[x]])) > 0) {
            names(expectedInnerClasses[[x]])[match(names(li[[x]]),
                                        names(expectedInnerClasses[[x]]))]
          } else {
            seq_along(li[[x]])
          }
          NAItems <- is.na(items)
          if (any(NAItems)) { # delete unnamed list elements; they must be named
            if (length(li[[x]]) > length(names(expectedInnerClasses[[x]]))) {
              # too many items; this is an error
              items <- names(li[[x]])
            } else {
              # browser(expr = exists("innerClasses"))
              items[NAItems] <- names(expectedInnerClasses[[x]][NAItems])[
                !names(expectedInnerClasses[[x]])[NAItems] %in% na.omit(items)]
              names(li[[x]])[NAItems] <- items[NAItems]
            }

          }
          namesInner[[x]] <<- items

          all(sapply(items, function(y) {
            if (is.null(expectedInnerClasses[[x]][[y]])) {
              FALSE
            } else {
              is(li[[x]][[y]], expectedInnerClasses[[x]][[y]])
            }
          }))
        } else {
          if (length(li[[x]]) > 0)
            all(sapply(seq_along(li[[x]]), function(y) {
              is(li[[x]][[y]], expectedInnerClasses[[x]])
            }))
        }
      } else {
        TRUE
      }
    }))
    # browser(expr = exists("innerClasses"))
    # give names to inner elements if they were only done by order
    nulls <- sapply(namesInner, is.null)
    eic <- expectedInnerClasses[!nulls]
    namesInner <- namesInner[!nulls]
    if (length(namesInner) > 0) {
      li[names(namesInner)] <- Map(lis = li[names(namesInner)],
                                   nam = namesInner,
                                   expected = expectedInnerClasses[names(namesInner)],
                                   function(lis, nam, expected) {
                                     out <- setNames(lis, nam)
                                     out[names(expected)]
                                     })
    }

    if (!all(correctArgsInner)) {
      plural <- (sum(!correctArgsInner) > 1) + 1
      expectedList <- append(list(arg = names(li)), list(expectedInnerClasses))
      stop("simInit is incorrectly specified. ", " The ", paste(names(li)[!correctArgsInner], collapse = ", "), " argument",
           c("", "s")[plural], " ", c("is", "are")[plural], " specified incorrectly.",
           c(" It is", " They are")[plural], " expected to be a list of ",
           paste(expectedList[[2]][!correctArgsInner], collapse = ", "), " objects")
    }

    sim <- simInit(times = li$times, params = li$params,
                   modules = li$modules, objects = li$objects,
                   paths = li$paths, inputs = li$inputs,
                   outputs = li$outputs, loadOrder = li$loadOrder,
                   notOlderThan = li$notOlderThan, ...)

    # sim2 <- do.call("simInit", args = li, quote = TRUE)

    return(invisible(sim))
})


#' `simInit2` is a convenience wrapper for `do.call(simInit, listOfArgs)`,
#' i.e., a user can pass a list of all the arguments.
#'
#' @export
#' @rdname simInit
#' @param l A list of arguments to passed to `simInit`.
simInit2 <- function(l) {
  do.call(simInit, l)
}

#' `spades2` is a convenience wrapper for `do.call(spades, listOfArgs)`,
#' i.e., a user can pass a list of all the arguments.
#'
#' @export
#' @rdname spades
#' @param l A list of arguments to passed to `spades`.
spades2 <- function(l) {
  do.call(spades, l)
}

#' `simInitAndSpades2` is a convenience wrapper for `do.call(simInitAndSpades, listOfArgs)`,
#' i.e., a user can pass a list of all the arguments.
#'
#' @export
#' @rdname simInitAndSpades
#' @param l A list of arguments to passed to `simInitAndSpades`.
simInitAndSpades2 <- function(l) {
  doCallSafe(simInitAndSpades, l)
}



#' Memory safe alternative to `do.call`
#'
#' `doCallSafe` is an alternative implementation for `do.call` that does not
#' evaluate the `args` prior to running. This means that R does not become unresponsive
#' when there are large objects in the `args`. This should be used *always* instead
#' of `do.call`, whenever there are possibly large objects within the `args`. This is
#' a verbatim copy from package `Gmisc` at
#' \url{https://search.r-project.org/CRAN/refmans/Gmisc/html/fastDoCall.html}
#'
#' @returns Same as `do.call`, but without the memory inefficiency.
#'
#' @export
#' @rdname do.call
#' @inheritParams base::do.call
doCallSafe <- function (what, args, quote = FALSE, envir = parent.frame()) {
  # Copied directly from: https://search.r-project.org/CRAN/refmans/Gmisc/html/fastDoCall.html
  if (quote) {
    args <- lapply(args, enquote)
  }
  if (is.null(names(args)) || is.data.frame(args)) {
    argn <- args
    args <- list()
  }
  else {
    argn <- lapply(names(args)[names(args) != ""], as.name)
    names(argn) <- names(args)[names(args) != ""]
    argn <- c(argn, args[names(args) == ""])
    args <- args[names(args) != ""]
  }
  if ("character" %in% class(what)) {
    if (is.character(what)) {
      fn <- strsplit(what, "[:]{2,3}")[[1]]
      what <- if (length(fn) == 1) {
        get(fn[[1]], envir = envir, mode = "function")
      }
      else {
        get(fn[[2]], envir = asNamespace(fn[[1]]), mode = "function")
      }
    }
    call <- as.call(c(list(what), argn))
  }
  else if ("function" %in% class(what)) {
    f_name <- deparse(substitute(what))
    call <- as.call(c(list(as.name(f_name)), argn))
    args[[f_name]] <- what
  }
  else if ("name" %in% class(what)) {
    call <- as.call(c(list(what, argn)))
  }
  eval(call, envir = args, enclos = envir)
}
#' Call `simInit` and `spades` together
#'
#' These functions are convenience wrappers that may allow for more efficient caching.
#' Passes all arguments to `simInit()`, then passes the created `simList` to `spades()`.
#'
#' @param ... Arguments passed to `simInit()` and `spades()`
#'
#' @return Same as [spades()] (a `simList`) or
#'
#'
#' @seealso [simInit()], [spades()]
#'
#' @export
#' @inheritParams simInit
#' @inheritParams spades
#'
#' @rdname simInitAndSpades
simInitAndSpades <- function(times, params, modules, objects, paths, inputs, outputs, loadOrder,
                             notOlderThan, debug, progress, cache, .plots,
                             .plotInitialTime, .saveInitialTime, events, ...) {

  # because Cache (and possibly others, we have to strip any other call wrapping simInitAndSpades)
  lsAllNames <- ls(all.names = TRUE)
  # lsAllNames <- lsAllNames[lsAllNames != "..."]
  formsSimInit <- setdiff(formalArgs(simInit), "...")
  formsSpades <- setdiff(formalArgs(spades), "...")
  formsOnlySpades <- setdiff(formsSpades, formsSimInit)

  # lsAllNames <- ls()
  passedArgs <- as.list(match.call(simInit))[-1]
  passedArgsNames <- setdiff(names(passedArgs), formsOnlySpades)
  namesMatchCall <- names(match.call())
  defaultArgs <- .fillInSimInit(list(), namesMatchCall)
  simInitCall <- as.call(x = append(list(simInit), append(passedArgs[passedArgsNames], defaultArgs)))
  sim <- eval(simInitCall, envir = parent.frame())

  # objsAll <- mget(lsAllNames, envir = environment())
  # objsSimInit <- objsAll[formalArgs(simInit)]

  # objsSimInit <- .fillInSimInit(objsSimInit, namesMatchCall)

  # namesMatchCall <- names(match.call())

  # sim <- simInit(times = objsSimInit$times, params = objsSimInit$params,
  #                modules = objsSimInit$modules, objects = objsSimInit$objects,
  #                paths = objsSimInit$paths, inputs = objsSimInit$inputs,
  #                outputs = objsSimInit$outputs, loadOrder = objsSimInit$loadOrder,
  #                notOlderThan = objsSimInit$notOlderThan, ...)
  opts <- options(spades.loadReqdPkgs = FALSE)
  on.exit(options(opts), add = TRUE)
  #sim <- do.call(simInit, objsSimInit) # serializes the objects

  passedArgsToSpades <- as.list(match.call(spades))[-1]
  spadesFormals <- formalArgs(spades)[formalArgs(spades) %in% names(passedArgsToSpades)]
  ## quote is so that entire simList is not serialized in do.call
  objsSpades <- append(alist(sim = sim), passedArgs[spadesFormals])
  sim <- do.call(spades, objsSpades)
}

#' Identify child modules from a recursive list
#'
#' There can be parents, grandparents, etc
#'
#' @param sim a `simList` object
#'
#' @param modules List of module names
#'
#' @return list of `modules` will flat named list of all module names (children, parents etc.) and
#'         `childModules` a non-flat named list of only the `childModule` names.
#'
#' @importFrom reproducible basename2
#' @keywords internal
#' @rdname identifyChildModules
.identifyChildModules <- function(sim, modules) {
  modulesToSearch <- modules
  if (any(duplicated(modules))) {
    message("Duplicate module, ", modules[duplicated(modules)], ", specified. Skipping loading it twice.")
  }
  if (length(modules) > 0) {
    modulesToSearch3 <- lapply(.parseModulePartial(sim, modulesToSearch,
                                                   defineModuleElement = "childModules",
                                                   envir = sim@.xData[[".parsedFiles"]]),
                               as.list)
    if (length(modulesToSearch3) > 0) {
      isParent <- unlist(lapply(modulesToSearch3, function(x) length(x) > 0))

      modulesToSearch3[isParent] <- Map(
        x = modulesToSearch3[isParent],
        nam = dirname(names(modulesToSearch3[isParent])),
        function(x, nam) {
          mods <- lapply(x, function(y) file.path(nam, y))
          names(mods) <- unlist(lapply(mods, basename2))
          .identifyChildModules(sim = sim, modules = mods)
        }
      )
      modulesToSearch2 <- as.list(names(modulesToSearch3[!isParent]))
      names(modulesToSearch2) <- names(modulesToSearch3[!isParent])
      modulesToSearch3[!isParent] <- modulesToSearch2
      modulesToSearch <- modulesToSearch3
    }
  }
  return(modulesToSearch)
}

#' Identify module names up to a given recursive level
#'
#' With children, parents, grandparents, etc.; there can be several "layers" of recursion.
#' Some functions need to evaluate the outer level for a value, if found, they don't need
#' to proceed further. If not found, increment one more level of recursion, etc.
#'
#' @param recursive Numeric. The depth of recursion, where 0 is only top level, 1 is 1 level in etc.
#' @param modList (Nested) Named list of module names
#' @return Character vector of modules names
#'
#' @keywords internal
#' @rdname findModuleName
.findModuleName <- function(modList, recursive = 0) {
  isParent <- unlist(lapply(modList, function(x) length(x) > 1))
  parentNames <- lapply(modList, function(x) x)
  parentNames <- if (any(unlist(isParent))) {
    if (recursive) {
      parentNamesInside <- lapply(modList[isParent], .findModuleName, recursive = recursive - 1)
      c(names(isParent[isParent]), unlist(parentNamesInside))
    } else {
      names(isParent)
    }
  } else {
    names(isParent)
  }

  return(parentNames)
}

#' Run module's `.inputObjects`
#'
#' Run `.inputObjects()` from each module file from each module, one at a time,
#' and remove it from the `simList` so next module won't rerun it.
#'
#' @keywords internal
#' @importFrom cli col_green
#' @importFrom reproducible basename2
#' @rdname runModuleInputsObjects
.runModuleInputObjects <- function(sim, m, objects, notOlderThan) {
  # If user supplies the needed objects, then test whether all are supplied.
  # If they are all supplied, then skip the .inputObjects code
  cacheIt <- FALSE

  mnames <- vapply(seq_along(sim@depends@dependencies), function(k) {
    sim@depends@dependencies[[k]]@name
  }, character(1))
  mBase <- basename2(m)
  i <- which(mnames == mBase)

  ## temporarily assign current module
  sim@current <- newEventList <- list(
    eventTime = start(sim),
    moduleName = mBase,
    eventType = ".inputObjects",
    eventPriority = .normal()
  )

  # loggingMessage helpers
  simNestingRevert <- sim[[._txtSimNesting]]
  on.exit(sim[[._txtSimNesting]] <- simNestingRevert, add = TRUE)
  sim[[._txtSimNesting]] <- simNestingOverride(sim, sim@current$moduleName)
  assign(._txtSimNesting, sim[[._txtSimNesting]])
  # ._simNesting <- sim[[._txtSimNesting]]

  inputObjectsThisModule <- sim@depends@dependencies[[i]]@inputObjects[["objectName"]]
  allObjsProvided <- inputObjectsThisModule %in% sim$.userSuppliedObjNames
  if (!all(allObjsProvided)) {
    if (!is.null(.getModuleInputObjects(sim, m))) {
      if (!missing(objects)) {

        # If the init events have gone already, then the "objects" shouldn't override
        #   the outputs of those modules
        objectsToUse <- objects[inputObjectsThisModule[allObjsProvided]]
        if (isTRUE(getOption("spades.allowInitDuringSimInit"))) {
          objectsToUse <- objectsToUseUpdatesFromPrevInits(sim, objectsToUse)
        }
        list2env(objectsToUse, envir = sim@.xData)
      }
      a <- P(sim, ".useCache", mBase)
      if (!is.null(a)) {
        if (!identical(FALSE, a)) {
          if (isTRUE(a)) {
            cacheIt <- TRUE
          } else {
            if (".inputObjects" %in% a) {
              cacheIt <- TRUE
            }
          }
        }
      }

      # message(cli::col_green("Running .inputObjects for ", mBase, sep = ""))

      debug <- getDebug() # from options first, then override if in a simInitAndSpades
      if  (is.call(debug))
        debug <- eval(debug)

      cur <- sim@current
      curModNam <- cur$moduleName
      if (!(all(unlist(lapply(debug, identical, FALSE))))) {
        .pkgEnv[[".spadesDebugFirst"]] <- TRUE
        # sim[["._spadesDebugWidth"]] <- c(9, 10, 9, 13)
      }
      debugMessage(debug, sim, cur, sim@.xData[[dotMods]][[curModNam]], curModNam)

      if (!(FALSE %in% debug || any(is.na(debug))))
        objsIsNullBefore <- objsAreNull(sim)

      allowSequentialCaching <- getOption("spades.allowSequentialCaching", FALSE)
      if (isTRUE(cacheIt)) {
        moduleSpecificInputObjects <- sim@depends@dependencies[[i]]@inputObjects[["objectName"]]
        moduleSpecificInputObjects <- na.omit(moduleSpecificInputObjects)
        moduleSpecificInputObjects <- c(moduleSpecificInputObjects, m)
        moduleSpecificInputObjects <- c(moduleSpecificInputObjects, paste0(dotMods, "$", m), paste0(dotObjs, "$", m))

        # ensure backwards compatibility with non-namespaced modules
        if (.isNamespaced(sim, mBase)) {
          moduleSpecificObjs <- paste(mBase, ".inputObjects", sep = ":")
          objectsToEvaluateForCaching <- c(moduleSpecificObjs)
        } else {
          objectsToEvaluateForCaching <- c(grep(ls(sim@.xData, all.names = TRUE),
                                                pattern = mBase, value = TRUE),
                                           na.omit(moduleSpecificInputObjects))
        }

        .inputObjects <- .getModuleInputObjects(sim, mBase)
        if (!is.null(.inputObjects)) {
          args <- as.list(formals(.inputObjects))
          env <- environment()
          if (!isTRUE(names(args) %in% "sim"))
            stop("The .inputObjects can only have a single argument, specifically 'sim'. ",
                 "Currently, ", mBase, " has others")
          args <- lapply(args[unlist(lapply(args, function(x) all(nzchar(x))))], eval, envir = env)
          args[["sim"]] <- sim

          ## This next line will make the Caching sensitive to userSuppliedObjs
          ##  (which are already in the simList) or objects supplied by another module
          inSimList <- suppliedElsewhere(moduleSpecificInputObjects, sim, where = c("sim", "i", "c"))
          if (any(inSimList)) {
            objectsToEvaluateForCaching <- c(objectsToEvaluateForCaching,
                                             # objSynName,
                                             moduleSpecificInputObjects[inSimList])
          }
          moduleSpecificInputObjects <- c(moduleSpecificInputObjects, objSynName)


          showSimilar <- if (is.null(sim@params[[mBase]][[".showSimilar"]]) ||
                             isTRUE(is.na(sim@params[[mBase]][[".showSimilar"]]))) {
            isTRUE(getOption("reproducible.showSimilar", FALSE))
          } else {
            isTRUE(sim@params[[mBase]][[".showSimilar"]])
          }

          if (any(".inputObjects" %in% debug))
            debugonce(.inputObjects)

          modParams <- sim@params[[mBase]]
          paramsDontCacheOnActual <- names(sim@params[[mBase]]) %in%
            paramsDontCacheOn
          paramsWoKnowns <- modParams[!paramsDontCacheOnActual]

          # nextEvent <- NULL
          runFnCallAsExpr <- TRUE
          debugForCache <- debugToVerbose(debug)
          if (allowSequentialCaching) {
            sim <- allowSequentialCaching1(sim, cacheIt, moduleCall = ".inputObjects", verbose = debugForCache)
            runFnCallAsExpr <- is.null(attr(sim, "runFnCallAsExpr"))
          }
          if (runFnCallAsExpr) {
            pkgs <- Require::extractPkgName(unlist(moduleMetadata(sim, currentModule(sim))$reqdPkgs))
            pkgs <- c(pkgs, "stats")
            # if (getOption("spades.useBox", FALSE) && FALSE)
            #   do.call(box::use, lapply(pkgs, as.name))
            if (any(mBase %in% getOption("spades.debugModule"))) {
              browser()
            }

            # if (isTRUE("Biomass_borealDataPrep" %in% mBase)) {
            #   aaaa <<- 1; on.exit(rm(aaaa, envir = .GlobalEnv))
            # }
            # aaaa <<- 1; on.exit(rm(aaaa, envir = .GlobalEnv))

            # Remove outputObjects from @depends, as it should not affect the .inputObjects
            dependsSlots <- metadataToDigest
            dependsSlots <- outputsRmDontNeedForCache(dependsSlots, "outputObjects")
            # dependsSlots <- setdiff(dependsSlots, "outputObjects")

            sim <- .inputObjects(sim) |>
              Cache(
                .objects = objectsToEvaluateForCaching,
                notOlderThan = notOlderThan,
                outputObjects = moduleSpecificInputObjects,
                quick = getOption("reproducible.quick", FALSE),
                cachePath = sim@paths$cachePath,
                classOptions = list(events = FALSE,
                                    current = FALSE,
                                    completed = FALSE,
                                    simtimes = FALSE,
                                    params = paramsWoKnowns,
                                    depends = dependsSlots,
                                    # .globals = globsWoKnowns,
                                    modules = mBase),
                showSimilar = showSimilar,
                .functionName = paste0(".inputObjects_", mBase),
                userTags = c(paste0("module:", mBase),
                             "eventType:.inputObjects"),
                verbose = debugForCache)
          }
          if (allowSequentialCaching) {
            sim <- allowSequentialCachingUpdateTags(sim, cacheIt)
          }


          # put back the current values of params that were not cached on
          if (sum(paramsDontCacheOnActual))
            sim@params[[mBase]][paramsDontCacheOnActual] <- modParams[paramsDontCacheOnActual]



        }
      } else {
        # .modifySearchPath(pkgs = sim@depends@dependencies[[i]]@reqdPkgs)
        .inputObjects <- .getModuleInputObjects(sim, mBase)
        if (!is.null(.inputObjects)) {
          sim <- .inputObjects(sim)
        }
      }
      if (allowSequentialCaching) {
        sim <- allowSequentialCachingFinal(sim)
      }

      if (!(FALSE %in% debug || any(is.na(debug)))) {
        sim <- objectsCreatedPost(sim, objsIsNullBefore)
      }
      evalPostEvent() # this is getOption("spades.debugPrint")
    }
  } else {
    message(
      cli::col_green("All required inputObjects for ", mBase, " provided; skipping .inputObjects")
    )
  }

  sim@current <- list()
  return(sim)
}

.timeunitDefault <- function() "year"
.timesDefault <- function() list(start = 0, end = 10)
.paramsDefault <- function() list()
.modulesDefault <- function() list()
.objectsDefault <- function() list()
.pathsDefault <- function() suppressMessages(.paths())
.inputsDefault <- function() as.data.frame(NULL)
.outputsDefault <- function() as.data.frame(NULL)
.loadOrderDefault <- function() character(0)
.notOlderThanDefault <- function() NULL

#' `simInit` default values
#' @export
#' @rdname simInit
simInitDefaults <- function() {
  times <- append(.timesDefault(), list(timeunit = .timeunitDefault()))
  simInitCall <- call("simInit", times = times)

  .fillInSimInit(list(times = times), namesMatchCall = names(simInitCall))
}

.fillInSimInit <- function(li, namesMatchCall) {
  fa <- formalArgs(simInit)
  fa <- fa[!fa %in% "..."]
  isMissing <- !fa %in% namesMatchCall[-1]
  formalsTF <- fa

  names(isMissing) <- formalsTF

  if (any(isMissing))
    li[names(isMissing)[isMissing]] <- Map(x = isMissing[isMissing], nam = names(isMissing)[isMissing],
                         function(x, nam) {
                           get(paste0(".", nam, "Default"))()}
    )
  # if (isTRUE(isMissing["times"])) li$times <- .timesDefault
  # if (isTRUE(isMissing["params"])) li$params <- .paramsDefault
  # if (isTRUE(isMissing["modules"])) li$modules <- .modulesDefault
  # if (isTRUE(isMissing["objects"])) li$objects <- .objectsDefault
  # if (isTRUE(isMissing["paths"])) li$paths <- suppressMessages(.paths())
  # if (isTRUE(isMissing["inputs"])) li$inputs <- .inputsDefault
  # if (isTRUE(isMissing["outputs"])) li$outputs <- .outputsDefault
  # if (isTRUE(isMissing["loadOrder"])) li$loadOrder <- .loadOrderDefault

  return(li)
}

.checkModuleDirsAndFiles <- function(modules, modulePath) {
  moduleDirsPoss <- lapply(modules, function(m) file.path(modulePath, m))
  moduleDirsExist <- lapply(moduleDirsPoss, function(poss) dir.exists(poss))
  moduleFilesPoss <- lapply(moduleDirsPoss, function(poss) {
    file.path(file.path(poss, paste0(basename(poss), ".R")))
  })
  moduleFilesExist <- lapply(moduleFilesPoss, function(poss) file.exists(poss))
  # dir.exists(file.path(paths$modulePath, unlist(modules)))
  if (!isTRUE(all(unlist(lapply(moduleDirsExist, any))))) {
    if (sum(!unlist(moduleDirsExist)) > 1) {
      moduleTxt1 <- "These modules"
      moduleTxt2 <- "don't"
    } else {
      moduleTxt1 <- "This module"
      moduleTxt2 <- "doesn't"
    }
    stop(moduleTxt1, ":\n    ", paste(unlist(modules)[!unlist(moduleDirsExist)], collapse = ", "), ",\n  ",
         moduleTxt2," exist in:\n    ", modulePath)
  }
  if (!isTRUE(all(unlist(lapply(moduleFilesExist, any))))) {
    notExist <- !unlist(lapply(moduleFilesExist, any))
    notExist <- Map(poss = moduleDirsPoss[notExist], exist = moduleFilesExist[notExist],
                    f = function(poss, exist) poss[!exist])
    stop(paste0(names(notExist), " doesn't exist in modulePath(sim): (",
                lapply(notExist, paste, collapse = ", "), ")", collapse = "\n"))
  }
  modulePaths <- Map(poss = moduleDirsPoss, exist = moduleDirsExist,
                     f = function(poss, exist) poss[exist][1])
}

#' @importFrom Require extractVersionNumber
#' @importFrom utils packageVersion
checkSpaDES.coreMinVersion <- function(allPkgs) {
  whSC <- grepl("\\<SpaDES.core\\>", allPkgs)
  if (any(whSC)) {
    scPackageFullnames <- allPkgs[whSC]
    sc <- extractVersionNumber(scPackageFullnames)
    # versionSpecs <- Require::getPkgVersions(scPackageFullnames)
    scWONA <- na.omit(sc)# [Package == "SpaDES.core" & hasVersionSpec == TRUE]
    if (NROW(scWONA)) {
      scCurVersion <- packageVersion("SpaDES.core")
      ineq <- extractInequality(scPackageFullnames)
      ok <- compareVersion2(scCurVersion, sc, ineq)

      if (any(ok %in% FALSE))
        stop("One of the modules needs a newer version of SpaDES.core. Please ",
             "restart R and install with: \n",
             "Require::Install(c('",
             paste(scPackageFullnames[ok %in% FALSE], collapse = ", "), "'))")
    }
  }
}

findSmallestTU <- function(sim, mods, childModules) { # recursive function
  out <- mods

  modsForTU <- names(childModules)
  stillFinding <- TRUE
  recurseLevel <- 1
  ## Time unit could be NA, in which case, it should find the smallest one that is inside a parent...
  ## if none there, then inside grandparent etc.
  while (stillFinding && length(modsForTU)) {
    tu <- .parseModulePartial(sim, as.list(modsForTU), defineModuleElement = "timeunit",
                              envir = sim@.xData[[".parsedFiles"]])
    hasTU <- !is.na(tu)
    innerNames <- .findModuleName(childModules, recursive = recurseLevel)
    modsForTU <- innerNames[nzchar(names(innerNames))]
    stillFinding <- all(!hasTU)
    recurseLevel <- recurseLevel + 1 # if there were no time units at the first level of module, go into next level
  }
  if (!exists("tu", inherits = FALSE)) {
    return(list(.timeunitDefault())) # default
  }
  minTU <- minTimeunit(as.list(unlist(tu)))
  if (isTRUE(is.na(minTU[[1]]))) {
    minTU[[1]] <- .timeunitDefault()
  }

  # no timeunits or no modules at all
  if (length(minTU) == 0) minTU <- list(moduleDefaults$timeunit)

  return(minTU)
}

# recursive function to extract parent and child structures
buildParentChildGraph <- function(sim, mods, childModules) {
  out <- childModules

  # A child module will be a list inside a list of parent modules
  isParent <- sapply(out, function(x) is.list(x))
  if (length(isParent)) {
    from <- rep(names(out)[isParent], unlist(lapply(out[isParent], length)))
    to <- unlist(lapply(out, function(x) names(x)))
    if (is.null(to)) to <- character(0)
    outDF <- data.frame(from = from, to = to, stringsAsFactors = FALSE)
    aa <- lapply(childModules[isParent], function(x) buildParentChildGraph(sim, mods, x))
    aa <- rbindlist(aa)
    outDF <- rbind(outDF, aa)
  } else {
    outDF <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
  }
  outDF
}

#' @importFrom Require getCRANrepos
loadPkgs <- function(reqdPkgs) {
  uniqueReqdPkgs <- unlist(reqdPkgs)
  uniqueReqdPkgs <- uniqueReqdPkgs[!duplicated(uniqueReqdPkgs)]

  if (length(uniqueReqdPkgs)) {
    allPkgs <- uniqueReqdPkgs
    if (!any(grepl("SpaDES.core", uniqueReqdPkgs))) {# append SpaDES.core if it isn't already there
      allPkgs <- c(uniqueReqdPkgs, "SpaDES.core")
    }

    # Check for SpaDES.core minimum version
    checkSpaDES.coreMinVersion(allPkgs)
    allPkgs <- grep("^SpaDES.core\\>", allPkgs, value = TRUE, invert = TRUE)

    pkgsDontLoad <- getOption("spades.reqdPkgsDontLoad", NULL)
    allPkgs <- reqdPkgsDontLoad(allPkgs, pkgsDontLoad)

    if (getOption("spades.useRequire") && !getOption("spades.useBox", FALSE)) {
      getCRANrepos(ind = 1) # running this first is neutral if it is set
      Require(allPkgs, require = TRUE, standAlone = FALSE, upgrade = FALSE)
      if (!is.null(pkgsDontLoad)) {
        verbose <- getOption("reproducible.verbose")
        Require::Require(pkgsDontLoad, require = FALSE, standAlone = FALSE,
                         upgrade = FALSE, verbose = verbose - 1)
      }
      # RequireWithHandling(allPkgs, standAlone = FALSE, upgrade = FALSE)
    } else {
      if (!getOption("spades.useBox", FALSE)) {
        allPkgs <- unique(Require::extractPkgName(allPkgs))
        loadedPkgs <- lapply(allPkgs, base::require, character.only = TRUE)
      }
    }
  }
}

#' @importFrom quickPlot whereInStack
#' @importFrom Require messageVerbose
resolveDepsRunInitIfPoss <- function(sim, modules, paths, params, objects, inputs, outputs) {
  # THIS FUNCTION PASSES THINGS TO THE OUTER sim OBJECT as side effects. CAREFUL
  depsGr <- depsGraph(sim, plot = FALSE)
  depsGrDF <- (depsEdgeList(sim, FALSE) |> .depsPruneEdges())
  #depsGrDF1 <- depsEdgeList(sim, FALSE)
  #depsGrDF <- depsGrDF1[from != to]
  if (getOption("spades.allowInitDuringSimInit", TRUE)) {
    cannotSafelyRunInit <- unique(depsGrDF[from != "_INPUT_"]$to)
    hasUnresolvedInputs <- unique(depsGrDF[from == "_INPUT_"]$to)
    canSafelyRunInit <- setdiff(hasUnresolvedInputs, cannotSafelyRunInit)
    shouldRunAltSimInit <- !all(sim@modules %in% canSafelyRunInit)
  }

  loadOrder <- .depsLoadOrder(sim, depsGr) # brings up the loadOrder metadata -- so can add modules that aren't being used
  sim@modules <- sim@modules[na.omit(match(loadOrder, sim@modules))] # na.omit is for loadOrder metadata ones

  if (getOption("spades.allowInitDuringSimInit", TRUE)) {
    if (length(canSafelyRunInit) && isTRUE(shouldRunAltSimInit)) {
      verbose <- getOption("reproducible.verbose")
      messageVerbose(cli::col_yellow("These modules will be run prior to all other modules' .inputObjects"), verbose = verbose)
      messageVerbose(cli::col_yellow("as their outputs are needed by the other modules and ",
                                    "can be safely run"), verbose = verbose)
      safeToRunModules <- paste(canSafelyRunInit, collapse = ", ")
      messageVerbose(cli::col_yellow(safeToRunModules), verbose = verbose)
      stripNchars <- getOption("spades.messagingNumCharsModule") - 5
      stripNcharsSpades <- 2 #stripNchars + 2
      stripNcharsSimInit <- stripNchars + 5
      debug <- getDebug() # from options first, then override if in a simInitAndSpades
      if  (is.call(debug))
        debug <- eval(debug)

      len <- length(sim[[._txtSimNesting]])
      ._simNestingLocal <- sim[[._txtSimNesting]]
      val <- "intsDrngSmInt"
      ._simNestingLocal[len] <- val

      squash <- withCallingHandlers({
        simAlt <- simInit(modules = canSafelyRunInit, paths = paths, params = params,
                          objects = objects, inputs = inputs, outputs = outputs,
                          times = list(start = as.numeric(start(sim)),
                                       end = as.numeric(end(sim)), timeunit = timeunit(sim)),
                          ._startClockTime = sim[[._txtStartClockTime]])
        simAlt@.xData$._ranInitDuringSimInit <- completed(simAlt)$moduleName
        messageVerbose(cli::col_yellow("**** Running spades call for:", safeToRunModules, "****"))
        simAltOut <- spades(simAlt, events = "init", debug = debug)
      })

      Map(mod = canSafelyRunInit, function(mod) {
        objEnv <- simAltOut[[dotObjs]][[mod]] # $.objects
        objsNames <- ls(objEnv, all.names = TRUE)
        objs <- mget(objsNames, objEnv)
        list2env(objs, sim[[dotObjs]][[mod]]) # $.objects)
      })
      # update parameters -- from simAltOut; then from user passed params
      # Don't use `globals(sim)<-` because it updates the parameters, which we don't want here
      sim@params$.globals <- modifyList2(globals(sim), globals(simAltOut))
      sim <- updateParamsFromGlobals(sim, dontUseGlobals = params)
      # This keeps the user passed params
      sim@params <- modifyList2(sim@params, params)

      list2env(objs(simAltOut), envir(sim))

      dotUnderscoreObjs <- ls(pattern = "^._", envir(simAltOut), all.names = TRUE)
      list2env(mget(dotUnderscoreObjs, envir = envir(simAltOut)), envir(sim))

      loadOrder <- loadOrder[!loadOrder %in% canSafelyRunInit]
      list2env(as.list(simAltOut@completed), sim@completed)

      ## 2024-05-14: scheduled saves, via outputs, modules, etc. need to be kept in the queue
      ## don't remove them, as the save init event was already run and won't be run again.

      if (length(simAltOut@events)) {
        # have to remove the .coreModules if they have already run
        modulesRun <- vapply(sim@events, function(x) x$moduleName, FUN.VALUE = character(1))
        coreModules <- modulesRun %in% .coreModules()
        if (any(coreModules))
          sim@events <- sim@events[-which(coreModules)]
        sim@events <- append(sim@events, simAltOut@events)
      }
    }
  }
  # sim@modules <- sim@modules[match(loadOrder, sim@modules)]
  sim
}

updateParamsFromGlobals <- function(sim, dontUseGlobals = list()) {
  modDefaultParams <- Map(mod = sim@depends@dependencies, function(mod) mod@parameters$paramName)
  sim@params <- updateParamsSlotFromGlobals(sim@params, dontUseGlobals = dontUseGlobals,
                                            modDefaultParams = modDefaultParams)
  sim
}

updateParamsSlotFromGlobals <- function(paramsOrig, paramsWithUpdates,
                                        dontUseGlobals = list(),
                                        modDefaultParams) {
  if (missing(paramsWithUpdates)) {
    paramsWithUpdates <- paramsOrig
  }
  globalsUsed <- globalsUsedInModules <- NULL
  globalsDF <- list()
  knownParamsWOdotPlotInitialTime <- setdiff(.knownDotParams, ".plotInitialTime")
  for (mod in setdiff(ls(paramsWithUpdates), unlist(.coreModules()))) { # don't include the dot paramsWithUpdates; just non hidden modules
    modParams <- modDefaultParams[[mod]]
    modParams <- union(modParams, knownParamsWOdotPlotInitialTime)
    userOverrides <- if (is.null(dontUseGlobals[[mod]])) NULL else dontUseGlobals[[mod]]
    common <- intersect(modParams, names(paramsWithUpdates$.globals))
    common <- setdiff(common, names(userOverrides))
    if (length(common)) {
      globalsUsed <- paste(common, sep = ", ")
      globalsUsedInModules <- rep(mod, length(common))
      globalsDF[[mod]] <- list(module = globalsUsedInModules, global = globalsUsed)
      paramsOrig[[mod]][common] <- paramsWithUpdates$.globals[common]
    }
  }
  if (!is.null(globalsUsed)) {
    globalsDF <- rbindlist(globalsDF)
    setkeyv(globalsDF, c("global", "module"))
    message("The following .globals were used:")
    reproducible::messageDF(globalsDF)
  }
  paramsOrig
}


#' @keywords internal
#' @importFrom reproducible messageColoured
objectsCreatedPost <- function(sim, objsIsNullBefore) {
  objsIsNullAfter <- objsAreNull(sim)
  newObjs <- setdiffNamed(objsIsNullAfter, objsIsNullBefore)
  if (length(newObjs)) {
    df <- data.frame(newObjects = names(newObjs))
    messageColoured("New objects created:", colour = "yellow")
    messageDF(df, colour = "yellow", colnames = FALSE)
    setDT(df)
    sim@current$eventTime <- convertTimeunit(sim@current$eventTime, unit = sim@simtimes$timeunit, sim@.xData)
    set(df, NULL, names(sim@current), sim@current)
    if (is.null(sim$._objectsCreated))
      sim$._objectsCreated <- list()
    sim$._objectsCreated <- append(sim$._objectsCreated, list(df))
  }
  sim
}

objsAreNull <- function(sim) {
  mapply(obj = mget(grep("^\\._|^\\.mods|^\\.parsedFiles|^\\.userSuppliedObjNames",
                         ls(sim, all.names = TRUE), invert = TRUE, value = TRUE),
                    envir = envir(sim)), function(obj) is.null(obj))
}

adjustModuleNameSpacing <- function(modNames) {
  nchar <- getOption("spades.messagingNumCharsModule") - loggingMessagePrefixLength
  if (length(modNames)) {
    for (i in seq(nchar, max(nchar(modNames)))) {
      modName8Chars <- mapply(
        modName = unname(modNames),
        MoreArgs = list(ncm = i),
        FUN = function(modName, ncm) moduleNameStripped(modName, numCharsMax = ncm)
      )
      if (all(!duplicated(modName8Chars))) {
        options("spades.messagingNumCharsModule" = i + loggingMessagePrefixLength)
        break
      }
    }
  }
}

RequireWithHandling <- function(allPkgs, standAlone = FALSE, upgrade = FALSE) {
  # alreadyLoadedMess <- c()
  withCallingHandlers(
    Require(allPkgs, standAlone = standAlone, upgrade = upgrade) # basically don't change anything
    , message = function(m) {
      if (any(grepl("Error: package or namespace", m$message))) {
        pkg <- gsub("^.+namespace \u2018(.+)\u2019 .+ is already loaded.+$", "\\1", m$message)
        message(m)
        stop(stopMessForRequireFail(pkg))
      }
    }
    , warning = function(w) {
      warnMess <- "^.+ersion .+ of \u2018(.+)\u2019 masked by .+$"
      if (any(grepl(warnMess, w$message))) {
        pkg <- gsub(warnMess, "\\1", w$message)
        warning(w)
        stop(stopMessForRequireFail(pkg))
      }
    }
  )
}

#' @importFrom cli cli cli_code col_blue col_yellow cli_text
stopMessForRequireFail <- function(pkg) {
  cli::cli({
    cli::cli_text("The above error(s) likely mean(s) you must restart R and run again.")
    cli::cli_text(
      "If this/these occur(s) again, your session likely pre-loads old packages",
      "from e.g., your personal library."
    )
    cli::cli_text(
      cli::col_yellow("The best thing to do is try to restart R without loading any packages.")
    )

    cli::cli_text("\nIf that is not easy to do, try to update e.g., a CRAN package, with:")
    cli::cli_text(cli::col_yellow("Restart your R session"))
    cli::col_blue(cli::cli_code(paste0("\ninstall.packages(c('", pkg, "'))")))
    cli::cli_text(cli::col_yellow("Restart your R session"))

    cli::cli_text("\nIf that fails (including non-CRAN packages), try removing the old one:")
    cli::cli_text(cli::col_yellow("Restart your R session"))
    cli::col_blue(cli::cli_code(paste0("\nremove.packages(c('", pkg, "'))")))
    cli::cli_text(cli::col_yellow("Restart your R session"))

    cli::cli_text("\nThis should trigger a re-installation, or allow for a manual install.")
  })
}

getDebug <- function() {
  hasDebug <- tryCatch(whereInStack("debug"), silent = TRUE, error = function(e) FALSE)
  debug <- getOption("spades.debug")
  if (!isFALSE(hasDebug)) {
    newDebug <- try(get("debug", hasDebug), silent = TRUE)
    if (!is(newDebug, "try-error"))
      debug <- newDebug
  }
  debug
}

#' @keywords internal
#' @importFrom Require messageVerbose
dealWithOptions <- function(objects, dotNames, sim,
                            thePkgs = c("SpaDES.core", "reproducible", "Require")) {
  finished <- FALSE

  thePkgsAsOptions <- gsub("SpaDES.core", "spades", thePkgs)
  thePkgsGrep <- paste0(paste(thePkgsAsOptions, collapse = "\\.|"), "\\.")
  fns <- paste0(thePkgsAsOptions, "Options")
  allOptions <- Map(pkg = thePkgs, fn = fns, function(pkg, fn)
    getFromNamespace(fn, ns = pkg)())

  currOptionsLong <- names(unlist(unname(allOptions), recursive = FALSE))

  currOptionsShort <- gsub(thePkgsGrep, "", currOptionsLong)
  namesPoss <- if (is.null(dotNames) && !missing(sim)) {
    names(sim)
  } else {
    dotNames
  }

  optionsDotsShort <- currOptionsShort %in% namesPoss
  optionsDotsLong <- currOptionsLong %in% namesPoss
  optionsDots <- optionsDotsShort | optionsDotsLong

  either <- optionsDotsShort | optionsDotsLong
  optionsNamesShort <- currOptionsShort[either]
  # theOrd2 <- match(so$short, optionsNamesShort)
  optionsNamesShort <- optionsNamesShort
  optionsNamesLong <- currOptionsLong[either]
  optionsNamesLong <- optionsNamesLong
  df <- data.frame(long = optionsNamesLong, short = optionsNamesShort)

  suppliedOrder <- data.frame(namesPoss = namesPoss, order = seq(namesPoss),
                              short = gsub(thePkgsGrep, "", namesPoss))
  suppliedOrder <- merge(df, suppliedOrder)
  theOrd <- order(suppliedOrder$order)
  suppliedOrder <- suppliedOrder[theOrd, ]
  nonOptionsDots <- setdiff(namesPoss, suppliedOrder$namesPoss)

  optsPrev <-  NULL
  optionsValsLong <- optionsVals <- list()
  keepObjNames <- character()
  if (any(optionsDots)) {
    # optionsNamesLong <- paste0("spades.", optionsNamesShort)
    optionsVals <- if (missing(objects)) {
      mget(suppliedOrder$namesPoss, envir = envir(sim))
    } else {
      objects[suppliedOrder$namesPoss]
    }
    optionsValsLong <- optionsVals
    names(optionsValsLong) <- suppliedOrder$long# optionsNamesLong
    # clean up objects -- so no options are in the sim during
    # simInit call
    optsPrev <- options(optionsValsLong)
    on.exit(if (isFALSE(finished)) {
      options(optsPrev)
    })

    # deal with verbose
    verboses <- grepl("\\.verbose", names(optionsValsLong))
    verbose <- if (any(verboses)) {
      tail(optionsVals[verboses], 1)[[1]]
    } else {
      getOption("reproducible.verbose")
    }
    newValue <- as.character(unname(unlist(optionsValsLong)))
    oldValue <- as.character(unname(unlist(optsPrev)))
    changed <- newValue != oldValue
    if (any(changed )) {
      messageVerbose("The following options have been changed temporarily (during this call)",
                     verbose = verbose)
      updates <- try(silent = TRUE,
                     data.table::data.table(optionName = suppliedOrder$long[changed],
                                            newValue = newValue[changed], oldValue = oldValue[changed]))
      if (!is(updates, "try-error"))
        messageDF(updates, verbose = verbose)
    }

    keepObjNames <- setdiff(namesPoss, names(optionsVals))
  }

  finished <- TRUE
  return(list(options = optionsValsLong, # optionsShort = optionsVals,
              optionsAsProvided = optionsVals,
              optsPrev = optsPrev, keepObjNames = keepObjNames))
}

elapsedTimeInSimInit <- function(._startClockTime, sim) {
  elapsed <- difftime(Sys.time(), ._startClockTime, units = "sec")
  #if (is.null(sim@.xData[["._simInitElapsedTime"]])) {
    sim@.xData[["._simInitElapsedTime"]] <- elapsed
  #} else {
  #  sim@.xData[["._simInitElapsedTime"]] <- sim@.xData[["._simInitElapsedTime"]] + elapsed
  #}
  sim
}

warningSplitOnColon <- function(w) {
  mess <- strsplit(w$message, "\\): ")[[1]]
  len <- length(mess)
  mess[-len] <- paste0(mess[-len], "):")
  mess <- Map(mes = mess, vec = rep("  ", length(mess)), tim = seq_along(mess) - 1,
              function(mes, vec, tim) paste(paste(rep(vec, tim), collapse = ""), mes))
  lapply(mess, warning, call. = FALSE)
}

prefixSimInit <- " simInit:"

spaceDashDashSpace <- " -- "

simNestingSetup <- function(...) {
  prevSimEnv <- tryCatch(whereInStack(._txtSimNesting), error = function(x) character())
  if (is.environment(prevSimEnv)) {
    prevSimEnv <- get0(._txtSimNesting, envir = prevSimEnv, inherits = FALSE)
  }
  simNestingArg <- list(...)[[._txtSimNesting]]
  messageTxt <- if (is.null(simNestingArg)) "simInit" else simNestingArg
  c(prevSimEnv, messageTxt)
}

#' @importFrom cli col_green
simNestingOverride <- function(sim, mBase) {
  len <- length(sim[[._txtSimNesting]])
  ._simNestingTail <- sim[[._txtSimNesting]][len]
  numCharsMax <- max(0, getOption("spades.messagingNumCharsModule", 21) - loggingMessagePrefixLength)
  modName8Chars <- moduleNameStripped(mBase, numCharsMax)
  sim[[._txtSimNesting]][len] <- paste0(modName8Chars, ":", cli::col_green(sim@current$eventType))
  sim[[._txtSimNesting]]
}

isMacOSX <- function()
  isMac <- tolower(Sys.info()["sysname"]) == "darwin"



debugToVerbose <- function(debug) {
  debugOut <- sapply(debug, function(de)
    if (is.numeric(de) || is.logical(de)) de else !is.null(de)
  )
  debugOut[is.na(debugOut)] <- FALSE
  any(as.logical(debugOut))
}


metadataToDigest <- c("inputObjects", "outputObjects", "parameters","childModules", "loadOrder", "reqdPkgs",
                      "spatialExtent", "timeframe", "timeunit", "version")

objectsToUseUpdatesFromPrevInits <- function(sim, objectsToUse) {
  comps <- rbindlist(as.list(sim@completed)) # completed(sim)
  anyInits <- comps[["eventType"]] == "init"
  if (sum(anyInits)) {
    inits <- comps[anyInits]
    coreMods <- unlist(.pkgEnv$.coreModules)
    inits <- inits[!moduleName %in% coreMods]

    outputsFromOtherInits <- Map(mod = sim@depends@dependencies[inits[["moduleName"]]], function(mod)
      mod@outputObjects[["objectName"]]
    ) |> unlist() |> unique()
    objectsToUse <- objectsToUse[setdiff(names(objectsToUse), outputsFromOtherInits)]
  }
  objectsToUse
}
