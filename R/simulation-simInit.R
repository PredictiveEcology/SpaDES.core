utils::globalVariables(c(".", "Package", "hasVersionSpec"))

#' Initialize a new simulation
#'
#' Create a new simulation object, the "sim" object. This object is implemented
#' using an \code{environment} where all objects and functions are placed.
#' Since environments in \code{R} are pass by reference, "putting" objects in
#' the sim object does no actual copy.
#' The \code{simList} also stores all parameters, and other important simulation
#' information, such as times, paths, modules, and module load order.
#' See more details below.
#'
#' \subsection{Calling this \code{simInit} function does the following:}{
#'   \tabular{lll}{
#'   \bold{What} \tab \bold{Details} \tab \bold{Argument(s) to use} \cr
#'   fills \code{simList} slots \tab places the arguments \code{times},
#'     \code{params}, \code{modules}, \code{paths} into equivalently named
#'     \code{simList} slots \tab \code{times},
#'     \code{params}, \code{modules}, \code{paths}\cr
#'   sources all module files \tab places all function definitions in the
#'     \code{simList}, specifically, into a sub-environment of the main
#'     \code{simList} environment: e.g., \code{sim$<moduleName>$function1}
#'     (see section on \bold{Scoping}) \tab \code{modules} \cr
#'   copies objects \tab from the global environment to the
#'     \code{simList} environment \tab \code{objects} \cr
#'   loads objects \tab from disk into the \code{simList} \tab \code{inputs} \cr
#'   schedule object loading/copying \tab Objects can be loaded into the
#'     \code{simList} at any time during a simulation  \tab \code{inputs} \cr
#'   schedule object saving \tab Objects can be saved to disk at any arbitrary
#'     time during the simulation. If specified here, this will be in addition
#'     to any saving due code inside a module (i.e., a module may manually
#'     run \code{write.table(...)} \tab \code{outputs} \cr
#'   schedules "init" events \tab from all modules (see \code{\link{events}})
#'        \tab automatic  \cr
#'   assesses module dependencies \tab via the inputs and outputs identified in their
#'     metadata. This gives the order of the \code{.inputObjects} and \code{init}
#'     events. This can be overridden by \code{loadOrder}. \tab automatic \cr
#'   determines time unit \tab takes time units of modules
#'       and how they fit together \tab \code{times} or automatic \cr
#'   runs \code{.inputObjects} functions \tab from every module
#'     \emph{in the module order as determined above} \tab automatic \cr
#'   }
#' }
#'
#' \code{params} can only contain updates to any parameters that are defined in
#' the metadata of modules. Take the example of a module named, \code{Fire}, which
#' has a parameter named \code{.plotInitialTime}. In the metadata of that module,
#' it says \code{TRUE}. Here we can override that default with:
#' \code{list(Fire=list(.plotInitialTime=NA))}, effectively turning off plotting.
#' Since this is a list of lists, one can override the module defaults for multiple
#' parameters from multiple modules all at once, with say:
#' \code{list(Fire = list(.plotInitialTime = NA, .plotInterval = 2),
#'            caribouModule = list(N = 1000))}.
#'
#' The \code{params} list can contain a list (named `.globals`) of named objects
#' e.g., `.globals = list(climateURL = "https:\\something.com")` entry. Any and every
#' module that has a parameter with that name (in this case `climateURL`) will be
#' overridden with this value as passed.
#'
#' \code{params} can be used to set the seed for a specific event in a module. This
#' is done using the normal \code{params} argument, specifying `.seed` as a list
#' where the elements are a numeric for the seed and the name is the event. Since
#' parameters must be specific to a module, this creates a module and event specific
#' seed e.g., \code{params = list(moduleName = list(.seed = list(init = 123)))} will
#' set the \code{init} event of module named \code{moduleName} to 123. The RN stream
#' will be reset to its state prior to the \code{set.seed} call after the event.
#'
#' We implement a discrete event simulation in a more modular fashion so it is
#' easier to add modules to the simulation. We use S4 classes and methods,
#' and fast lists to manage the event queue.
#'
#' \code{paths} specifies the location of the module source files,
#' the data input files, and the saving output files. If no paths are specified
#' the defaults are as follows:
#'
#' \itemize{
#'   \item \code{cachePath}: \code{getOption("reproducible.cachePath")};
#'
#'   \item \code{inputPath}: \code{getOption("spades.modulePath")};
#'
#'   \item \code{modulePath}: \code{getOption("spades.inputPath")};
#'
#'   \item \code{inputPath}: \code{getOption("spades.outputPath")}.
#' }
#'
#' @section Parsing and Checking Code:
#'
#' The \code{simInit} function will attempt to find usage of \code{sim$xxx} or \code{sim[['xxx']]}
#'  on either side of the assignment (\code{<-}) operator.
#' It will compare these to the module metadata, specifically \code{inputObjects} for cases where
#' objects or "gotten" from the \code{simList} and \code{outputObjects} for cases where objects are
#' assigned to the \code{simList.}
#'
#' It will also attempt to find potential, common function name conflicts with things like
#' \code{scale} and \code{stack} (both in \pkg{base} and \pkg{raster}), and
#' \code{Plot} (in \pkg{quickPlot} and some modules).
#'
#' \emph{This code checking is young and may get false positives and false negatives,
#' i.e., miss things}.
#' It also takes computational time, which may be undesirable in operational code.
#' To turn off checking (i.e., if there are too many false positives and negatives), set
#' \code{options(spades.moduleCodeChecks = FALSE)}.
#'
#' @section Caching:
#'
#' Using caching with \code{SpaDES} is vital when building re-usable and reproducible content.
#' Please see the vignette dedicated to this topic.
#'
#' @note
#' Since the objects in the \code{simList} are passed-by-reference, it is useful
#' to create a copy of the initialized \code{simList} object prior to running
#' the simulation (e.g., \code{mySimOut <- spades(Copy(mySim))}).
#' This ensures you retain access to the original objects, which would otherwise
#' be overwritten/modified during the simulation.
#'
#' @note
#' The user can opt to run a simpler \code{simInit} call without inputs, outputs, and times.
#' These can be added later with the accessor methods (See example).
#' These are not required for initializing the simulation via \code{simInit}.
#' All of \code{modules}, \code{paths}, \code{params}, and \code{objects} are needed
#' for successful initialization.
#'
#' @param times A named list of numeric simulation start and end times
#'        (e.g., \code{times = list(start = 0.0, end = 10.0, timeunit = "year")}),
#'        with the final optional element, \code{timeunit}, overriding the default
#'        time unit used in the simulation which is the "smallest time unit" across all
#'        modules. See examples.
#'
#' @param params A list of lists of the form \code{list(moduleName=list(param1=value, param2=value))}.
#' See details.
#'
#' @param modules A named list of character strings specifying the names of modules to be loaded
#' for the simulation.
#' Note: the module name should correspond to the R source file from which the module is loaded.
#' Example: a module named "caribou" will be sourced form the file \file{caribou.R},
#' located at the specified \code{modulePath(simList)} (see below).
#'
#' @param objects (optional) A vector of object names (naming objects
#'                that are in the calling environment of
#'                the \code{simInit}, which is often the
#'                \code{.GlobalEnv} unless used programmatically.
#'                NOTE: this mechanism will
#'                fail if object name is in a package dependency), or
#'                a named list of data objects to be
#'                passed into the \code{simList} (more reliable).
#'                These objects will be accessible
#'                from the \code{simList} as a normal list, e.g,. \code{mySim$obj}.
#'
#' @param paths  An optional named list with up to 4 named elements,
#' \code{modulePath}, \code{inputPath}, \code{outputPath}, and \code{cachePath}.
#' See details. NOTE: Experimental feature now allows for multiple \code{modulePath}s
#' to be specified in a character vector. The modules will be searched for sequentially
#' in the first \code{modulePath}, then if it doesn't find it, in the second etc.
#'
#' @param inputs A \code{data.frame}. Can specify from 1 to 6
#' columns with following column names: \code{objectName} (character, required),
#' \code{file} (character), \code{fun} (character), \code{package} (character),
#' \code{interval} (numeric), \code{loadTime} (numeric).
#' See \code{\link{inputs}} and vignette("ii-modules") section about inputs.
#'
#' @param outputs A \code{data.frame}. Can specify from 1 to 5
#' columns with following column names: \code{objectName} (character, required),
#' \code{file} (character), \code{fun} (character), \code{package} (character),
#' \code{saveTime} (numeric) and \code{eventPriority} (numeric). If
#' \code{eventPriority} is not set, it defaults to \code{.last()}. If \code{eventPriority}
#' is set to a low value, e.g., 0, 1, 2 and \code{saveTime} is \code{start(sim)},
#' it should give "initial conditions".
#'
#' See \code{\link{outputs}} and
#' \code{vignette("ii-modules")} section about outputs.
#'
#' @param loadOrder  An optional character vector of module names specifying the order in
#'                   which to load the modules. If not specified, the module
#'                   load order will be determined automatically.
#'
#' @param notOlderThan A time, as in from \code{Sys.time()}. This is passed into
#'                     the \code{Cache} function that wraps \code{.inputObjects}.
#'                     If the module uses the \code{.useCache} parameter and it is
#'                     set to \code{TRUE} or \code{".inputObjects"},
#'                     then the \code{.inputObjects} will be cached.
#'                     Setting \code{notOlderThan = Sys.time()} will cause the
#'                     cached versions of \code{.inputObjects} to be refreshed,
#'                     i.e., rerun.
#'
#' @return A \code{simList} simulation object, pre-initialized from values
#' specified in the arguments supplied.
#'
#' @seealso \code{\link{spades}},
#' \code{\link{times}}, \code{\link{params}}, \code{\link{objs}}, \code{\link{paths}},
#' \code{\link{modules}}, \code{\link{inputs}}, \code{\link{outputs}}
#'
#' @author Alex Chubaty and Eliot McIntire
#' @export
#' @include environment.R
#' @include module-dependencies-class.R
#' @include simList-class.R
#' @include simulation-parseModule.R
#' @include priority.R
#' @importFrom reproducible basename2
#' @importFrom utils compareVersion
#' @importFrom Require Require trimVersionNumber modifyList2
#' @importFrom utils compareVersion
#' @rdname simInit
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
#'             San Francisco, CA: No Starch Press, Inc..
#'             Retrieved from \url{https://nostarch.com/artofr.htm}
#'
#' @examples
#' \dontrun{
#' mySim <- simInit(
#'  times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'  params = list(
#'    .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'  ),
#'  modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'  paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
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
#'  paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
#' )
#' outSim <- spades(mySim)
#'
#' # A little more complicated with inputs and outputs
#' if (require(rgdal)) {
#'  mapPath <- system.file("maps", package = "quickPlot")
#'  mySim <- simInit(
#'    times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'    ),
#'    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
#'                 outputPath = tempdir()),
#'    inputs = data.frame(
#'      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
#'      functions = "raster",
#'      package = "raster",
#'      loadTime = 1,
#'      stringsAsFactors = FALSE),
#'    outputs = data.frame(
#'      expand.grid(objectName = c("caribou","landscape"),
#'      saveTime = 1:2,
#'      stringsAsFactors = FALSE))
#'  )
#'
#'  # Use accessors for inputs, outputs
#'  mySim2 <- simInit(
#'    times = list(current = 0, start = 0.0, end = 2.0, timeunit = "year"),
#'    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'    params = list(.globals = list(stackName = "landscape", burnStats = "nPixelsBurned")),
#'    paths = list(
#'      modulePath = system.file("sampleModules", package = "SpaDES.core"),
#'      outputPath = tempdir()
#'    )
#'  )
#'
#'  # add by accessor is equivalent
#'  inputs(mySim2) <- data.frame(
#'      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
#'      functions = "raster",
#'      package = "raster",
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
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
#'                 outputPath = tempdir()),
#'    inputs = data.frame(
#'      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
#'      functions = "raster",
#'      package = "raster",
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
#' }
#' }
#'
setGeneric(
  "simInit",
  function(times, params, modules, objects, paths, inputs, outputs, loadOrder,
           notOlderThan = NULL) {
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
                        notOlderThan) {

    paths <- lapply(paths, function(p)
      checkPath(p, create = TRUE)
    )

    objNames <- names(objects)
    if (length(objNames) != length(objects)) {
      stop(
        "Please pass a named list or character vector of object names whose values",
        "can be found in the parent frame of the simInit() call"
      )
    }

    # user modules
    modulesLoaded <- list()
    # create simList object for the simulation
    sim <- new("simList")
    # Make a temporary place to store parsed module files
    sim@.xData[[".parsedFiles"]] <- new.env(parent = emptyenv())
    on.exit(rm(".parsedFiles", envir = sim@.xData), add = TRUE )

    # paths
    oldGetPaths <- .paths()
    do.call(setPaths, paths)
    on.exit({
      do.call(setPaths, append(list(silent = TRUE), oldGetPaths))
    }, add = TRUE)
    paths(sim) <- paths #paths accessor does important stuff

    names(modules) <- unlist(modules)

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
      stop(childModMainFiles[!childModFilesExist], " does not exist; please create one or diagnose. ",
           "  Some possible causes:\n  ",
           "- git submodule not initiated?\n  ",
           "- the module was not created using 'newModule(...)' so is missing key files")
    }

    modules <- modules[!sapply(modules, is.null)] %>%
      lapply(., `attributes<-`, list(parsed = FALSE))

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
    reqdPkgs <- packages(modules = sim@modules,
                         filenames = file.path(names(sim@modules), paste0(mBase, ".R")),
                         paths = paths(sim)$modulePath,
                         envir = sim@.xData[[".parsedFiles"]])
    loadPkgs(reqdPkgs)

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
    # browser(expr = exists("._simInit_5"))
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
      globalsUsed <- globalsUsedInModules <- NULL
      globalsDF <- list()
      for (mod in ls(sim@params)) { # don't include the dot params; just non hidden
        common <- intersect(names(sim@params[[mod]]), names(sim@params$.globals))
        if (length(common)) {
          globalsUsed <- paste(common, sep = ", ")
          globalsUsedInModules <- rep(mod, length(common))
          globalsDF[[mod]] <- list(module = globalsUsedInModules, global = globalsUsed)
          sim@params[[mod]][common] <- sim@params$.globals[common]
        }
      }
      if (!is.null(globalsUsed)) {
        globalsDF <- rbindlist(globalsDF)
        message("The following .globals were used:")
        reproducible::messageDF(globalsDF)
      }
    }
    # From here, capture messaging and prepend it
    withCallingHandlers({

      ## add name to depends
      if (!is.null(names(sim@depends@dependencies))) {
        names(sim@depends@dependencies) <- sim@depends@dependencies %>%
          lapply(., function(x)
            x@name) %>%
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

      ## check user-supplied load order
      if (!all(length(loadOrder),
               all(sim@modules %in% loadOrder),
               all(loadOrder %in% sim@modules))) {
        loadOrder <- depsGraph(sim, plot = FALSE) %>% .depsLoadOrder(sim, .)
      }

      mBase <- basename2(unlist(sim@modules))
      loadOrderBase <- basename2(loadOrder)
      names(loadOrder) <- names(unlist(sim@modules))[na.omit(match(mBase, loadOrderBase))]
      loadOrder[] <- loadOrderBase
      loadOrderNames <- names(loadOrder)

      # This is a quick override so that .runInputObjects has access to these
      #   synonynms
      if (!is.null(objects$objectSynonyms)) {
        sim$objectSynonyms <- objects$objectSynonyms
        sim <- .checkObjectSynonyms(sim)
      }

      # Make local activeBindings to mod
      lapply(as.character(sim@modules), function(mod) {
        makeModActiveBinding(sim = sim, mod = mod)
      })

      lapply(sim@modules, function(mod) {
        makeParActiveBinding(sim = sim, mod = mod)
      })

      ## load user-defined modules
      # browser(expr = exists("._simInit_4"))

      for (m in loadOrder) {
        mFullPath <- loadOrderNames[match(m, loadOrder)]

        ## run .inputObjects() for each module
        if (is.character(getOption("spades.covr", FALSE))  ) {
          mod <- getOption("spades.covr")
          tf <- tmpfile();
          if (is.null(notOlderThan)) notOlderThan <- "NULL"
          cat(file = tf, paste0('simOut <- .runModuleInputObjects(sim, "',m,'", notOlderThan = ',notOlderThan,')'))
          # cat(file = tf, paste('spades(sim, events = ',capture.output(dput(events)),', .plotInitialTime = ', .plotInitialTime, ')', collapse = "\n"))
          unlockBinding(mod, sim$.mods)
          if (length(objects))
            list2env(objects, envir(sim))
          sim$.mods[[mod]]$sim <- sim
          aa <- covr::environment_coverage(sim$.mods[[mod]], test_files = tf)
          sim <- sim$.mods[[mod]]$sim
          rm(list = "sim", envir = sim$.mods[[mod]])
          if (is.null(.pkgEnv$._covr)) .pkgEnv$._covr <- list()
          .pkgEnv$._covr <- append(.pkgEnv$._covr, list(aa))
        } else {
          sim <- .runModuleInputObjects(sim, m, objects, notOlderThan)
        }

        ## schedule each module's init event:
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
          } else if (is.na(sim@params[[m]][[x]])) {
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
            objs(sim) <- objects
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
          ) %>%
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

    },
    message = function(m) {
      message(loggingMessage(m$message, prefix = " simInit:"))
      # message(Sys.time(), " simInit::", gsub("\\n", "", m$message))
      # This will "muffle" the original message
      tryCatch(invokeRestart("muffleMessage"), error = function(e) NULL)
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
                        notOlderThan) {
    namesMatchCall <- names(match.call())
    li <- lapply(namesMatchCall[-1], function(x) eval(parse(text = x)))
    names(li) <- namesMatchCall[-1]
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
                   notOlderThan = li$notOlderThan)

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
                        notOlderThan) {
    namesMatchCall <- names(match.call())

    li <- lapply(namesMatchCall[-1], function(x) eval(parse(text = x)))
    names(li) <- namesMatchCall[-1]
    li$modules <- as.list(modules)

    li <- .fillInSimInit(li, namesMatchCall)

    sim <- simInit(times = li$times, params = li$params,
                   modules = li$modules, objects = li$objects,
                   paths = li$paths, inputs = li$inputs,
                   outputs = li$outputs, loadOrder = li$loadOrder,
                   notOlderThan = li$notOlderThan)

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
                        notOlderThan) {
    # browser(expr = exists("._simInit_1"))
    namesMatchCall <- names(match.call())
    li <- lapply(namesMatchCall[-1], function(x) eval(parse(text = x)))
    names(li) <- namesMatchCall[-1]

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

    correctArgs <- (sapply(1:length(li), function(x) {
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
    correctArgsInner <- unlist(lapply(1:length(li), function(x) {
      # browser(expr = exists("._simInit_3"))
      if (isTRUE(is(li[[x]], "list")) &&
          isTRUE(all(expectedInnerClasses[[x]] != "ANY"))) {
        if (is(expectedInnerClasses[[x]], "list")) {
          items <- if (length(names(li[[x]])) > 0) {
            names(expectedInnerClasses[[x]])[match(names(li[[x]]),
                                        names(expectedInnerClasses[[x]]))]
          } else {
            seq(length(li[[x]]))
          }
          NAItems <- is.na(items)
          if (any(NAItems)) { # fill in unnamed list elements
            # browser(expr = exists("innerClasses"))
            items[NAItems] <- names(expectedInnerClasses[[x]][NAItems])[
              !names(expectedInnerClasses[[x]])[NAItems] %in% na.omit(items)]
            names(li[[x]])[NAItems] <- items[NAItems]

          }
          namesInner[[x]] <<- items

          all(sapply(items, function(y) {
            is(li[[x]][[y]], expectedInnerClasses[[x]][[y]])
          }))
        } else {
          if (length(li[[x]]) > 0)
            all(sapply(seq(length(li[[x]])), function(y) {
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
                   notOlderThan = li$notOlderThan)

    # sim2 <- do.call("simInit", args = li, quote = TRUE)

    return(invisible(sim))
})

#' Call \code{simInit} and \code{spades} together
#'
#' These functions are convenience wrappers that may allow for
#' more efficient Caching.
#' Passes all arguments to \code{simInit}, then passes the created \code{simList}
#' to \code{spades}.
#'
#' @param ... Arguments passed to simInit and spades
#'
#' @return Same as \code{\link{spades}} (a \code{simList}) or
#'
#'
#' @seealso \code{\link{simInit}}, \code{\link{spades}}
#'
#' @export
#' @inheritParams simInit
#' @inheritParams spades
#'
#' @aliases simInitAndSpades
#' @rdname simInitAnd
simInitAndSpades <- function(times, params, modules, objects, paths, inputs, outputs, loadOrder,
                             notOlderThan, debug, progress, cache, .plots,
                             .plotInitialTime, .saveInitialTime, events, ...) {

  # because Cache (and possibly others, we have to strip any other call wrapping simInitAndSpades)
  lsAllNames <- ls(all.names = TRUE)
  lsAllNames <- lsAllNames[lsAllNames != "..."]

  objsAll <- mget(lsAllNames, envir = environment())
  objsSimInit <- objsAll[formalArgs(simInit)]

  namesMatchCall <- names(match.call())
  objsSimInit <- .fillInSimInit(objsSimInit, namesMatchCall)

  sim <- simInit(times = objsSimInit$times, params = objsSimInit$params,
                 modules = objsSimInit$modules, objects = objsSimInit$objects,
                 paths = objsSimInit$paths, inputs = objsSimInit$inputs,
                 outputs = objsSimInit$outputs, loadOrder = objsSimInit$loadOrder,
                 notOlderThan = objsSimInit$notOlderThan)
  #sim <- do.call(simInit, objsSimInit) # serializes the objects

  spadesFormals <- formalArgs(spades)[formalArgs(spades) %in% names(objsAll)]
  ## quote is so that entire simList is not serialized in do.call
  objsSpades <- append(alist(sim = sim), objsAll[spadesFormals])
  sim <- do.call(spades, objsSpades)
}


#' Identify Child Modules from a recursive list
#'
#' There can be parents, grandparents, etc
#'
#' @rdname identifyChildModules
#' @param sim simList
#' @param modules List of module names
#' @importFrom reproducible basename2
#' @keywords internal
#' @return list of \code{modules} will flat named list of all module names (children, parents etc.) and
#'         \code{childModules} a non flat named list of only the childModule names.
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

      modulesToSearch3[isParent] <- Map(x = modulesToSearch3[isParent],
                                       nam = dirname(names(modulesToSearch3[isParent])),
                                       function(x, nam){
                                         mods <- lapply(x, function(y) file.path(nam, y))
                                         names(mods) <- unlist(lapply(mods, basename2))

                                         .identifyChildModules(sim = sim, modules = mods)}
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
#' @param modules (Nested) Named list of module names
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

#' Run module's \code{.inputObjects}
#'
#' Run \code{.inputObjects()} from each module file from each module, one at a time,
#' and remove it from the \code{simList} so next module won't rerun it.
#'
#' @keywords internal
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
  # browser(expr = exists("._runModuleInputObjects_1"))

  allObjsProvided <- sim@depends@dependencies[[i]]@inputObjects[["objectName"]] %in%
    sim$.userSuppliedObjNames
  if (!all(allObjsProvided)) {
    if (!is.null(sim@.xData$.mods[[mBase]][[".inputObjects"]])) {
      # browser(expr = exists("._runModuleInputObjects_2"))
      if (!missing(objects))
        list2env(objects[sim@depends@dependencies[[i]]@inputObjects[["objectName"]][allObjsProvided]],
                 envir = sim@.xData)
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

      message(crayon::green("Running .inputObjects for ", mBase, sep = ""))
      if (isTRUE(cacheIt)) {
        moduleSpecificInputObjects <- sim@depends@dependencies[[i]]@inputObjects[["objectName"]]
        moduleSpecificInputObjects <- na.omit(moduleSpecificInputObjects)
        moduleSpecificInputObjects <- c(moduleSpecificInputObjects, m)

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
          inSimList <- suppliedElsewhere(moduleSpecificInputObjects, sim, where = "sim")
          if (any(inSimList)) {
            objectsToEvaluateForCaching <- c(objectsToEvaluateForCaching,
                                             moduleSpecificInputObjects[inSimList])
          }

          #sim <- Cache(FUN = do.call, .inputObjects, args, # remove the do.call
          # showSimilar <- isTRUE(sim@params[[mBase]][[".showSimilar"]])
          # browser(expr = exists("._runModuleInputObjects_3"))
          showSimilar <- if (is.null(sim@params[[mBase]][[".showSimilar"]]) ||
                             isTRUE(is.na(sim@params[[mBase]][[".showSimilar"]]))) {
            isTRUE(getOption("reproducible.showSimilar", FALSE))
          } else {
            isTRUE(sim@params[[mBase]][[".showSimilar"]])
          }

          sim <- Cache(.inputObjects, sim,
                       .objects = objectsToEvaluateForCaching,
                       notOlderThan = notOlderThan,
                       outputObjects = moduleSpecificInputObjects,
                       quick = getOption("reproducible.quick", FALSE),
                       cacheRepo = sim@paths$cachePath,
                       classOptions = list(events = FALSE, current = FALSE, completed = FALSE, simtimes = FALSE,
                                            params = sim@params[[mBase]],
                                            modules = mBase),
                       showSimilar = showSimilar,
                       userTags = c(paste0("module:", mBase),
                                    "eventType:.inputObjects",
                                    "function:.inputObjects"))
        }
      } else {
        .modifySearchPath(pkgs = sim@depends@dependencies[[i]]@reqdPkgs)
        .inputObjects <- .getModuleInputObjects(sim, mBase)
        if (!is.null(.inputObjects)) {
          sim <- .inputObjects(sim)
        }
      }
    }
  } else {
    message("All required input Objects provided; skipping .inputObjects")
  }

  sim@current <- list()
  return(sim)
}

.timesDefault <- function() list(start = 0, end = 10)
.paramsDefault <- function() list()
.modulesDefault <- function() list()
.objectsDefault <- function() list()
.pathsDefault <- function() suppressMessages(.paths())
.inputsDefault <- function() as.data.frame(NULL)
.outputsDefault <- function() as.data.frame(NULL)
.loadOrderDefault <- function() character(0)
.notOlderThanDefault <- function() NULL

.fillInSimInit <- function(li, namesMatchCall) {

  isMissing <- !formalArgs(simInit) %in% namesMatchCall[-1]
  formalsTF <- formalArgs(simInit)

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
  moduleFilesPoss <- lapply(moduleDirsPoss, function(poss) file.path(file.path(poss,
                                                                               paste0(basename(poss), ".R"))))
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
    notExist <- !unlist(lapply(moduleFilesExist, any));
    notExist <- Map(poss = moduleDirsPoss[notExist], exist = moduleFilesExist[notExist],
        function(poss, exist) poss[!exist])
    stop(paste0(names(notExist), " doesn't exist in modulePath(sim): (",
                   lapply(notExist, paste, collapse = ", "), ")", collapse = "\n"))
  }
  modulePaths <- Map(poss = moduleDirsPoss, exist = moduleDirsExist, function(poss, exist)
    poss[exist][1])
}

checkSpaDES.coreMinVersion <- function(allPkgs) {
  whSC <- startsWith(allPkgs, "SpaDES.core")
  if (any(whSC)) {
    versionSpecs <- Require::getPkgVersions(allPkgs[whSC])
    sc <- versionSpecs[Package == "SpaDES.core" & hasVersionSpec == TRUE]
    if (NROW(sc)) {
      out11 <- unlist(lapply(which(sc$hasVersionSpec), function(iii) {
        comp <- compareVersion(as.character(packageVersion(sc$Package[iii])),
                               sc$versionSpec[iii])}))
      if (any(out11 < 0))
        stop("One of the modules needs a newer version of SpaDES.core. Please ",
             "restart R and install with: \n",
             "Require::Require('",sc$packageFullName[1],"')") # 1 is the highest
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
    return(list("year")) # default
  }
  minTU <- minTimeunit(as.list(unlist(tu)))
  if (isTRUE(is.na(minTU[[1]]))) {
    minTU[[1]] <- "year"
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

loadPkgs <- function(reqdPkgs) {
  uniqueReqdPkgs <- unique(unlist(reqdPkgs))

  if (length(uniqueReqdPkgs)) {
    allPkgs <- unique(c(uniqueReqdPkgs, "SpaDES.core"))

    # Check for SpaDES.core minimum version
    checkSpaDES.coreMinVersion(allPkgs)

    if (getOption("spades.useRequire")) {
      Require(allPkgs, upgrade = FALSE)
    } else {
      loadedPkgs <- search();
      neededPkgs <- uniqueReqdPkgs %in% gsub(".*:", "", loadedPkgs)
      names(neededPkgs) <- uniqueReqdPkgs
      allPkgs <- unique(c(names(neededPkgs)[!neededPkgs], "SpaDES.core"))
      message("options('spades.useRequire' = FALSE), so not checking minimum package version requirements")
      # versionSpecs <- Require::getPkgVersions(allPkgs)
      # if (any(versionSpecs$hasVersionSpec)) {
      #   out11 <- lapply(which(versionSpecs$hasVersionSpec), function(iii) {
      #     comp <- compareVersion(as.character(packageVersion(versionSpecs$Package[iii])),
      #                            versionSpecs$versionSpec[iii])
      #     if (comp < 0)
      #       warning(versionSpecs$Package[iii], " needs to be updated to at least ",
      #               versionSpecs$versionSpec[iii])
      #   })
      #
      # }
      allPkgs <- unique(Require::extractPkgName(allPkgs))
      loadedPkgs <- lapply(trimVersionNumber(allPkgs), require, character.only = TRUE)
    }
  }

}
