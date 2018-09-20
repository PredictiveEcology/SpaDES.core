################################################################################
#' SpaDES time units
#'
#' \code{SpaDES} modules commonly use approximate durations that divide with no
#' remainder among themselves.
#' For example, models that simulate based on a "week" timestep, will likely
#' want to fall in lock step with a second module that is a "year" timestep.
#' Since, weeks, months, years don't really have this behaviour because of:
#' leap years, leap seconds, not quite 52 weeks in a year, months that are of
#' different duration, etc.
#' We have generated a set of units that work well together that are based on
#' the astronomical or "Julian" year.
#' In an astronomical year, leap years are added within each year with an extra
#' 1/4 day, (i.e., 1 year == 365.25 days); months are defined as year/12, and
#' weeks as year/52.
#'
#' When these units are not correct, a module developer can create their own
#' time unit using, and create a function to calculate the number of seconds
#' in that unit using the "d" prefix (for duration), following the
#' \code{lubridate} package standard:
#' \code{dfortnight <- function(x) lubridate::duration(dday(14))}.
#' Then the module developer can use "fortnight" as the module's time unit.
#'
#' @param x numeric. Number of the desired units
#'
#' @return Number of seconds within each unit
#'
#' @export
#' @rdname spadesTime
#'
#' @author Eliot McIntire
#'
setGeneric("dyears", function(x) {
  standardGeneric("dyears")
})

#' @importFrom lubridate duration
#' @export
#' @rdname spadesTime
setMethod("dyears",
          signature(x = "numeric"),
          definition = function(x) {
            duration(x * 60 * 60 * 24 * 365.25)
})

yearsInSeconds <- as.numeric(dyears(1)) # 31557600L
attributes(yearsInSeconds)$unit <- "second"

#' @inheritParams dyears
#' @export
#' @rdname spadesTime
setGeneric("dmonths", function(x) {
  standardGeneric("dmonths")
})

#' @importFrom lubridate duration
#' @rdname spadesTime
setMethod("dmonths",
          signature(x = "numeric"),
          definition = function(x) {
            duration(x * as.numeric(yearsInSeconds) / 12)
})

#' @export
#' @rdname spadesTime
setGeneric("dweeks", function(x) {
  standardGeneric("dweeks")
})

#' @export
#' @importFrom lubridate duration
#' @rdname spadesTime
setMethod("dweeks",
          signature(x = "numeric"),
          definition = function(x) {
            duration(x * as.numeric(yearsInSeconds) / 52)
})

#' @export
#' @rdname spadesTime
dweek <- function(x) {
  dweeks(x)
}

#' @export
#' @rdname spadesTime
dmonth <- function(x) {
  dmonths(x)
}

#' @export
#' @rdname spadesTime
dyear <- function(x) {
  dyears(x)
}

#' @export
#' @rdname spadesTime
#' @importFrom lubridate dseconds
dsecond <- function(x) {
  dseconds(x)
}

#' @export
#' @rdname spadesTime
#' @importFrom lubridate ddays
dday <- function(x) {
  ddays(x)
}

#' @export
#' @rdname spadesTime
#' @importFrom lubridate dhours
dhour <- function(x) {
  dhours(x)
}

#' @export
#' @rdname spadesTime
setGeneric("dNA", function(x) {
  standardGeneric("dNA")
})

#' @export
#' @importFrom lubridate duration
#' @rdname spadesTime
setMethod("dNA",
          signature(x = "ANY"),
          definition = function(x) {
            duration(0)
})

secondsInSeconds <- as.numeric(1)
hoursInSeconds <- as.numeric(dhour(1))    # 3600L
daysInSeconds <- as.numeric(dday(1))      # 86400L
weeksInSeconds <- as.numeric(dweek(1))    # 606876.92307692
monthsInSeconds <- as.numeric(dmonth(1))  # 2629800L
attributes(secondsInSeconds)$unit <- "second"
attributes(hoursInSeconds)$unit <- "second"
attributes(daysInSeconds)$unit <- "second"
attributes(weeksInSeconds)$unit <- "second"
attributes(monthsInSeconds)$unit <- "second"

################################################################################
#' Convert time units
#'
#' In addition to using the \code{lubridate} package, some additional functions
#' to work with times are provided.
#'
#' Current pre-defined units are found within the \code{spadesTimes()} function.
#' The user can define a new unit. The unit name can be anything, but the function
#' definition must be of the form \code{"dunitName"}, e.g., \code{dyear} or \code{dfortnight}.
#' The unit name is the part without the \code{d} and the function name definition includes the \code{d}.
#' This new function, e.g., \code{dfortnight <- function(x) lubridate::duration(dday(14))}
#' can be placed anywhere in the search path or in a module.
#'
#' @param unit   Character. One of the time units used in \code{SpaDES} or user
#'               defined time unit, given as the unit name only. See details.
#' @param envir   An environment. This is where to look up the function definition for
#'                the time unit. See details.
#' @param skipChecks For speed, the internal checks for classes and missingness can be skipped.
#'                   Default \code{FALSE}.
#'
#' @details Because of R scoping, if \code{envir} is a simList environment, then
#' this function will search there first, then up the current \code{search()} path.
#' Thus, it will find a user defined or module defined unit before a SpaDES unit.
#' This means that a user can override the \code{dyear} given in SpaDES, for example,
#' which is 365.25 days, with \code{dyear <- function(x) lubridate::duration(dday(365))}.
#'
#' @return A numeric vector of length 1, with \code{unit} attribute set to "seconds".
#'
#' @export
#' @author Alex Chubaty & Eliot McIntire
#' @rdname timeConversion
#'
inSeconds <- function(unit, envir, skipChecks = FALSE) {
  if (!skipChecks) {
    if (missing(envir)) envir <- .GlobalEnv
    if (missing(unit)) unit <- NA_character_
    if (is.null(unit)) unit <- NA_character_
    if (!is.character(unit)) stop("unit must be a character")
  }
  out <- switch(unit,
                second = secondsInSeconds,
                seconds = secondsInSeconds,
                hour = hoursInSeconds,
                hours = hoursInSeconds,
                day = daysInSeconds,
                days = daysInSeconds,
                week = weeksInSeconds,
                weeks = weeksInSeconds,
                month = monthsInSeconds,
                months = monthsInSeconds,
                year = yearsInSeconds,
                years = yearsInSeconds,
                # Allow for user defined time units in metadata - null is result
                #  from switch fn above if it does not appear. So search through SpaDES
                # functions first above, then check user defined units
                #attributes(out)$unit = "second"
                if(checkTimeunit(unit, envir)) {
                  as.numeric(get(paste0("d", unit), envir = envir)(1))
                  } else {
                    NULL
                  })

  return(out)
}
################################################################################
#' Convert time units
#'
#' This function takes a numeric with a "unit" attribute and converts it to
#' another numeric with a different time attribute.
#' If the units passed to argument \code{units} are the same as
#' \code{attr(time, "unit")}, then it simply returns input \code{time}.
#'
#' If \code{time} has no \code{unit} attribute, then it is assumed to be
#' seconds.
#'
#' @param time   Numeric. With a unit attribute, indicating the time unit of the
#'               input numeric. See Details.
#' @export
#' @importFrom stringi stri_detect_fixed
#' @inheritParams inSeconds
#' @include simList-class.R
#' @rdname timeConversion
#' @author Eliot McIntire
convertTimeunit <- function(time, unit, envir, skipChecks = FALSE) {
  if (!skipChecks) {
    if (missing(envir)) envir <- .GlobalEnv
    if (missing(unit)) unit <- "second"
    if (!is.character(unit)) stop("unit must be a character")
    if (!is.numeric(time)) stop("time must be a numeric")
    if (!is.environment(envir)) stop("envir must be an environment")
  }

  timeUnit <- attr(time, "unit")

  if (!skipChecks) {
    # Assume default of seconds if time has no unit
    if (!is.character(timeUnit)) {
      attr(time, "unit") <- timeUnit <- "second"
    }
    if (is.na(timeUnit)) timeUnit <- "NA" # for startsWith next
  }
  # Commenting out these two sections -- very rare for the simulation
  #  *and* the units requested to be in seconds
  # Also, NA is handled by inSeconds
  #if (!all(startsWith(c(unit, timeUnit), "second"))) {
  #  if (!is.na(timeUnit) & !is.na(unit)) {
      # confirm that units are usable by SpaDES
      #  This has been commented out, because it is too slow to check every time
      #  This should be checked at defineMetadata stage, rather than every
      #  time time(sim) is used.
      #checkTimeunit(c(timeUnit, unit), envir)

      # if timeUnit is same as unit, skip calculations
  if (is.null(timeUnit)) timeUnit <- "NA" # for next line
  if (unit != timeUnit) {

    # For bypassing calculation -- use table of knowns
    if (timeUnit != "NA") {
      if (!startsWith(timeUnit, prefix = "second")) {
        if (isTRUE(time %% 1 == 0)) {
          if (time[1] < .pkgEnv[["nUnitConversions"]]) {
            if (unit %in% .spadesTimes) {
              time <- as.numeric(.pkgEnv[["unitConversions"]][time[1] + 1L, attr(time, "unit")])
            }
          }
        }
      }
    }

    if (!is.null(attr(time, "unit"))) {
      if (startsWith(timeUnit, prefix = "second")) {
        time <- time / inSeconds(unit, envir, skipChecks = TRUE)
      } else if (startsWith(unit, prefix = "second")) {
        time <- time * inSeconds(timeUnit, envir, skipChecks = TRUE)
      } else {
        time <- time * inSeconds(timeUnit, envir, skipChecks = TRUE) /
                        inSeconds(unit, envir, skipChecks = TRUE)
      }
    }
    attr(time, "unit") <- unit

  }
  #   } else {
  #     # if timeunit is NA
  #     time <- 0
  #     attr(time, "unit") <- unit
  #   }
  # } else {
  #
  # }
  return(time)
}
################################################################################
#' Determine the largest timestep unit in a simulation
#'
#' @param sim   A \code{simList} simulation object.
#'
#' @return The timeunit as a character string. This defaults to \code{NA} if
#' none of the modules has explicit units.
#'
#' @export
#' @include simList-class.R
#' @rdname maxTimeunit
#'
#' @author Eliot McIntire and Alex Chubaty
#'
setGeneric("maxTimeunit", function(sim) {
  standardGeneric("maxTimeunit")
})

#' @export
#' @rdname maxTimeunit
setMethod(
  "maxTimeunit",
  signature(sim = "simList"),
  definition = function(sim) {
    if (length(sim@depends@dependencies)) {
      if (!is.null(sim@depends@dependencies[[1]])) {
        timesteps <- lapply(sim@depends@dependencies, function(x) {
          x@timeunit
        })
        if (!all(sapply(timesteps, is.na))) {
          return(timesteps[!is.na(timesteps)][[which.max(sapply(
            timesteps[!sapply(timesteps, is.na)], function(ts) {
              eval(parse(text = paste0("d", ts, "(1)")), envir = sim@.envir)
          }))]])
        }
      }
    }
    return(NA_character_)
})

################################################################################
#' Determine the smallest timeunit in a simulation
#'
#' When modules have different timeunit, SpaDES automatically takes the smallest
#' (e.g., "second") as the unit for a simulation.
#'
#' @param sim   A \code{simList} simulation object.
#'
#' @return The timeunit as a character string. This defaults to "second" if
#' none of the modules has explicit units.
#'
#' @export
#' @include simList-class.R
#' @rdname minTimeunit
#'
#' @author Eliot McIntire
#'
setGeneric("minTimeunit", function(sim) {
  standardGeneric("minTimeunit")
})

#' @export
#' @rdname minTimeunit
setMethod(
  "minTimeunit",
  signature(sim = "simList"),
  definition = function(sim) {
    if (length(sim@depends@dependencies)) {
      if (!is.null(sim@depends@dependencies[[1]])) {
        timesteps <- lapply(sim@depends@dependencies, function(x) {
          x@timeunit
        })
        if (!all(sapply(timesteps, is.na))) {
          return(timesteps[!is.na(timesteps)][[which.min(sapply(
            timesteps[!sapply(timesteps, is.na)], function(ts) {
              eval(parse(text = paste0("d", ts, "(1)")), envir = sim@.envir)
          }))]])
        }
      }
    }
    return("second")
})

#' @export
#' @rdname minTimeunit
setMethod(
  "minTimeunit",
  signature(sim = "list"),
  definition = function(sim) {
    keep <- sapply(sim, function(x) !is.na(x))
    if (all(!keep)) {
      keep <- rep(TRUE, length(keep))
    }
    tu <- unlist(lapply(sim[keep], function(xtime)
      as.numeric(eval(parse(text = paste0("d", xtime, "(1)"))))))

    return(sim[keep][which.min(tu)])
})

#' @rdname timeConversion
.spadesTimes <- c("year", "month", "week", "day", "hour", "second")
.spadesTimes <- c(.spadesTimes, paste0(.spadesTimes, "s"))


#' @export
#' @rdname timeConversion
spadesTimes <- function() {
  gsub(.spadesTimes, pattern = "[[:punct:]]", replacement = "")
}

#' @export
#' @rdname timeConversion
setGeneric("checkTimeunit", function(unit, envir) {
  standardGeneric("checkTimeunit")
})

#' @export
#' @rdname timeConversion
setMethod("checkTimeunit",
          signature(unit = "character", "missing"),
          definition = function(unit, envir) {
            checkTimeunit(unit, envir = .GlobalEnv)
})

#' @export
#' @rdname timeConversion
setMethod("checkTimeunit",
          signature(unit = "character", "environment"),
          definition = function(unit, envir) {
            out <- FALSE

            # check for .spadesTimes first, then user defined ones
            #   d*unit*, then d*units* then "d*unit omit s"
            if (all(!is.na(pmatch(unit, .spadesTimes)))) {
            #if (sum(str_detect(.spadesTimes, pattern = unit), na.rm = TRUE)==
            #   length(unit)) {
              out <- TRUE
            } else {
              out <- sapply(unit, function(unit) {
                if (exists(paste0("d", unit), envir = envir )) {
                  if (is.function(get(paste0("d", unit), envir = envir)))
                    out <- TRUE
                } else if (exists(paste0("d", unit, "s"), envir = envir) ) {
                  if (is.function(get(paste0("d", unit, "s"), envir = envir)))
                    out <- TRUE
                } else if (exists(gsub(x = paste0("d", unit),
                                       pattern = "s$", replacement = ""), envir = envir) ) {
                  if (is.function(get(gsub(x = paste0("d", unit),
                                          pattern = "s$", replacement = ""), envir = envir)))
                    out <- TRUE
                } else {
                  out <- FALSE
                }
              })
            }

            if (!all(out)) message("unknown timeunit provided: ", unit[!out])
            return(invisible(out))
})
