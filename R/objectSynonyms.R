################################################################################
#' Identify synonyms in a `simList`
#'
#' This will create active bindings amongst the synonyms. To minimize copying,
#' the first one that exists in the character vector will become the "canonical"
#' object. All others named in the character vector will be `activeBindings` to
#' that canonical one.  This synonym list will be assigned to the `envir`,
#' as an object named `objectSynonyms`. That object will have an attribute
#' called, `bindings` indicating which one is the canonical one and which
#' is/are the `activeBindings.` EXPERIMENTAL: If the objects are removed during a
#' `spades` call by, say, a module, then at the end of the event, the
#' `spades` call will replace the bindings. In other words, if a module
#' deletes the object, it will "come back". This may not always be desired.
#'
#' @param envir  An environment, which in the context of SpaDES.core is usually
#'               a `simList` to find and/or place the `objectSynonyms`
#'               object.
#' @param synonyms A list of synonym character vectors, such as
#' `list(c("age", "ageMap", "age2"), c("veg", "vegMap"))`
#'
#' @details
#' This is very experimental and only has minimal tests. Please report if this is
#' not working, and under what circumstances (e.g., please submit a reproducible example
#' to our issues tracker)
#'
#' This function will append any new `objectSynonym` to any pre-existing
#' `objectSynonym` in the `envir`. Similarly, this function assumes
#' transitivity, i.e., if `age` and `ageMap` are synonyms, and `ageMap` and `timeSinceFire`
#' are synonyms, then `age` and `timeSinceFire` must be synonyms.
#'
#' @return Active bindings in the `envir` so that all synonyms point to the same
#' canonical object, e.g., they would be at `envir[[synonym[[1]][1]]]` and
#' `envir[[synonym[[1]][2]]]`, if a list of length one is passed into
#' `synonyms`, with a character vector of length two. See examples.
#'
#' @export
#' @include simList-class.R
#'
#' @examples
#' sim <- simInit()
#'
#' sim$age <- 1:10;
#' sim <- objectSynonyms(sim, list(c("age", "ageMap")))
#'
#' identical(sim$ageMap, sim$age)
#' sim$age <- 4
#' identical(sim$ageMap, sim$age)
#' sim$ageMap <- 2:5
#' sim$ageMap[3] <- 11
#' identical(sim$ageMap, sim$age)
#'
#' # Also works to pass it in as an object
#' objectSynonyms <- list(c("age", "ageMap"))
#' sim <- simInit(objects = list(objectSynonyms = objectSynonyms))
#' identical(sim$ageMap, sim$age) # they are NULL at this point
#' sim$age <- 1:10
#' identical(sim$ageMap, sim$age) # they are not NULL at this point
#'
#' ## More complicated, with 'updating' i.e., you can add new synonyms to previous
#' sim <- simInit()
#' os <- list(c("age", "ageMap"), c("vegMap", "veg"), c("studyArea", "studyArea2"))
#' os2 <- list(c("ageMap", "timeSinceFire", "tsf"),
#'             c("systime", "systime2"),
#'             c("vegMap", "veg"))
#' sim <- objectSynonyms(sim, os)
#' sim <- objectSynonyms(sim, os2)
#'
#' # check
#' sim$objectSynonyms
#'
#'
objectSynonyms <- function(envir, synonyms) {

  # First, this may be an overwrite of an existing set of synonyms.
  #  If already in the envir$objectSynonyms, then remove it first
  if (exists("objectSynonyms", envir = envir, inherits = FALSE)) {

    for (syns in seq_along(synonyms)) {
      for(cur in envir$objectSynonyms) {
        if (any(cur %in% synonyms[[syns]])) {
          synonyms[[syns]] <- unique(c(cur, synonyms[[syns]]))
          whSyn <- unlist(lapply(envir$objectSynonyms, identical, cur))
          attrs <- attr(envir$objectSynonyms, "bindings")
          envir$objectSynonyms <- envir$objectSynonyms[!whSyn]
          attr(envir$objectSynonyms, "bindings") <- attrs[!whSyn]
        }
      }

    }
  }
  canonicalVersions <- Map(syns = synonyms, #name2 = names(synonyms),
      MoreArgs = list(envir = envir), function(syns, envir) {

        anyExist <- unlist(lapply(syns, exists, envir = envir))
        if (sum(anyExist) > 1)
          message(paste(syns[anyExist], collapse = ", "), " already exist; using the ",
                  "first one, ", syns[anyExist][1])
        canonicalVersion <- if (sum(anyExist) > 0) {
          syns[anyExist][1]
        } else {
          syns[1]
        }

        if (!exists(canonicalVersion, envir = envir)) {
          envir[[canonicalVersion]] <- NULL
        }
        activeBindingObjects <- syns[!syns %in% canonicalVersion]
        for (nam in activeBindingObjects) {
          if (exists(nam, envir = envir, inherits = FALSE)) {
            envir[[canonicalVersion]] <- envir[[nam]]
            rm(list = nam, envir = envir)
          }

          makeActiveBinding(sym = nam,
                            fun = function(value){
                              if (missing(value)) {
                                if (exists(canonicalVersion, envir)) {
                                  get(canonicalVersion, envir = envir, inherits = FALSE)
                                } else {
                                  warning("Canonical object does not exist")
                                  NULL
                                }
                              } else {
                                assign(canonicalVersion, value, envir = envir)
                              }
                            },
                            env = envir
          )
        }
        list(canonicalVersion = canonicalVersion,
             activeBindingObjects = activeBindingObjects)
      })
  attrs <- attr(envir$objectSynonyms, "bindings")
  envir$objectSynonyms <- append(envir$objectSynonyms, synonyms)
  attr(envir$objectSynonyms, "bindings") <- append(attrs, canonicalVersions)
  envir
}



.checkObjectSynonyms <- function(envir) {

  bindings <- attr(envir$objectSynonyms, "bindings")

  # It may be passed in as a list with no attributes
  if (is.null(bindings)) {
    envir <- objectSynonyms(envir, envir$objectSynonyms)
    bindings <- attr(envir$objectSynonyms, "bindings")
  }

  Map(syns = envir$objectSynonyms, bindings = bindings, #name2 = names(synonyms),
      MoreArgs = list(envir = envir), function(syns, bindings, envir) {
        if (!exists(bindings$canonicalVersion, envir)) {
          envir <<- objectSynonyms(envir, list(syns))
        }

        out <- lapply(bindings$activeBindingObjects, function(nam) {
          out <- bindingIsActive(nam, envir)
          if (!out)
            stop(nam, " is part of an object synonym. It should not be assigned to")
          out
        })
        out
      })
  envir
}
