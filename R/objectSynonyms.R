################################################################################
#' Identify synonyms in a \code{simList}
#'
#' This will create active bindings amongs the synonyms. To minimize copying,
#' the first one that exists in the character vector will become the "canonical"
#' object. All others named in the character vector will be activeBindings to that
#' canonical one.  This synonym list will be assigned to the \code{envir}, as an
#' object named \code{objectSynonyms}. That object will have an attribute called,
#' \code{bindings} indicating which one is the canonical one and which is/are the
#' activeBindings. If the objects are removed during a \code{spades} call by, say,
#' a module, then at the end of the
#' event, the \code{spades} call will replace the bindings.
#' In other words, if a module deletes the object, it will
#' "come back". This may not always be desired.
#'
#' @param envir      A \code{simList} object from which to extract element(s) or
#'                 in which to replace element(s).
#' @param synonyms A list of synonym vectors,
#' \code{list(c("age", "ageMap", "age2"), c("veg", "vegMap"))}
#'
#' @details
#' This is very experimental and only has minimal tests. Please report if this is
#' not working, and under what circumstances (e.g., please submit a reproducible example
#' to our issues tracker)
#'
#' This function will append any new \code{objectSynonym} to any pre-existing
#' \code{objectSynonym} in the \code{envir}
#'
#' @return Active bindings in the \code{envir} so that all synonyms point to the same
#' canonical object, e.g., they would be at \code{envir[[synonym[[1]][1]]]} and
#' \code{envir[[synonym[[1]][2]]]}, if a list of length one is passed into
#' \code{synonyms}, with a character vector of length two. See examples.
#'
#' @export
#' @include simList-class.R
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
objectSynonyms <- function(envir, synonyms) {
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
                                if (exists(canonicalVersion, envir))
                                  get(canonicalVersion, envir = envir)
                                else
                                  NULL
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
  attr(envir$objectSynonyms, "bindings") <- append(attrs,canonicalVersions)
  envir
}



.checkObjectSynonyms <- function(envir) {
  synonyms <- envir$objectSynonyms
  Map(syns = synonyms, bindings = attr(synonyms, "bindings"), #name2 = names(synonyms),
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
