################################################################################
#' Identify synonyms in a \code{simList}
#'
#' This will create active bindings from each element in the vector of names
#' to one or more new, hidden object(s) whose name(s) will be \code{paste0("._", synonym[1])}.
#' The user should not
#' need to use the new hidden object directly; rather they should be able to continue
#' using \code{sim$obj} or \code{sim$obj2}, only now those two or more object names
#' refer to the same
#' object. If the objects are removed during a \code{spades} call, at the end of the
#' event, they will be replaced. In other words, if a module deletes the object, it will
#' "come back". This may not always be desired.
#'
#' @param sim      A \code{simList} object from which to extract element(s) or
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
#' \code{objectSynonym} in the \code{simList}
#'
#' @return Active bindings in the \code{simList} so that all synonyms point to the same
#' object, e.g., they would be at \code{sim[[synonym[[1]][1]]]} and \code{sim[[synonym[[1]][2]]]},
#' each bound to \code{sim[[paste0("._", synonym[[1]][1])]]}, for a list of length one, that
#' element with a character vector of length two. See examples.
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
objectSynonyms <- function(sim, synonyms) {
  sim$objectSynonyms <- c(sim$objectSynonyms, synonyms)
  Map(syns = synonyms, #name2 = names(synonyms),
      MoreArgs = list(sim = sim), function(syns, sim) {

        hiddenVersion <- paste0("._", syns[1])
        for (nam in syns) {
          if (exists(nam, envir = sim, inherits = FALSE)) {
            sim[[hiddenVersion]] <- sim[[nam]]
            rm(list = nam, envir = sim)
          }

          makeActiveBinding(sym = nam,
                            fun = function(value){
                              if (missing(value)) {
                                if (exists(hiddenVersion, sim))
                                  get(hiddenVersion, envir = sim)
                                else
                                  NULL
                              } else {
                                assign(hiddenVersion, value, envir = sim)
                              }
                            },
                            env = sim
          )
        }
      })
  sim
}



.checkObjectSynonyms <- function(sim) {
  synonyms <- sim$objectSynonyms
  Map(syns = synonyms, #name2 = names(synonyms),
      MoreArgs = list(sim = sim), function(syns, sim) {
        out <- lapply(syns, function(nam) {
          if (!exists(nam, sim)) {
            sim <<- objectSynonyms(sim, list(syns))
          }

          out <- bindingIsActive(nam, sim)
          if (!out)
            stop(nam, " is part of an object synonym. It should not be assigned to")
        })
      })
  sim
}
