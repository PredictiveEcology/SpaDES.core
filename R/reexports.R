utils::globalVariables(c(
  ".", "keepBasedOnRedundantInequalities", "inequality", "verbose", "inequality"
))

## non-exported imports from other packages -------------------------------------

.message <- getFromNamespace(".message", "reproducible") ## envir for messages + message-funs
.quickPlotEnv <- getFromNamespace(".quickPlotEnv", "quickPlot")

## TODO: why aren't these properly exported if intended for use?
## see <https://github.com/PredictiveEcology/Require/issues/105>
## and <https://github.com/PredictiveEcology/reproducible/issues/389>
compareVersion2 <- getFromNamespace("compareVersion2", "Require")
extractInequality <- getFromNamespace("extractInequality", "Require")
GETWauthThenNonAuth <- getFromNamespace("GETWauthThenNonAuth", "Require")
getGitCredsToken <- getFromNamespace("getGitCredsToken", "Require")
trimRedundancies <- getFromNamespace("trimRedundancies", "Require")

getDrv <- getFromNamespace("getDrv", "reproducible")
isWindows <- getFromNamespace("isWindows", "reproducible")
isAbsolutePath <- getFromNamespace("isAbsolutePath", "reproducible")
isRaster <- getFromNamespace("isRaster", "reproducible")
isSpat <- getFromNamespace("isSpat", "reproducible")
layerNamesDelimiter <- getFromNamespace("layerNamesDelimiter", "reproducible")
.updateTagsRepo <- getFromNamespace(".updateTagsRepo", "reproducible")
.addTagsRepo <- getFromNamespace(".addTagsRepo", "reproducible")
._prepInputsMetadata <- getFromNamespace("._prepInputsMetadata", "reproducible")
.txtNoPrefix <- getFromNamespace(".txtNoPrefix", "reproducible")

makeAbsolute <- getFromNamespace("makeAbsolute", "reproducible")

## re-exported functions --------------------------------------------------------

#' @importFrom reproducible paddedFloatToChar
#' @export
reproducible::paddedFloatToChar
