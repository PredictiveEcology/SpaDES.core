## non-exported imports from other packages -------------------------------------

.message <- getFromNamespace(".message", "reproducible") ## envir for messages + message-funs
.quickPlotEnv <- getFromNamespace(".quickPlotEnv", "quickPlot")

#' @importFrom reproducible messageColoured
messageColoured <- reproducible::messageColoured

#' @importFrom reproducible messageCache
messageCache <- reproducible::messageCache

## TODO: why aren't these properly exported if intended for use?
## see <https://github.com/PredictiveEcology/Require/issues/105>
## and <https://github.com/PredictiveEcology/reproducible/issues/389>
compareVersion2 <- getFromNamespace("compareVersion2", "Require")
extractFromCache <- getFromNamespace("extractFromCache", "reproducible")
extractInequality <- getFromNamespace("extractInequality", "Require")
fileExt <- getFromNamespace("fileExt", "reproducible")
filePathSansExt <- getFromNamespace("filePathSansExt", "reproducible")
getDrv <- getFromNamespace("getDrv", "reproducible")
isWindows <- getFromNamespace("isWindows", "reproducible")
isAbsolutePath <- getFromNamespace("isAbsolutePath", "reproducible")
isRaster <- getFromNamespace("isRaster", "reproducible")
isSpat <- getFromNamespace("isSpat", "reproducible")
layerNamesDelimiter <- getFromNamespace("layerNamesDelimiter", "reproducible")
loadFile <- getFromNamespace("loadFile", "reproducible")
makeAbsolute <- getFromNamespace("makeAbsolute", "reproducible")
messageVerbose <- utils::getFromNamespace("messageVerbose", "Require")

## re-exported functions --------------------------------------------------------

#' @importFrom reproducible paddedFloatToChar
#' @export
reproducible::paddedFloatToChar
