## non-exported imports from other packages -------------------------------------

.message <- getFromNamespace(".message", "reproducible") ## envir for messages + message-funs
.quickPlotEnv <- getFromNamespace(".quickPlotEnv", "quickPlot")

#' @importFrom reproducible messageColoured
messageColoured <- reproducible::messageColoured

#' @importFrom reproducible messageCache
messageCache <- reproducible::messageCache

## TODO: why aren't these properly exported from reproducible if intended for use?
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
remapFilenames <- getFromNamespace("remapFilenames", "reproducible")

## re-exported functions --------------------------------------------------------

#' @importFrom reproducible paddedFloatToChar
#' @export
reproducible::paddedFloatToChar
