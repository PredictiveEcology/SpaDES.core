#' @importFrom reproducible paddedFloatToChar
#' @export
reproducible::paddedFloatToChar

filePathSansExt <- getFromNamespace("filePathSansExt", ns = "reproducible")
messageColoured <- utils::getFromNamespace("messageColoured", "reproducible")
messageVerbose <- utils::getFromNamespace("messageVerbose", "Require")

getDrv <- getFromNamespace("getDrv", "reproducible")
isWindows <- getFromNamespace("isWindows", "reproducible")
makeAbsolute <- getFromNamespace("makeAbsolute", "reproducible")
isAbsolutePath <- getFromNamespace("isAbsolutePath", "reproducible")
isRaster <- getFromNamespace("isRaster", "reproducible")
isSpat <- getFromNamespace("isSpat", "reproducible")
layerNamesDelimiter <- getFromNamespace("layerNamesDelimiter", "reproducible")

.quickPlotEnv <- getFromNamespace(".quickPlotEnv", "quickPlot")

fileExt <- getFromNamespace("fileExt", "reproducible")
filePathSansExt <- getFromNamespace("filePathSansExt", "reproducible")
extractInequality <- getFromNamespace("extractInequality", "Require")
compareVersion2 <- getFromNamespace("compareVersion2", "Require")

remapFilenames <- getFromNamespace("remapFilenames", "reproducible")
extractFromCache <- getFromNamespace("extractFromCache", "reproducible")
loadFile <- getFromNamespace("loadFile", "reproducible")

.messageAddingToMemoised <- getFromNamespace(".messageAddingToMemoised", "reproducible")
.messageCacheIndent <- getFromNamespace(".messageCacheIndent", "reproducible")
