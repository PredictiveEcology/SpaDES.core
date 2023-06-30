#' @importFrom reproducible paddedFloatToChar
#' @export
reproducible::paddedFloatToChar

messageColoured <- utils::getFromNamespace("messageColoured", "reproducible")
messageVerbose <- utils::getFromNamespace("messageVerbose", "Require")

getDrv <-  getFromNamespace("getDrv", "reproducible")
isWindows <-  getFromNamespace("isWindows", "reproducible")
makeRelative <- getFromNamespace("makeRelative", "reproducible")
makeAbsolute <- getFromNamespace("makeAbsolute", "reproducible")
isAbsolutePath <- getFromNamespace("isAbsolutePath", "reproducible")
isRaster <- getFromNamespace("isRaster", "reproducible")
isSpat <- getFromNamespace("isSpat", "reproducible")
layerNamesDelimiter <- getFromNamespace("layerNamesDelimiter", "reproducible")
