aa <- options(spades.debug = FALSE)
options("spades.temp.debug" = aa)

cleanMessage <- function(mm) {
  mm1 <- gsub(".{1}\\[.{1,2}m", "", mm)
  mm1 <- gsub("\\n", "", mm1)
  mm1 <- gsub("\\(.*\\)", "", mm1)
  mm1 <- gsub("\\.", "", mm1)
  mm1
}
