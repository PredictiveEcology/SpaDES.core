## The "restored ..." message, on one line: the console wraps it, and cli adds a prefix per line.
restoredMsgs <- function(m) {
  x <- grep("cacheChaining: restored", m, value = TRUE)
  trimws(gsub("\\s+", " ", gsub("[^[:alnum:][:space:]]*[[:space:]]*[[:alpha:]]{3}[0-9]+ [0-9:]+ \\S+\\s+:\\S+\\s+", " ", x)))
}
