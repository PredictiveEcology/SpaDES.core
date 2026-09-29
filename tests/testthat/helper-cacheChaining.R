## The "restored ..." message, on one line: the console wraps it, and cli adds a prefix per line.
restoredMsgs <- function(m) {
  x <- grep("cacheChaining: restored", m, value = TRUE)
  trimws(gsub("\\s+", " ", gsub("[^[:alnum:][:space:]]*[[:space:]]*[[:alpha:]]{3}[0-9]+ [0-9:]+ \\S+\\s+:\\S+\\s+", " ", x)))
}

## The events a jump restored, in order, from its numbered list ("  2. jC init  (<cacheId>)").
restoredList <- function(m) {
  m <- unlist(strsplit(m, "\n", fixed = TRUE)) # the numbered list is one message, one line per event
  x <- grep("^\\s*[0-9]+\\. \\S+ \\S+\\s+\\([0-9a-f]+\\)\\s*$", sub("^[[:alpha:]]{3}[0-9]+ [0-9:]+ \\S+\\s+:\\S+\\s+", "", m), value = TRUE)
  trimws(sub("^\\s*[0-9]+\\. (\\S+ \\S+)\\s+\\(.*$", "\\1", x))
}
