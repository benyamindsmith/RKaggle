.onLoad <- function(libname, pkgname) {
  suppressMessages(
  if (requireNamespace("conflicted", quietly = TRUE)) {
    conflicted::conflict_prefer("timestamp", "utils")
  }
  )
}

