.onLoad <- function(libname, pkgname) {
  if (requireNamespace("conflicted", quietly = TRUE)) {
    conflicted::conflict_prefer("timestamp", "utils")
  }
}