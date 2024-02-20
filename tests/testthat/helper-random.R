## Serialising and restoring an external pointer replaces the pointer
## with one to NULL.
corrupt_pointer <- function(x) {
  unserialize(serialize(x, NULL))
}


skip_for_compilation <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("pkgload")
}


copy_directory <- function(src, as) {
  files <- dir(src, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  dir.create(as, FALSE, TRUE)
  ok <- file.copy(files, as, recursive = TRUE)
  if (!all(ok)) {
    stop("Error copying files")
  }
}
