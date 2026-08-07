#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
## usethis namespace: end
NULL

# Package-level state: used to emit one-time messages rather than one per call.
the <- new.env(parent = emptyenv())
the$warned_sodium <- FALSE
the$warned_flagsta <- FALSE

.onLoad <- function(libname, pkgname) {
  op <- options()
  defaults <- list(
    icpim.version = "1",
    icpim.quiet   = FALSE
  )
  toset <- !(names(defaults) %in% names(op))
  if (any(toset)) options(defaults[toset])
  invisible()
}
