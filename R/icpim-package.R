#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
## usethis namespace: end
NULL

# Package-level state: used to emit one-time messages rather than one per call.
the <- new.env(parent = emptyenv())

# Called at load and available to tests, which need to clear the warn-once
# guards without emptying the environment: a missing flag is not FALSE, and
# `!NULL` is length zero, which errors rather than warning.
reset_session_state <- function() {
  the$warned_sodium   <- FALSE
  the$warned_flagsta  <- FALSE
  # "latest" is resolved once per session; a reset lets it be resolved again.
  the$resolved_latest <- NULL
  invisible()
}
reset_session_state()

.onLoad <- function(libname, pkgname) {
  reset_session_state()
  op <- options()
  defaults <- list(
    icpim.version = IM_DEFAULT_VERSION,
    icpim.quiet   = FALSE
  )
  toset <- !(names(defaults) %in% names(op))
  if (any(toset)) options(defaults[toset])
  invisible()
}
