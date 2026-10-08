# The default version is "latest", which is resolved against the repository.
# The suite runs against the bundled release instead, so a default `version`
# argument never goes to the network. Tests of the "latest" default set the
# option themselves and mock the repository.
withr::local_options(icpim.version = IM_BUNDLED_VERSION,
                     .local_envir = teardown_env())

# Under R CMD check - NOT_CRAN unset, as on CRAN and in the R-CMD-check
# workflow - the suite must not reach the network. skip_on_cran() only skips
# the tests that say they need it, and a test that reaches the repository by
# accident still passes, because the package falls back quietly when the
# repository cannot be answered. One did. So in that mode every
# request is refused and recorded, and test-zzz-offline.R fails, naming each
# address, if any was made. Tests that need a particular answer mock these
# functions themselves, which takes precedence for the length of that test.
offline_requests <- new.env(parent = emptyenv())
offline_requests$urls <- character()

if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  refuse <- function(url, ...) {
    offline_requests$urls <- c(offline_requests$urls, as.character(url)[1])
    stop("Network access refused: the test suite runs offline under R CMD check.",
         call. = FALSE)
  }
  local_mocked_bindings(
    curl_fetch_memory = refuse,
    curl_download     = refuse,
    has_internet      = function(...) {
      offline_requests$urls <- c(offline_requests$urls, "curl::has_internet()")
      FALSE
    },
    .package = "curl",
    .env = teardown_env()
  )
}
