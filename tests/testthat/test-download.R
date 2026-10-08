# A failed download has four quite different causes, and the advice differs
# completely between them. Blaming the network for all of them sends a reader
# looking in the wrong place.

fetch_and_catch <- function(version) {
  tryCatch(
    fetch_file("https://example.invalid/x.csv",
               file.path(withr::local_tempdir(), "x.csv"),
               version = version),
    error = function(e) conditionMessage(e)
  )
}

test_that("a version that is not published is not blamed on the network", {
  local_mocked_bindings(
    curl_download = function(...) stop("HTTP response code said error: 404"),
    has_internet = function(...) TRUE,
    .package = "curl"
  )
  # The repository answers, so an absent version really is absent.
  local_mocked_bindings(im_api_dataset = function(version = NULL) list(doi = "x"))
  local_mocked_bindings(im_version_exists = function(version) FALSE)

  err <- fetch_and_catch("99")
  expect_match(err, "not published")
  expect_no_match(err, "no network connection")
})

test_that("an unreachable repository is not mistaken for an unpublished version", {
  # Network up, repository down: the existence check cannot tell absent from
  # unreachable, and used to tell the reader to re-pin a release that exists.
  local_mocked_bindings(
    curl_download = function(...) stop("Could not connect to server"),
    has_internet = function(...) TRUE,
    .package = "curl"
  )
  local_mocked_bindings(im_api_dataset = function(version = NULL) NULL)

  err <- fetch_and_catch("2")
  expect_match(err, "could not be reached")
  expect_no_match(err, "not published")
  expect_no_match(err, "no network connection")
})

test_that("a file missing from a release that does exist says so", {
  local_mocked_bindings(
    curl_download = function(...) stop("HTTP response code said error: 404"),
    has_internet = function(...) TRUE,
    .package = "curl"
  )
  local_mocked_bindings(im_api_dataset = function(version = NULL) list(doi = "x"))
  local_mocked_bindings(im_version_exists = function(version) TRUE)

  err <- fetch_and_catch("1")
  expect_match(err, "renamed or withdrawn")
})

test_that("a refused download says the repository may have moved its files", {
  # What the old download address answered to everything once the repository
  # moved its files in autumn 2026. The file was not missing, so "renamed or
  # withdrawn" sent the reader looking in the wrong place.
  refused <- structure(
    class = c("curl_error_http_returned_error", "curl_error", "error", "condition"),
    list(message = paste0("HTTP response code said error [doris.snd.se]:\n",
                          "The requested URL returned error: 401"),
         call = NULL)
  )
  local_mocked_bindings(
    curl_download = function(...) stop(refused),
    has_internet = function(...) TRUE,
    .package = "curl"
  )
  local_mocked_bindings(im_api_dataset = function(version = NULL) list(doi = "x"))

  # Unwrapped, since cli breaks long lines wherever the width falls.
  err <- gsub("\\s+", " ", cli::ansi_strip(fetch_and_catch("2")))
  expect_match(err, "refused the request (HTTP 401). It may have moved its files",
               fixed = TRUE)
  expect_no_match(err, "renamed or withdrawn")
  expect_identical(http_status(refused), 401L)
  expect_identical(http_status(simpleError("returned error: 401")), NA_integer_)
})

test_that("a genuine network failure still says so", {
  local_mocked_bindings(
    curl_download = function(...) stop("Could not resolve host"),
    has_internet = function(...) FALSE,
    .package = "curl"
  )
  err <- fetch_and_catch("1")
  expect_match(err, "no network connection")
})

test_that("an error page names the version asked for, not the pinned one", {
  withr::local_options(icpim.version = "1")
  local_mocked_bindings(
    curl_download = function(url, destfile, ...) {
      writeLines("<!DOCTYPE html><html>", destfile)
      destfile
    },
    .package = "curl"
  )
  expect_match(fetch_and_catch("99"), "99")
})

test_that("an empty download is refused rather than cached", {
  local_mocked_bindings(
    curl_download = function(url, destfile, ...) {
      file.create(destfile)
      destfile
    },
    .package = "curl"
  )
  err <- fetch_and_catch("1")
  expect_match(err, "empty file")
})
