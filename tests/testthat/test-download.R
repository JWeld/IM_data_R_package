# A failed download has three quite different causes, and the advice differs
# completely between them. Blaming the network for all three sends a reader
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
  local_mocked_bindings(im_version_exists = function(version) FALSE)

  err <- fetch_and_catch("99")
  expect_match(err, "not published")
  expect_no_match(err, "no network connection")
})

test_that("a file missing from a release that does exist says so", {
  local_mocked_bindings(
    curl_download = function(...) stop("HTTP response code said error: 404"),
    has_internet = function(...) TRUE,
    .package = "curl"
  )
  local_mocked_bindings(im_version_exists = function(version) TRUE)

  err <- fetch_and_catch("1")
  expect_match(err, "renamed or withdrawn")
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
