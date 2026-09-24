# Fixes from the 2026-09-24 review, made in a sandbox where the data hosts
# were unreachable: the case a CRAN check machine is in.

# Examples and the vignette must not resolve "latest" --------------------

# Every version literal that the examples and the vignette pin must be the
# bundled release: that is the only one that resolves offline. A later bump of
# IM_BUNDLED_VERSION without updating them would send R CMD check to the
# network again, and no other test would notice.
pinned_versions <- function(text) {
  m <- regmatches(text, gregexpr('version = "([0-9.]+)"', text))
  unique(sub('version = "([0-9.]+)"', "\\1", unlist(m)))
}

test_that("examples pin the bundled release wherever they name a version", {
  src <- testthat::test_path("..", "..", "man")
  db <- if (dir.exists(src)) {
    tools::Rd_db(dir = testthat::test_path("..", ".."))
  } else {
    tools::Rd_db("icpim")
  }
  seen <- character()
  for (rd in db) {
    tmp <- withr::local_tempfile()
    tools::Rd2ex(rd, out = tmp)
    if (file.exists(tmp)) {
      seen <- c(seen, pinned_versions(readLines(tmp, warn = FALSE)))
    }
  }
  seen <- unique(seen)
  expect_gt(length(seen), 0)
  expect_equal(seen, IM_BUNDLED_VERSION)
})

test_that("the vignette pins the bundled release", {
  cands <- c(testthat::test_path("..", "..", "vignettes", "icpim.Rmd"),
             system.file("doc", "icpim.Rmd", package = "icpim"))
  path <- cands[file.exists(cands) & nzchar(cands)][1]
  skip_if(is.na(path), "vignette source not available here")
  txt <- readLines(path, warn = FALSE)
  expect_true(any(grepl('options(icpim.version = "', txt, fixed = TRUE)))
  expect_equal(pinned_versions(txt), IM_BUNDLED_VERSION)
})

# quiet reaches the version note -----------------------------------------

test_that("im_read(quiet = TRUE) silences the newest-release note", {
  local_planted_pc()
  withr::defer(reset_session_state())
  reset_session_state()
  withr::local_options(icpim.version = "latest", icpim.quiet = FALSE)
  local_mocked_bindings(im_latest_version = function() IM_BUNDLED_VERSION)
  expect_no_message(im_read("PC", quiet = TRUE))
  # The argument, not the option, decided: with it FALSE the note appears.
  reset_session_state()
  expect_message(im_read("PC", quiet = FALSE), "newest release")
})

test_that("with_quiet leaves the option alone when quiet is NULL", {
  withr::local_options(icpim.quiet = TRUE)
  expect_true(with_quiet(NULL, getOption("icpim.quiet")))
  expect_false(with_quiet(FALSE, getOption("icpim.quiet")))
  expect_true(getOption("icpim.quiet"))
})

# Cache paths ----------------------------------------------------------

test_that("a file name with a directory in it cannot leave the cache", {
  local_planted_pc()
  local_mocked_bindings(
    subprog_file = function(code, version) "../../PC_precipitation_chemistry.csv"
  )
  p <- im_local_path("PC")
  expect_equal(normalizePath(dirname(p)), normalizePath(im_cache_dir()))
  expect_true(file.exists(p))
})

# Absent versus unreachable ----------------------------------------------

test_that("im_manifest says 'not published' for a version the repository denies", {
  local_mocked_bindings(im_api_dataset = function(version = NULL) NULL)
  local_mocked_bindings(version_absent = function(version) TRUE)
  expect_warning(man <- im_manifest("99"), "not published")
  expect_equal(nrow(man), 21L)

  local_mocked_bindings(version_absent = function(version) FALSE)
  expect_warning(im_manifest("99"), "Could not read the file list")
})

# No tibble warning before the real one --------------------------------

test_that("an empty countries filter on a table without COUNTRY warns once", {
  local_planted_pc()
  # Rewrite the planted file without its COUNTRY column.
  path <- file.path(im_cache_dir(), "PC_precipitation_chemistry.csv")
  src <- readLines(path, encoding = "UTF-8")
  hdr <- strsplit(src[1], ",", fixed = TRUE)[[1]]
  i <- which(hdr == "COUNTRY")
  expect_length(i, 1L)
  out <- vapply(src, function(ln) {
    f <- strsplit(ln, ",", fixed = TRUE)[[1]]
    length(f) <- length(hdr)
    f[is.na(f)] <- ""
    paste(f[-i], collapse = ",")
  }, character(1), USE.NAMES = FALSE)
  writeLines(out, path, useBytes = TRUE)

  warns <- capture_warnings(im_read("PC", countries = "Swedenn", quiet = TRUE))
  expect_length(warns, 1L)
  expect_match(warns, "matched no rows")
})
