# Each release has its own DOI, so citing must follow the pinned version.
# Reporting version 1's DOI while reading another release is the specific
# failure these guard against.

test_that("version 1 resolves offline from the bundled constants", {
  withr::local_options(icpim.version = "1")
  expect_equal(im_doi(), "10.5878/z376-2m63")
  info <- im_dataset_info()
  expect_equal(info$version, "1")
  expect_equal(info$year, "2026")
  expect_equal(info$url, "https://doi.org/10.5878/z376-2m63")
})

test_that("an unresolvable version yields NA, never another version's DOI", {
  withr::local_options(icpim.version = "99")
  local_mocked_bindings(im_api_dataset = function(version = NULL) NULL)

  expect_warning(doi <- im_doi(), "could not be determined")
  expect_true(is.na(doi))
  expect_false(identical(doi, "10.5878/z376-2m63"))

  info <- im_dataset_info()
  expect_true(is.na(info$doi))
  expect_true(is.na(info$url))
  expect_equal(info$version, "99")
})

test_that("citing an unresolvable version says so rather than citing v1", {
  withr::local_options(icpim.version = "99")
  local_mocked_bindings(im_api_dataset = function(version = NULL) NULL)

  txt <- capture.output(cite <- im_cite())
  expect_match(paste(txt, collapse = " "), "Cannot cite version 99")
  # The wrong DOI must not appear anywhere in the output.
  expect_false(any(grepl("z376-2m63", txt, fixed = TRUE)))
})

test_that("the citation names the version being read", {
  withr::local_options(icpim.version = "1")
  txt <- capture.output(im_cite())
  expect_match(paste(txt, collapse = " "), "version 1")
  expect_match(paste(txt, collapse = " "), "10.5878/z376-2m63", fixed = TRUE)
})

test_that("the concept DOI does not depend on the pinned version", {
  withr::local_options(icpim.version = "1")
  a <- im_doi(concept = TRUE)
  withr::local_options(icpim.version = "99")
  expect_equal(im_doi(concept = TRUE), a)
  expect_equal(a, "10.5878/x6fn-gw26")
})

test_that("metadata pulled from the API agrees with the bundled constants", {
  skip_on_cran()
  skip_if_offline()
  # Force version 1 down the API path and compare with what we ship.
  local_mocked_bindings(IM_BUNDLED_VERSION = "0")
  info <- im_dataset_info("1")
  expect_equal(info$doi, "10.5878/z376-2m63")
  expect_equal(info$version, "1")
  expect_equal(info$year, "2026")
  expect_equal(info$publisher, "Swedish University of Agricultural Sciences")
})
