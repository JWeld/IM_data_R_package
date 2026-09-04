# Each release has its own DOI, so citing must follow the version being read.
# Reporting the bundled release's DOI while reading another release is the
# specific failure these guard against.

test_that("the bundled release resolves offline from the bundled constants", {
  withr::local_options(icpim.version = IM_BUNDLED_VERSION)
  local_mocked_bindings(im_api_dataset = function(version = NULL) NULL)
  expect_equal(im_doi(), IM_BUNDLED_INFO$doi)
  info <- im_dataset_info()
  expect_equal(info$version, IM_BUNDLED_VERSION)
  expect_equal(info$year, "2026")
  expect_equal(info$url, paste0("https://doi.org/", IM_BUNDLED_INFO$doi))
})

test_that("the bundled constants describe version 2", {
  # The bundled lookups were rebuilt from version 2, which corrected the
  # sodium code; the citation offline must name that release and its DOI.
  expect_equal(IM_BUNDLED_VERSION, "2")
  expect_equal(IM_BUNDLED_INFO$doi, "10.5878/kdf4-n452")
  expect_equal(IM_BUNDLED_INFO$version, IM_BUNDLED_VERSION)
})

test_that("an unresolvable version yields NA, never another version's DOI", {
  withr::local_options(icpim.version = "99")
  local_mocked_bindings(im_api_dataset = function(version = NULL) NULL)

  expect_warning(doi <- im_doi(), "could not be determined")
  expect_true(is.na(doi))
  expect_false(identical(doi, IM_BUNDLED_INFO$doi))

  info <- im_dataset_info()
  expect_true(is.na(info$doi))
  expect_true(is.na(info$url))
  expect_equal(info$version, "99")
})

test_that("an unresolvable version still yields the paper citation", {
  withr::local_options(icpim.version = "99")
  local_mocked_bindings(im_api_dataset = function(version = NULL) NULL)

  txt <- capture.output(im_cite())
  one <- paste(txt, collapse = " ")
  # The paper does not depend on which release was read, so it is still given.
  expect_match(one, "Scientific Data")
  expect_match(one, "10.1038/s41597-026-07181-8", fixed = TRUE)
  # The deposit line says what is missing rather than naming another release.
  expect_match(one, "version 99 is not known")
  expect_false(any(grepl(IM_BUNDLED_INFO$doi, txt, fixed = TRUE)))
})

test_that("the data paper is given before the deposit", {
  withr::local_options(icpim.version = IM_BUNDLED_VERSION)
  txt <- capture.output(im_cite())
  paper   <- grep("Scientific Data", txt)[1]
  deposit <- grep(IM_BUNDLED_INFO$doi, txt, fixed = TRUE)[1]
  expect_true(paper < deposit)
  # And it is introduced as the thing to cite, not as an afterthought.
  expect_match(txt[1], "Cite the data paper")
})

test_that("the deposit is attributed to the programme, not the host university", {
  withr::local_options(icpim.version = IM_BUNDLED_VERSION)
  one <- paste(capture.output(im_cite()), collapse = " ")
  expect_match(one, "ICP Integrated Monitoring Programme Centre", fixed = TRUE)
  expect_no_match(one, "Swedish University of Agricultural Sciences", fixed = TRUE)
  expect_equal(im_dataset_info()$publisher,
               "ICP Integrated Monitoring Programme Centre")
})

test_that("the citation names the version being read", {
  withr::local_options(icpim.version = IM_BUNDLED_VERSION)
  txt <- capture.output(im_cite())
  expect_match(paste(txt, collapse = " "), paste("version", IM_BUNDLED_VERSION))
  expect_match(paste(txt, collapse = " "), IM_BUNDLED_INFO$doi, fixed = TRUE)
})

test_that("the concept DOI does not depend on the pinned version", {
  withr::local_options(icpim.version = IM_BUNDLED_VERSION)
  a <- im_doi(concept = TRUE)
  withr::local_options(icpim.version = "99")
  expect_equal(im_doi(concept = TRUE), a)
  expect_equal(a, "10.5878/x6fn-gw26")
})

test_that("metadata pulled from the API agrees with the bundled constants", {
  skip_on_cran()
  skip_if_offline()
  # Force the bundled release down the API path and compare with what we ship.
  bundled <- IM_BUNDLED_VERSION
  local_mocked_bindings(IM_BUNDLED_VERSION = "0")
  info <- im_dataset_info(bundled)
  expect_equal(info$doi, IM_BUNDLED_INFO$doi)
  expect_equal(info$version, IM_BUNDLED_INFO$version)
  expect_equal(info$year, IM_BUNDLED_INFO$year)
  # And version 1, the release with the blank sodium code, is still there
  # under its own DOI.
  expect_equal(im_dataset_info("1")$doi, "10.5878/z376-2m63")
  # The API names SLU as the record's principal; the citation attributes the
  # programme that produces the data, so this must not follow the API.
  expect_equal(info$publisher, "ICP Integrated Monitoring Programme Centre")
})
