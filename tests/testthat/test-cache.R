test_that("the cache path is version-keyed and overridable", {
  withr::with_options(list(icpim.cache_dir = "/tmp/icpim-test"), {
    expect_equal(im_cache_dir("1", create = FALSE), "/tmp/icpim-test/v1")
    expect_equal(im_cache_dir("2", create = FALSE), "/tmp/icpim-test/v2")
  })
})

test_that("the default cache sits under R_user_dir, as CRAN requires", {
  withr::with_options(list(icpim.cache_dir = NULL), {
    withr::with_envvar(list(ICPIM_CACHE_DIR = ""), {
      expect_true(startsWith(
        im_cache_dir(create = FALSE),
        tools::R_user_dir("icpim", "cache")
      ))
    })
  })
})

test_that("listing an empty cache gives zero rows, not an error", {
  withr::with_options(list(icpim.cache_dir = tempfile()), {
    out <- im_cache_list()
    expect_s3_class(out, "tbl_df")
    expect_equal(nrow(out), 0L)
  })
})

test_that("version pinning is explicit and reported", {
  expect_equal(im_version(), "1")
  withr::with_options(list(icpim.version = "2"), {
    expect_equal(im_version(), "2")
  })
})

test_that("file URLs are built against the pinned version", {
  url <- im_file_url("PC_precipitation_chemistry.csv", "data", version = "1")
  expect_match(url, "2024-180/1/data")
  expect_match(url, "filePath=PC_precipitation_chemistry\\.csv")
})

test_that("the DOIs are the published ones", {
  expect_equal(im_doi(), "10.5878/z376-2m63")
  expect_equal(im_doi(concept = TRUE), "10.5878/x6fn-gw26")
})
