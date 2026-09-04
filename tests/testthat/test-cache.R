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

test_that("a pinned version is read as written", {
  withr::with_options(list(icpim.version = "1"), {
    expect_equal(im_version(), "1")
  })
  withr::with_options(list(icpim.version = "2"), {
    expect_equal(im_version(), "2")
  })
  # Numbers are accepted too, and never reach a URL as "2.0".
  withr::with_options(list(icpim.version = 2), {
    expect_equal(im_version(), "2")
  })
})

test_that("file URLs are built against the pinned version", {
  url <- im_file_url("PC_precipitation_chemistry.csv", "data", version = "1")
  expect_match(url, "2024-180/1/data")
  expect_match(url, "filePath=PC_precipitation_chemistry\\.csv")
})

test_that("the DOIs are the published ones", {
  expect_equal(im_doi(IM_BUNDLED_VERSION), IM_BUNDLED_INFO$doi)
  expect_equal(im_doi(concept = TRUE), "10.5878/x6fn-gw26")
})

test_that("listing the cache shows everything clearing it would remove", {
  # im_update_codes() caches the code lists alongside the data files, so a
  # lister that sees only CSVs under-reports what is there and what will go.
  dir <- withr::local_tempdir()
  withr::local_options(icpim.cache_dir = dir)
  dir.create(file.path(dir, "v1"))
  file.create(file.path(dir, "v1", "MC_metal_chemistry_mosses.csv"))
  saveRDS(list(), file.path(dir, "v1", "_code_lists.rds"))

  listed <- im_cache_list("1")$file
  expect_true("_code_lists.rds" %in% listed)
  removed <- suppressMessages(im_cache_clear("1", confirm = FALSE))
  expect_setequal(listed, basename(removed))
})
