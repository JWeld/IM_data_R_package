# Regressions found by reading the real files rather than the extracts.
# csv_file() and local_clear_state() are in helper-fixtures.R.

# Typing -------------------------------------------------------------------

test_that("NEEDLES keeps its letter codes", {
  # Foliage chemistry publishes the needle age class as C, P and E. Typed as
  # numeric, all 18,416 of them became NA.
  path <- csv_file(c(
    "AREA,SCODE,YYYYMM,NEEDLES,SUBST,VALUE",
    "SE14,0001,201510,C,CA,1.5",
    "SE14,0001,201510,P,CA,2.5",
    "SE14,0001,201510,,CA,3.5"
  ))
  expect_no_warning(x <- im_read_file(path))
  expect_identical(x$NEEDLES, c("C", "P", NA))

  # And it separates current-year needles from last year's when pivoting.
  w <- im_widen(x)
  expect_equal(nrow(w), 3L)
  expect_true("NEEDLES" %in% names(w))
})

test_that("a value that is not a number is reported, not silently dropped", {
  path <- csv_file(c(
    "AREA,SCODE,YYYYMM,SUBST,VALUE",
    "SE14,0001,201510,CA,1.5",
    "SE14,0001,201511,CA,<0.2",
    "SE14,0001,201512,CA,"
  ))
  # The blank is simply missing, so exactly one value is reported.
  expect_warning(x <- im_read_file(path), "1 value in VALUE",
                 class = "icpim_numeric_loss")
  expect_equal(x$VALUE, c(1.5, NA, NA))
})

# FLAGSTA nudge -------------------------------------------------------------

test_that("several statistics in a table is not several on one sample", {
  base <- tibble::tibble(
    AREA = "AT01", SCODE = "0001", YYYYMM = "200201",
    SUBST = c("PREC", "SO4S"), VALUE = c(50, 0.4), FLAGSTA = c("S", "W")
  )
  # S and W, but on different substances: nothing to warn about.
  expect_false(has_mixed_stats(base))

  # The same sample as a mean and a maximum: the case the nudge is for.
  mixed <- base[c(1, 1), ]
  mixed$FLAGSTA <- c("X", "Z")
  expect_true(has_mixed_stats(mixed))

  # An unflagged row beside a flagged one states no second statistic.
  mixed$FLAGSTA <- c("X", NA)
  expect_false(has_mixed_stats(mixed))

  expect_false(has_mixed_stats(im_read_file(im_example("sample_PC.csv"))))
  expect_true(has_mixed_stats(im_read_file(im_example("sample_AM.csv"))))
})

# API answers ---------------------------------------------------------------

test_that("a page that is not JSON is a network failure, not an absent version", {
  local_clear_state("^api_")
  local_mocked_bindings(IM_API_RETRY_AFTER = 0)
  n <- 0L
  local_mocked_bindings(
    curl_fetch_memory = function(url, ...) {
      n <<- n + 1L
      body <- if (n == 1L) "<html>Sign in to the wifi</html>"
              else "{\"dataset\":{\"versionNumber\":\"7\"}}"
      list(status_code = 200L, content = charToRaw(body))
    },
    .package = "curl"
  )
  expect_identical(im_latest_version(), NA_character_)
  # The network is back: the earlier answer must not have been remembered.
  expect_identical(im_latest_version(), "7")
})

test_that("a failed lookup is not repeated within the retry window", {
  local_clear_state("^api_")
  n <- 0L
  local_mocked_bindings(
    curl_fetch_memory = function(url, ...) { n <<- n + 1L; stop("timed out") },
    .package = "curl"
  )
  expect_null(im_api_dataset("9"))
  expect_null(im_api_dataset("9"))
  expect_equal(n, 1L)
})

test_that("only the repository's own null answer is remembered as absence", {
  local_clear_state("^api_")
  n <- 0L
  local_mocked_bindings(
    curl_fetch_memory = function(url, ...) {
      n <<- n + 1L
      list(status_code = 200L, content = charToRaw("{\"dataset\":null}"))
    },
    .package = "curl"
  )
  expect_false(im_version_exists("99"))
  expect_false(im_version_exists("99"))
  expect_equal(n, 1L)
})

# Code lists ------------------------------------------------------------------

test_that("a code list with a renamed header is not usable, and says so", {
  expect_no_warning(renamed <- build_code_tables(list(
    substances = data.frame(Code = "NA", Name = "Sodium")
  ))$substances)
  expect_false(valid_code_table(renamed, "substances"))

  expect_true(valid_code_table(im_substances, "substances"))
  expect_true(valid_code_table(im_sites, "sites"))
  # Sodium read as missing somewhere upstream.
  expect_false(valid_code_table(im_substances[im_substances$code != "NA", ],
                                "substances"))
  expect_false(valid_code_table(im_substances[0, ], "substances"))
  expect_false(valid_code_table(NULL, "substances"))
})

test_that("unusable lists are not cached, and a partial cache is completed", {
  withr::local_options(icpim.cache_dir = withr::local_tempdir(), icpim.quiet = TRUE)
  local_clear_state("^codes_")

  good <- c(
    substance_codes = "SubstanceCode,Name,Group,CASnumber,Description\nNA      ,Sodium,1,NULL,NULL\nXX9,Newium,1,NULL,NULL",
    determination_codes = "DeterminationCode,Description,NOTE\nAAS,Atomic absorption,NULL"
  )
  serve <- c("substance_codes")            # what the repository manages today
  local_mocked_bindings(
    curl_download = function(url, destfile, ...) {
      nm <- sub("\\.csv$", "", sub("^.*filePath=", "", url))
      if (nm == "pretreatment_codes") {
        writeLines("<html>Service unavailable</html>", destfile)   # 200, not CSV
      } else if (nm == "parameters_and_codes_by_subprogramme") {
        writeLines("Code,Label\nX,Y", destfile)                    # headers changed
      } else if (nm %in% serve) {
        writeLines(good[[nm]], destfile)
      } else stop("HTTP 503")
      destfile
    },
    .package = "curl"
  )

  expect_identical(codes_for("substances", "9")$name[1:2], c("Sodium", "Newium"))
  tabs <- read_code_cache("9")
  expect_named(tabs, "substances")

  # The lists that failed fall back, loudly, instead of decoding to nothing.
  expect_warning(p <- codes_for("parameters", "9"), "code lists published for")
  expect_identical(p, im_parameters)

  # A later refresh adds what it can and keeps what it had.
  serve <- "determination_codes"
  im_update_codes("9", quiet = TRUE)
  expect_setequal(names(read_code_cache("9")), c("substances", "determinations"))
  expect_false(any(grepl("\\.part-", list.files(im_cache_dir("9")))))
})

# Smaller things ------------------------------------------------------------------

test_that("halving below-detection values twice does not quarter them", {
  pc <- im_read_file(im_example("sample_PC.csv"))
  once <- im_detection_limit(pc, "half")
  expect_warning(twice <- im_detection_limit(once, "half"), "already been halved")
  expect_identical(twice$VALUE, once$VALUE)
  # The memory survives the usual row subsetting, and the other actions still
  # work on a halved table.
  expect_warning(im_detection_limit(once[once$year > 2015, ], "half"), "already")
  expect_no_warning(im_detection_limit(once, "drop"))
})

test_that("the substances filter ignores case like the other filters", {
  local_planted_pc()
  upper <- im_read("PC", substances = "SO4S", quiet = TRUE)
  expect_gt(nrow(upper), 0L)
  expect_no_warning(lower <- im_read("PC", substances = "so4s", quiet = TRUE))
  expect_identical(lower$VALUE, upper$VALUE)
  # Sodium is still asked for as the string, in either case.
  expect_gt(nrow(im_read("PC", substances = "na", quiet = TRUE)), 0L)
})
