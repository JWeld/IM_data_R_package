# These run against the bundled extracts, so they need no network.

test_that("station codes keep their leading zeros", {
  pc <- im_read_file(im_example("sample_PC.csv"))
  expect_type(pc$SCODE, "character")
  expect_true("0176" %in% pc$SCODE)
  # The failure mode being guarded against:
  expect_false("176" %in% pc$SCODE)
})

test_that("the sodium substance code survives reading", {
  pc <- im_read_file(im_example("sample_PC.csv"), repair = TRUE)
  na_rows <- sum(!is.na(pc$SUBST) & pc$SUBST == "NA")
  expect_equal(na_rows, 60)
  # It must be the string, not a missing value.
  expect_type(pc$SUBST, "character")
  expect_false(anyNA(pc$SUBST))
})

test_that("repair = FALSE returns the file as published", {
  pc <- im_read_file(im_example("sample_PC.csv"), repair = FALSE)
  expect_equal(sum(is.na(pc$SUBST)), 60)
  expect_false(any(pc$SUBST == "NA", na.rm = TRUE))
})

test_that("empty cells become NA but codes do not", {
  pc <- im_read_file(im_example("sample_PC.csv"))
  # VALUE is numeric with genuine missings allowed
  expect_type(pc$VALUE, "double")
  # FLAGQUA is empty for most rows and should be NA there
  expect_true(anyNA(pc$FLAGQUA))
  # but never the empty string
  expect_false(any(pc$FLAGQUA == "", na.rm = TRUE))
})

test_that("YYYYMM parses, including annual values with month 00", {
  bb <- im_read_file(im_example("sample_BB.csv"))
  expect_true(all(bb$YYYYMM == "200000" | grepl("^\\d{6}$", bb$YYYYMM)))
  expect_true(all(is.na(bb$month)))          # month 00 is not a month
  expect_true(all(!is.na(bb$year)))          # but the year is real
  expect_s3_class(bb$date, "Date")
  expect_true(all(format(bb$date, "%m") == "01"))

  pc <- im_read_file(im_example("sample_PC.csv"))
  expect_true(all(pc$month %in% 1:12))
  expect_true(all(pc$year %in% 2015:2019))
})

test_that("numeric columns are numeric and LEVEL may be negative", {
  am <- im_read_file(im_example("sample_AM.csv"))
  expect_type(am$VALUE, "double")
  expect_type(am$LEVEL, "double")
  expect_true(any(am$LEVEL < 0))   # soil depths are negative
})

test_that("unknown subprogramme codes fail with a useful message", {
  expect_error(resolve_subprog("XX"), "Unknown subprogramme")
  expect_error(resolve_subprog(c("PC", "TF")), "single code")
  expect_equal(resolve_subprog("pc"), "PC")
  expect_equal(resolve_subprog("PC_precipitation_chemistry.csv"), "PC")
  expect_length(resolve_subprog("all", several.ok = TRUE), 21)
})
