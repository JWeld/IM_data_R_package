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

# A version-2-style file: sodium already carries its code. Built from the real
# extract so it differs from it in exactly the one respect under test.
v2_file <- function() {
  src <- readLines(im_example("sample_PC.csv"), encoding = "UTF-8")
  hdr <- strsplit(src[1], ",", fixed = TRUE)[[1]]
  i <- which(hdr == "SUBST")
  body <- vapply(src[-1], function(ln) {
    f <- strsplit(ln, ",", fixed = TRUE)[[1]]
    # strsplit drops trailing empty fields; restore the full width or the
    # rewritten line is narrower than the header.
    length(f) <- length(hdr)
    f[is.na(f)] <- ""
    if (!nzchar(f[i])) f[i] <- "NA"
    paste(f, collapse = ",")
  }, character(1), USE.NAMES = FALSE)
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  writeLines(c(src[1], body), path, useBytes = TRUE)
  path
}

test_that("a correctly coded file passes through untouched", {
  path <- v2_file()
  # Reading it needs no repair at all.
  none <- im_read_file(path, repair = FALSE)
  expect_equal(sum(none$SUBST == "NA", na.rm = TRUE), 60)
  expect_false(anyNA(none$SUBST))
  expect_equal(attr(none, "icpim_blank_subst"), 0L)

  # And repairing is a no-op rather than a corruption.
  repaired <- im_read_file(path, repair = TRUE)
  expect_equal(repaired$SUBST, none$SUBST)
  expect_equal(nrow(repaired), nrow(none))
})

test_that("a corrected file gives the same result as a repaired v1 file", {
  v1 <- im_read_file(im_example("sample_PC.csv"), repair = TRUE)
  v2 <- im_read_file(v2_file(), repair = FALSE)
  expect_equal(v1$SUBST, v2$SUBST)
  expect_equal(v1$VALUE, v2$VALUE)
})

test_that("repair = 'auto' follows the dataset version", {
  expect_true(resolve_repair("auto", "1"))    # the affected release
  expect_false(resolve_repair("auto", "2"))   # corrected from here on
  expect_false(resolve_repair("auto", "3"))
  expect_true(resolve_repair(TRUE, "2"))      # explicit wins
  expect_false(resolve_repair(FALSE, "1"))
  expect_error(resolve_repair("yes", "1"), "must be TRUE, FALSE")
})

test_that("blank codes in an unaffected version are reported, not assumed", {
  withr::local_options(icpim.cache_dir = withr::local_tempdir(), icpim.version = "2")
  dir.create(file.path(im_cache_dir(), ""), recursive = TRUE, showWarnings = FALSE)
  # Plant a v1-style file (blank sodium) where version 2 would be looked for.
  file.copy(im_example("sample_PC.csv"),
            file.path(im_cache_dir(create = TRUE),
                      "PC_precipitation_chemistry.csv"))
  # Two distinct things are wrong here and both should be said out loud: the
  # blank codes, and that version 2's code lists are not available so version
  # 1's are being used.
  warns <- capture_warnings(out <- im_read("PC", quiet = TRUE))
  expect_match(warns, "blank substance code", all = FALSE)
  expect_match(warns, "code lists published for", all = FALSE)

  # Left missing rather than silently called sodium.
  expect_equal(sum(is.na(out$SUBST)), 60)
})

test_that("the sodium repair covers PARAM, not just SUBST", {
  # Sodium is measured in tree biomass, where the determinand column is PARAM.
  # Repairing only SUBST left these three rows missing.
  bi <- im_read_file(im_example("sample_BI.csv"), repair = TRUE)
  expect_equal(sum(bi$PARAM == "NA", na.rm = TRUE), 3L)
  expect_false(anyNA(bi$PARAM))

  named <- im_decode(bi)
  expect_equal(unique(named$parameter[named$PARAM == "NA"]), "Sodium")

  # And it is genuinely a substance-list code, as PARLIST says.
  expect_equal(unique(bi$PARLIST[bi$PARAM == "NA"]), "DB")
})

test_that("repair = FALSE leaves PARAM blanks missing too", {
  bi <- im_read_file(im_example("sample_BI.csv"), repair = FALSE)
  expect_equal(sum(is.na(bi$PARAM)), 3L)
  expect_false(any(bi$PARAM == "NA", na.rm = TRUE))
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

# The provenance record must state what was done, not what is plausible ----

test_that("sodium_corrected counts only rows actually repaired and returned", {
  cache <- withr::local_tempdir()
  withr::local_options(icpim.cache_dir = cache)
  fn <- im_subprogrammes$file[im_subprogrammes$subprog == "PC"]
  # Keep every repository lookup offline.
  local_mocked_bindings(im_api_dataset = function(version = NULL) NULL)

  # A version-2-style file: sodium already carries its code. Forcing the
  # repair must not claim credit for rows it never touched.
  dir.create(file.path(cache, "v2"))
  writeLines(
    c("COUNTRY,AREA,SUBST,VALUE,YYYYMM",
      "SE,SE14,NA,1.5,201501",
      "SE,SE14,NA,1.7,201502",
      "SE,SE14,CL,2.0,201501"),
    file.path(cache, "v2", fn)
  )
  out <- im_read("PC", version = "2", repair = TRUE, quiet = TRUE, decode = FALSE)
  expect_identical(im_provenance(out)$sodium_corrected, 0L)

  # A version-1-style file with genuine blanks: the count follows the rows
  # through filtering rather than reporting the file-level total.
  dir.create(file.path(cache, "v1"))
  writeLines(
    c("COUNTRY,AREA,SUBST,VALUE,YYYYMM",
      "SE,SE14,,1.5,201501",
      "SE,SE14,,1.7,201502",
      "SE,SE14,CL,2.0,201501"),
    file.path(cache, "v1", fn)
  )
  all_rows <- im_read("PC", version = "1", quiet = TRUE, decode = FALSE)
  expect_identical(im_provenance(all_rows)$sodium_corrected, 2L)

  only_na <- im_read("PC", version = "1", substances = "NA",
                     quiet = TRUE, decode = FALSE)
  expect_identical(im_provenance(only_na)$sodium_corrected, 2L)

  only_cl <- im_read("PC", version = "1", substances = "CL",
                     quiet = TRUE, decode = FALSE)
  expect_identical(im_provenance(only_cl)$sodium_corrected, 0L)

  # And left as published, nothing is claimed.
  untouched <- im_read("PC", version = "1", repair = FALSE,
                       quiet = TRUE, decode = FALSE)
  expect_identical(im_provenance(untouched)$sodium_corrected, 0L)
})

test_that("im_read survives a repository record with no DOI", {
  cache <- withr::local_tempdir()
  withr::local_options(icpim.cache_dir = cache)
  fn <- im_subprogrammes$file[im_subprogrammes$subprog == "PC"]
  dir.create(file.path(cache, "v9"))
  writeLines(
    c("COUNTRY,AREA,SUBST,VALUE,YYYYMM", "SE,SE14,CL,2.0,201501"),
    file.path(cache, "v9", fn)
  )
  # The record exists but carries no doi field: the read must complete, with
  # the provenance admitting the DOI is unknown rather than crashing after
  # the data was already parsed.
  local_mocked_bindings(
    im_api_dataset = function(version = NULL) list(versionNumber = "9")
  )
  out <- im_read("PC", version = "9", quiet = TRUE, decode = FALSE)
  p <- im_provenance(out)
  expect_identical(p$dataset_version, "9")
  expect_true(is.na(p$doi))
  expect_identical(p$rows_read, 1L)
})
