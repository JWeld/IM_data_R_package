# A filter that matches nothing used to return an empty table in silence,
# which is indistinguishable from "the data genuinely has none".

# local_planted_pc() is in helper-fixtures.R.

test_that("countries accepts the ISO code and the full name alike", {
  local_planted_pc()
  # The extract is Swedish: AREA is "SE14", COUNTRY is "Sweden".
  by_iso  <- im_read("PC", countries = "SE", quiet = TRUE)
  by_name <- im_read("PC", countries = "Sweden", quiet = TRUE)
  by_case <- im_read("PC", countries = "sweden", quiet = TRUE)

  expect_gt(nrow(by_iso), 0)
  expect_equal(nrow(by_name), nrow(by_iso))
  expect_equal(nrow(by_case), nrow(by_iso))
  expect_equal(by_name$VALUE, by_iso$VALUE)
})

test_that("a filter matching nothing warns and names what is available", {
  local_planted_pc()
  expect_warning(im_read("PC", countries = "Swedenn", quiet = TRUE),
                 "`countries` matched no rows")
  expect_warning(im_read("PC", sites = "SE99", quiet = TRUE),
                 "`sites` matched no rows")
  expect_warning(im_read("PC", substances = "XYZZY", quiet = TRUE),
                 "`substances` matched no rows")
  expect_warning(im_read("PC", years = 1800, quiet = TRUE),
                 "`years` matched no rows")
})

test_that("the warning survives quiet, which governs progress not diagnostics", {
  local_planted_pc()
  expect_warning(im_read("PC", sites = "SE99", quiet = TRUE), "matched no rows")
  expect_silent(suppressWarnings(im_read("PC", sites = "SE99", quiet = TRUE)))
})

test_that("a filter that does match stays silent", {
  local_planted_pc()
  expect_no_warning(im_read("PC", sites = "SE14", quiet = TRUE))
  expect_no_warning(im_read("PC", substances = "NA", quiet = TRUE))
})

test_that("filter_rows names what was asked for and what is available", {
  x <- im_read_file(im_example("sample_PC.csv"))
  w <- tryCatch(
    filter_rows(x, rep(FALSE, nrow(x)), "substances", "XYZZY", x$SUBST),
    warning = function(w) conditionMessage(w)
  )
  expect_match(w, "matched no rows")
  expect_match(w, "XYZZY")          # what was asked for
  expect_match(w, "\\bAL\\b")       # and real codes from this table
  # The list is capped, so a long vocabulary is marked as truncated.
  expect_match(w, "\\.\\.\\.")
})

test_that("a short vocabulary is listed in full, with no truncation mark", {
  x <- im_read_file(im_example("sample_BI.csv"))
  w <- tryCatch(
    filter_rows(x, rep(FALSE, nrow(x)), "sites", "ZZ99", x$AREA),
    warning = function(w) conditionMessage(w)
  )
  expect_match(w, "CZ02|EE02")
  expect_no_match(w, "\\.\\.\\.")
})

test_that("an already-empty table does not warn again on every filter", {
  x <- im_read_file(im_example("sample_PC.csv"))[0, ]
  expect_no_warning(filter_rows(x, logical(0), "sites", "SE14", character(0)))
})

test_that("known_subprogs returns the same columns whichever branch it takes", {
  bundled <- known_subprogs("1")
  expect_setequal(names(bundled), c("subprog", "name", "file", "collection", "key"))

  local_mocked_bindings(im_manifest = function(version = im_version(), type = "data") {
    tibble::tibble(subprog = c("MC", "XX"), name = c("Moss", "new"),
                   file = c("MC_x.csv", "XX_y.csv"), type = "data", size_mb = 1)
  })
  other <- known_subprogs("99")
  expect_equal(names(other), names(bundled))
  # A subprogramme new in that release gets NA, not a missing column.
  expect_true(is.na(other$key[other$subprog == "XX"]))
  expect_equal(other$key[other$subprog == "MC"], "SUBST")
})

# Version 2 keeps DAY in every chemistry table for schema stability, even
# where it is empty throughout. Seven of the fourteen carry it empty.
with_empty_day <- function(env = parent.frame()) {
  src <- readLines(im_example("sample_PC.csv"), encoding = "UTF-8")
  hdr <- strsplit(src[1], ",", fixed = TRUE)[[1]]
  i <- which(hdr == "YYYYMM")
  ins <- function(f) append(f, "", after = i)
  body <- vapply(src[-1], function(ln) {
    f <- strsplit(ln, ",", fixed = TRUE)[[1]]
    length(f) <- length(hdr); f[is.na(f)] <- ""
    paste(ins(f), collapse = ",")
  }, character(1), USE.NAMES = FALSE)
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  writeLines(c(paste(ins(hdr) |> replace(i + 1, "DAY"), collapse = ","), body),
             path, useBytes = TRUE)
  path
}

test_that("a column retained but empty throughout reads and widens", {
  x <- im_read_file(with_empty_day())
  expect_true("DAY" %in% names(x))
  expect_true(all(is.na(x$DAY)))
  expect_type(x$DAY, "double")          # typed, not left as character

  # It joins the pivot key without splitting anything, since it is constant.
  w <- im_widen(im_decode(x))
  ref <- im_widen(im_decode(im_read_file(im_example("sample_PC.csv"))))
  expect_equal(nrow(w), nrow(ref))
  expect_true("DAY" %in% names(w))
})

test_that("the sodium count is unchanged by the extra column", {
  x <- im_read_file(with_empty_day())
  expect_equal(sum(x$SUBST == "NA", na.rm = TRUE), 60L)
})
