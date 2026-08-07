test_that("the bundled lookups are trimmed so joins actually match", {
  expect_false(any(grepl("^\\s|\\s$", im_substances$code)))
  expect_false(any(grepl("^\\s|\\s$", im_parameters$code)))
  expect_false(any(grepl("^\\s|\\s$", im_determinations$code)))
  expect_false(any(grepl("^\\s|\\s$", im_pretreatments$code)))
})

test_that("sodium is in the substance list under the code NA", {
  expect_true("NA" %in% im_substances$code)
  expect_equal(im_substances$name[im_substances$code == "NA"], "Sodium")
  expect_false(anyNA(im_substances$code))
})

test_that("decoding names the substances, sodium included", {
  pc <- im_read_file(im_example("sample_PC.csv")) |> im_decode()
  expect_true("substance" %in% names(pc))
  expect_equal(unique(pc$substance[pc$SUBST == "NA"]), "Sodium")
  expect_equal(unique(pc$substance[pc$SUBST == "SO4S"]), "Sulphate as sulphur")
  # Every code in this extract should resolve.
  expect_false(anyNA(pc$substance))
})

test_that("flags decode, and unflagged rows are primary values", {
  am <- im_read_file(im_example("sample_AM.csv")) |> im_decode()
  expect_setequal(
    unique(am$stat),
    c("mean", "minimum", "maximum", "mean of daily minima",
      "mean of daily maxima", "sum", "maximum daily sum")
  )
  pc <- im_read_file(im_example("sample_PC.csv")) |> im_decode()
  expect_true(all(pc$stat[is.na(pc$FLAGSTA)] == "primary"))
})

test_that("all ten FLAGSTA codes are documented, not just the README's five", {
  fs <- im_flags$code[im_flags$type == "FLAGSTA"]
  expect_true(all(c("X", "W", "S", "SE", "M") %in% fs))   # README
  expect_true(all(c("A", "Z", "XA", "XZ", "SZ") %in% fs)) # Manual, AM section
})

test_that("im_codes searches both code and name", {
  expect_equal(im_codes("substance", "^sodium$")$code, "NA")
  expect_true(nrow(im_codes("flag")) == 13)
})
