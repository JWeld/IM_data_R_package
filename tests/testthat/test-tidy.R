test_that("widening refuses to collapse mixed statistics silently", {
  am <- im_read_file(im_example("sample_AM.csv")) |> im_decode()
  # Dropping FLAGSTA from the key would average a mean with a minimum.
  expect_error(
    im_widen(am, id_cols = c("AREA", "SCODE", "LEVEL", "YYYYMM")),
    "duplicate key"
  )
  # Keeping it is unambiguous.
  expect_no_error(im_widen(am))
})

test_that("the duplicate-key error names the columns responsible", {
  pc <- im_read_file(im_example("sample_PC.csv")) |> im_decode()
  # Force a collision by keying on too little.
  err <- tryCatch(
    im_widen(pc, id_cols = c("AREA", "year")),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "duplicate key")
  expect_match(err, "% of rows")
  # It should point at what actually varies, not at the decoded companions.
  expect_match(err, "YYYYMM|month|date")
  expect_no_match(err, "\\bsubstance\\b")
})

test_that("values_fn is the documented way through a real collision", {
  pc <- im_read_file(im_example("sample_PC.csv")) |> im_decode()
  expect_error(im_widen(pc, id_cols = c("AREA", "year")), "duplicate key")
  expect_no_error(
    im_widen(pc, id_cols = c("AREA", "year"), values_fn = mean)
  )
})

test_that("widening works once a single statistic is selected", {
  am <- im_read_file(im_example("sample_AM.csv")) |> im_decode()
  mean_only <- am[am$stat == "mean", ]
  w <- im_widen(mean_only, id_cols = c("AREA", "SCODE", "LEVEL", "YYYYMM"))
  expect_true("TEMP" %in% names(w))
  expect_equal(nrow(w), nrow(mean_only))
})

test_that("the stat column is what you select on, by code or by name", {
  am <- im_read_file(im_example("sample_AM.csv")) |> im_decode()
  # Reading fetches; choosing a statistic is the reader's own step, done on
  # either the published flag or its decoded name.
  expect_equal(sum(am$FLAGSTA == "X", na.rm = TRUE), 60L)
  expect_equal(sum(am$stat == "mean"), 60L)
  expect_equal(sum(am$stat == "maximum"), 60L)
  expect_equal(sum(am$stat %in% c("mean", "sum")), 72L)
  # The two are the same rows, not merely the same count.
  expect_identical(am$VALUE[am$FLAGSTA == "X" & !is.na(am$FLAGSTA)],
                   am$VALUE[am$stat == "mean"])
})

test_that("unflagged primary values are labelled in the stat column", {
  pc <- im_read_file(im_example("sample_PC.csv")) |> im_decode()
  expect_true(all(pc$stat[is.na(pc$FLAGSTA)] == "primary"))
})

test_that("mixing statistics is what the warning is about", {
  am <- im_read_file(im_example("sample_AM.csv"))
  temp <- am[am$SUBST == "TEMP" & am$LEVEL >= 150, ]
  naive <- mean(temp$VALUE, na.rm = TRUE)
  proper <- mean(temp$VALUE[temp$FLAGSTA == "X"], na.rm = TRUE)
  expect_false(isTRUE(all.equal(naive, proper)))
})

test_that("im_units flags determinands reported in more than one unit", {
  pc <- im_read_file(im_example("sample_PC.csv"))
  u <- im_units(pc)
  expect_true(all(c("SUBST", "UNIT", "n", "n_units") %in% names(u)))
  expect_true(all(u$n_units >= 1))
})

test_that("detection-limit handling changes values the intended way", {
  pc <- im_read_file(im_example("sample_PC.csv"))
  below <- !is.na(pc$FLAGQUA) & pc$FLAGQUA == "L"
  skip_if_not(any(below), "no below-detection rows in the extract")

  halved <- im_detection_limit(pc, "half")
  expect_equal(halved$VALUE[below], pc$VALUE[below] / 2)

  dropped <- im_detection_limit(pc, "drop")
  expect_equal(nrow(dropped), sum(!below))

  nad <- im_detection_limit(pc, "na")
  expect_true(all(is.na(nad$VALUE[below])))
})
