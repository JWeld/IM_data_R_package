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

test_that("widening works once a single statistic is selected", {
  am <- im_read_file(im_example("sample_AM.csv")) |> im_decode()
  mean_only <- filter_stat(am, "mean")
  w <- im_widen(mean_only, id_cols = c("AREA", "SCODE", "LEVEL", "YYYYMM"))
  expect_true("TEMP" %in% names(w))
  expect_equal(nrow(w), nrow(mean_only))
})

test_that("stat filtering accepts codes and readable names alike", {
  am <- im_read_file(im_example("sample_AM.csv")) |> im_decode()
  expect_equal(nrow(filter_stat(am, "X")), nrow(filter_stat(am, "mean")))
  expect_equal(nrow(filter_stat(am, "mean")), 60)
  expect_equal(nrow(filter_stat(am, "maximum")), 60)
  expect_equal(nrow(filter_stat(am, c("mean", "sum"))), 72)
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
