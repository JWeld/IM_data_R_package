# Five failures reported against 0.1.0. None was reachable with the published
# files as they stand; each is a guard against a release whose column set or a
# session whose state differs from today's.

test_that("a missing VALUE column is named, not silently ignored", {
  x <- im_read_file(im_example("sample_PC.csv"))
  x$VALUE <- NULL
  # It used to return the table unchanged behind a bare tibble warning, which
  # looks like "there was nothing below detection".
  expect_error(im_detection_limit(x, "half"), "No VALUE column")
  expect_error(im_units({ y <- im_read_file(im_example("sample_PC.csv")); y$UNIT <- NULL; y }),
               "No UNIT column")
})

test_that("filters name the column they need when it is absent", {
  x <- im_read_file(im_example("sample_PC.csv"))
  # Without the guard the subscript is length zero and the error is
  # "Logical subscript `keep` must be size 1 or N", which names nothing useful.
  expect_error(require_col({ y <- x; y$AREA <- NULL; y }, "AREA", "sites"),
               "filters on")
  expect_error(require_col({ y <- x; y$year <- NULL; y }, "year", "years"),
               "filters on")
  expect_true(require_col(x, "AREA", "sites"))
})

test_that("the version default comes from the constant, not a literal", {
  # It used to be written in three places, one of which - im_version()'s
  # fallback - was unreachable because .onLoad always sets the option, so a
  # maintainer could edit it and see no effect. Test the reachable behaviour:
  # with the option cleared, the fallback must be the constant.
  withr::with_options(list(icpim.version = NULL), {
    expect_equal(im_version(), IM_DEFAULT_VERSION)
  })
  expect_equal(im_version(), IM_DEFAULT_VERSION)
  # And the two version constants agree with the stamp on the bundled data.
  expect_equal(IM_BUNDLED_VERSION, attr(im_subprogrammes, "dataset_version"))
})

test_that("a wiped session environment does not break the warn-once guards", {
  withr::defer(reset_session_state())
  rm(list = ls(the, all.names = TRUE), envir = the)
  # `!NULL` is length zero, which errors inside `&&` rather than warning.
  expect_no_error(im_read_file(im_example("sample_PC.csv"), quiet = TRUE))
  reset_session_state()
  expect_false(the$warned_sodium)
  expect_false(the$warned_flagsta)
})

test_that("the duplicate-key report excludes the actual value column", {
  x <- im_read_file(im_example("sample_PC.csv")) |> im_decode()
  keys <- x[, c("AREA", "year", "SUBST"), drop = FALSE]

  # Default pivot: VALUE is the value column and must not be named a culprit.
  expect_false("VALUE" %in% varying_within_duplicates(x, keys, "VALUE"))

  # Non-default pivot: SPOOL is now the value column, so it is excluded and
  # VALUE becomes an ordinary column that may legitimately be reported.
  culprits <- varying_within_duplicates(x, keys, "SPOOL")
  expect_false("SPOOL" %in% culprits)
})
