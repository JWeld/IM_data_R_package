# The package must work loaded but not attached: icpim::im_read() from a
# script, or from a package that imports this one. Lazy-loaded data is not
# visible to package code by bare name - it lives in the namespace's lazydata
# environment, which only library(icpim) puts within reach - so every
# reference to a bundled table goes through bundled(). Everything else in the
# suite runs attached, which is how a bare reference went unnoticed:
# icpim::im_read() failed with "object 'im_substances' not found", and
# icpim::im_manifest() with "object 'im_subprogrammes' not found", while
# R CMD check, the examples and every test passed.
#
# Taking the package off the search path, with the namespace still loaded,
# is exactly the state a script is in after `icpim::` without `library()`.

test_that("the package works loaded but not attached", {
  skip_if_not("package:icpim" %in% search(), "icpim is not attached")
  # Under devtools::test() the attached environment also holds the test
  # helpers, so keep hold of the one this test uses, and put that same
  # environment back afterwards rather than a fresh attach of the namespace.
  plant  <- local_planted_pc
  pkgenv <- as.environment("package:icpim")
  detach("package:icpim", character.only = TRUE)
  withr::defer(attach(pkgenv, name = "package:icpim", warn.conflicts = FALSE))

  # The condition under test: no bundled table is reachable by bare name
  # from inside the namespace now.
  expect_false(exists("im_flags", envir = asNamespace("icpim")))

  # Decoding draws on the substance list and the flags.
  x <- icpim::im_read_file(icpim::im_example("sample_PC.csv"))
  dec <- icpim::im_decode(x, version = IM_BUNDLED_VERSION)
  expect_equal(unique(dec$substance[dec$SUBST == "NA"]), "Sodium")
  expect_true(all(dec$stat[is.na(dec$FLAGSTA)] == "primary"))
  expect_equal(nrow(icpim::im_codes("flag")), 13L)
  expect_equal(
    icpim::im_codes("substance", "^sodium$", version = IM_BUNDLED_VERSION)$code,
    "NA"
  )

  # The manifest names subprogrammes from the bundled catalogue on both of
  # its paths: a file list read from the repository, and the fallback.
  local_mocked_bindings(im_api_dataset = function(version = NULL) {
    list(versionNumber = "2", file = data.frame(
      name = "data/MC_metal_chemistry_mosses.csv", type = "open",
      url = "https://files.example/MC.csv", sha256 = "b"
    ))
  })
  expect_identical(icpim::im_manifest("2")$name, "Metal chemistry in mosses")
  local_mocked_bindings(im_api_dataset = function(version = NULL) NULL)
  expect_warning(man <- icpim::im_manifest("99"), "bundled catalogue")
  expect_equal(nrow(man), 21L)

  # And the main entry point, end to end, from a planted cache.
  plant()
  expect_no_error(pc <- icpim::im_read("PC", quiet = TRUE))
  expect_true(all(c("substance", "stat") %in% names(pc)))
})
