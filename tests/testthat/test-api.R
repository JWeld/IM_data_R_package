# Offline behaviour -------------------------------------------------------
# The package must stay usable without a network, falling back to what it
# ships with rather than failing.

test_that("the bundled catalogue holds only version-stable columns", {
  expect_setequal(
    names(im_subprogrammes),
    c("subprog", "name", "file", "collection", "key")
  )
  # Anything that changes between releases must not be recorded here.
  expect_false(any(c("n_rows", "n_sites", "size_mb", "first_year", "last_year")
                   %in% names(im_subprogrammes)))
})

test_that("the pinned version is the one the bundled tables came from", {
  # If these diverge, the bundled lookups silently describe a different
  # release from the one being read.
  expect_equal(im_version(), IM_BUNDLED_VERSION)
})

test_that("subprogrammes resolve from the bundled catalogue without network", {
  expect_equal(nrow(known_subprogs("1")), 21L)
  expect_equal(subprog_file("PC", "1"), "PC_precipitation_chemistry.csv")
  expect_equal(subprog_file("MC", "1"), "MC_metal_chemistry_mosses.csv")
})

test_that("code lists come from the bundle for the version it was built from", {
  expect_identical(codes_for("substances", IM_BUNDLED_VERSION), im_substances)
  expect_identical(codes_for("parameters", IM_BUNDLED_VERSION), im_parameters)
  expect_identical(codes_for("sites", IM_BUNDLED_VERSION), im_sites)
})

test_that("an unreachable repository falls back rather than failing", {
  # Point the API at a host that cannot answer.
  withr::local_options(icpim.version = "99")
  local_mocked_bindings(im_api_dataset = function(version = NULL) NULL)
  expect_warning(man <- im_manifest("99"), "bundled catalogue")
  expect_equal(nrow(man), 21L)
  # And subprogramme resolution still works.
  expect_equal(nrow(known_subprogs("99")), 21L)
})

test_that("falling back to another release's code lists is not silent", {
  withr::local_options(icpim.cache_dir = withr::local_tempdir())
  local_mocked_bindings(im_update_codes = function(...) invisible(NULL))
  # Clear the once-per-session guards so the warning can fire. Resetting is
  # not the same as emptying: an absent flag is not FALSE.
  rm(list = grep("^codes_", ls(the, all.names = TRUE), value = TRUE), envir = the)
  reset_session_state()

  expect_warning(
    out <- codes_for("substances", "99"),
    "code lists published for"
  )
  expect_identical(out, im_substances)

  # Warned once, not on every lookup.
  expect_no_warning(codes_for("parameters", "99"))
})

test_that("the bundled lookups are stamped with the release they came from", {
  # Guards the maintainer error the runtime warning cannot see: bumping
  # IM_BUNDLED_VERSION without rerunning data-raw/make_data.R would leave the
  # older release's lists in place, labelled as the newer one's.
  stamp <- attr(im_subprogrammes, "dataset_version")
  expect_false(is.null(stamp))
  expect_equal(stamp, IM_BUNDLED_VERSION)
  expect_false(is.null(attr(im_subprogrammes, "built")))
})

test_that("the bundled version needs no fetch and gives no warning", {
  expect_no_warning(codes_for("substances", IM_BUNDLED_VERSION))
  expect_identical(codes_for("substances", IM_BUNDLED_VERSION), im_substances)
})

test_that("resolve_subprog reports the codes valid for the release asked for", {
  expect_error(resolve_subprog("ZZ", version = "1"), "Unknown subprogramme")
  expect_equal(resolve_subprog("pc", version = "1"), "PC")
})

# Live repository ---------------------------------------------------------

test_that("the repository reports a version and a file list", {
  skip_on_cran()
  skip_if_offline()

  latest <- im_latest_version()
  expect_type(latest, "character")
  expect_false(is.na(latest))
  expect_true(as.numeric(latest) >= 1)

  man <- im_manifest("1", "data")
  expect_gte(nrow(man), 21L)
  expect_true(all(c("subprog", "file", "size_mb") %in% names(man)))
  expect_true(all(im_subprogrammes$subprog %in% man$subprog))
  # Sizes are real, so a stale figure cannot be reported.
  expect_true(all(man$size_mb > 0))
})

test_that("a version that does not exist is reported as absent, not as an error", {
  skip_on_cran()
  skip_if_offline()
  # The API answers 200 with a null body for these, so status alone would lie.
  expect_false(im_version_exists("99"))
  expect_true(im_version_exists("1"))
})

test_that("im_check_version compares the pinned version with the newest", {
  skip_on_cran()
  skip_if_offline()
  chk <- im_check_version(quiet = TRUE)
  expect_named(chk, c("current", "latest", "newer_available"))
  expect_equal(chk$current, im_version())
  expect_type(chk$newer_available, "logical")
})

test_that("a missing subprogramme code gets the package's own error", {
  # tolower(NA) == "all" is NA, which used to reach if() as an NA condition.
  expect_error(resolve_subprog(NA_character_, several.ok = TRUE),
               "Unknown subprogramme")
  expect_error(resolve_subprog(NA_character_), "Unknown subprogramme")
  expect_equal(resolve_subprog("all", several.ok = TRUE),
               im_subprogrammes$subprog)
})

# Robustness to the repository's answers ----------------------------------
# The API is not under this package's control, and it has form for odd
# replies: it answers 200 with a null body for versions that do not exist.
# A record missing a field must degrade, never crash.

test_that("im_check_version handles version strings as.numeric cannot", {
  withr::local_options(icpim.version = "1")

  # "2.0.1" is NA to as.numeric(); the old comparison crashed the printing
  # path on exactly the kind of version the check exists to discover.
  local_mocked_bindings(im_latest_version = function() "2.0.1")
  res <- im_check_version(quiet = TRUE)
  expect_true(res$newer_available)
  expect_message(im_check_version(quiet = FALSE), "2.0.1")

  # Unparseable strings compare FALSE rather than NA.
  local_mocked_bindings(im_latest_version = function() "not-a-version")
  res <- im_check_version(quiet = TRUE)
  expect_false(res$newer_available)
})

test_that("version_newer orders dotted versions correctly", {
  expect_true(version_newer("2.0.1", "1"))
  expect_true(version_newer("1.1", "1"))
  expect_false(version_newer("1", "2"))
  expect_false(version_newer("1", "1"))
  expect_false(version_newer("garbage", "1"))
})

test_that("a record missing fields degrades to NA rather than erroring", {
  local_mocked_bindings(
    im_api_dataset = function(version = NULL) list(versionNumber = "9")
  )

  info <- im_dataset_info("9")
  expect_identical(info$doi, NA_character_)
  expect_identical(info$version, "9")
  expect_true(is.na(info$year))
  expect_true(is.na(info$url))

  expect_warning(d <- im_doi("9"), "could not be determined")
  expect_identical(d, NA_character_)

  # The citation falls back to the concept DOI rather than printing nothing.
  txt <- paste(capture.output(im_cite("9")), collapse = " ")
  expect_match(txt, "cannot be cited")
  expect_match(txt, IM_DOI_CONCEPT, fixed = TRUE)
})

test_that("a file list without sizes still yields a manifest", {
  local_mocked_bindings(
    im_api_dataset = function(version = NULL) {
      list(versionNumber = "9",
           file = list(name = "PC_precipitation_chemistry.csv", type = "data"))
    }
  )
  man <- im_manifest("9")
  expect_equal(nrow(man), 1L)
  expect_identical(man$subprog, "PC")
  expect_true(is.na(man$size_mb))
})

test_that("a record with no file list falls back to the bundled catalogue", {
  local_mocked_bindings(
    im_api_dataset = function(version = NULL) list(versionNumber = "9")
  )
  expect_warning(man <- im_manifest("9"), "bundled catalogue")
  expect_equal(nrow(man), 21L)
})
