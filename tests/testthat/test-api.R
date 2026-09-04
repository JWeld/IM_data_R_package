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

# Resolving "latest" -------------------------------------------------------
# The default reads the newest published release, looked up once per session.
# What it must never do is quietly read something else without saying so.

local_latest <- function(env = parent.frame()) {
  # A clean slate: no remembered resolution, the "latest" default, a cache of
  # our own, and no chatter.
  withr::defer(reset_session_state(), envir = env)
  reset_session_state()
  withr::local_options(icpim.version = "latest", icpim.quiet = TRUE,
                       icpim.cache_dir = withr::local_tempdir(.local_envir = env),
                       .local_envir = env)
}

test_that("the default follows the newest published release", {
  local_latest()
  local_mocked_bindings(im_latest_version = function() "7")
  expect_equal(im_version(), "7")
  # Every default `version` argument goes through the same resolution.
  expect_equal(im_cache_dir(), file.path(getOption("icpim.cache_dir"), "v7"))
  expect_match(im_file_url("x.csv"), "2024-180/7/data")
})

test_that("'latest' is resolved once per session, then fixed", {
  local_latest()
  calls <- 0L
  local_mocked_bindings(im_latest_version = function() { calls <<- calls + 1L; "7" })
  im_version(); im_version(); im_cache_dir()
  expect_equal(calls, 1L)
  # A newer release appearing mid-session does not move a running analysis.
  local_mocked_bindings(im_latest_version = function() "8")
  expect_equal(im_version(), "7")
  # Until the state is reset, which is what a new session is.
  reset_session_state()
  expect_equal(im_version(), "8")
})

test_that("the first resolution says which release it settled on", {
  local_latest()
  withr::local_options(icpim.quiet = FALSE)
  local_mocked_bindings(im_latest_version = function() "7")
  expect_message(im_version(), "newest release")
  expect_no_message(im_version())
})

test_that("offline, 'latest' falls back to the newest cached release", {
  local_latest()
  local_mocked_bindings(im_latest_version = function() NA_character_)
  root <- getOption("icpim.cache_dir")
  for (v in c("v1", "v3", "v10")) {
    dir.create(file.path(root, v), recursive = TRUE)
    file.create(file.path(root, v, "PC_precipitation_chemistry.csv"))
  }
  dir.create(file.path(root, "v11"))          # empty: nothing to read there
  dir.create(file.path(root, "v12.part"))     # not a version directory
  expect_equal(newest_cached_version(), "10") # numeric, not lexical
  withr::local_options(icpim.quiet = FALSE)
  expect_message(v <- im_version(), "newest release in the cache")
  expect_equal(v, "10")
})

test_that("offline with an empty cache, 'latest' falls back to the bundled release", {
  local_latest()
  local_mocked_bindings(im_latest_version = function() NA_character_)
  withr::local_options(icpim.quiet = FALSE)
  expect_message(v <- im_version(), "built against")
  expect_equal(v, IM_BUNDLED_VERSION)
  # And that decodes without a fetch or a warning.
  expect_no_warning(codes_for("substances"))
})

test_that("'latest' works when passed as an explicit version argument", {
  local_latest()
  local_mocked_bindings(im_latest_version = function() "7")
  expect_equal(resolve_version("latest"), "7")
  expect_equal(resolve_version("LATEST"), "7")
  expect_match(im_file_url("x.csv", version = "latest"), "2024-180/7/data")
  # A concrete version passes through untouched, whatever the session reads.
  expect_equal(resolve_version("1"), "1")
  expect_equal(resolve_version(2), "2")
})

test_that("an empty or missing version setting means the default", {
  local_latest()
  local_mocked_bindings(im_latest_version = function() "7")
  expect_equal(resolve_version(NULL), "7")
  expect_equal(resolve_version(""), "7")
  expect_equal(resolve_version(NA), "7")
})

test_that("im_check_version tells a session that started offline about the newer release", {
  local_latest()
  local_mocked_bindings(im_latest_version = function() NA_character_)
  expect_equal(im_version(), IM_BUNDLED_VERSION)
  local_mocked_bindings(im_latest_version = function() "9")
  expect_message(chk <- im_check_version(), "settled on")
  expect_true(chk$newer_available)
  expect_equal(chk$current, IM_BUNDLED_VERSION)
})

test_that("im_check_version is quiet about a session already reading the newest", {
  local_latest()
  local_mocked_bindings(im_latest_version = function() "7")
  chk <- im_check_version(quiet = TRUE)
  expect_false(chk$newer_available)
  expect_equal(chk$current, "7")
})

test_that("the bundled release resolves without touching the network", {
  # If this needed the repository, every offline session would warn.
  local_mocked_bindings(im_api_dataset = function(version = NULL) stop("no network"))
  expect_equal(nrow(known_subprogs(IM_BUNDLED_VERSION)), 21L)
  expect_no_warning(codes_for("substances", IM_BUNDLED_VERSION))
  expect_equal(im_dataset_info(IM_BUNDLED_VERSION)$doi, IM_BUNDLED_INFO$doi)
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

test_that("im_check_version compares the version being read with the newest", {
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
