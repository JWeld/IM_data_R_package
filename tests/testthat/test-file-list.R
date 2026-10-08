# The repository's file list, and where files are fetched from.
#
# In autumn 2026 the repository changed both. The address this package built
# for each file began answering 401, and the file list began naming files with
# their folder and using `type` for the access level. Every download failed,
# im_manifest() returned an empty table without a word, and R CMD check stayed
# green throughout.

# A file list in the shape the repository has used since autumn 2026.
new_style_record <- function(sha = c("a", "b", "c")) {
  function(version = NULL) {
    list(versionNumber = "2", file = data.frame(
      name        = c("data/AC_air_chemistry.csv",
                      "data/MC_metal_chemistry_mosses.csv",
                      "documentation/substance_codes.csv"),
      contentSize = c(2106291L, 14374L, 76710L),
      type        = "open",
      url         = c("https://files.example/AC.csv",
                      "https://files.example/MC.csv",
                      "https://files.example/substance_codes.csv"),
      sha256      = sha
    ))
  }
}

test_that("the autumn 2026 file list is read, not emptied", {
  local_mocked_bindings(im_api_dataset = new_style_record())
  expect_no_warning(man <- im_manifest("2"))
  expect_identical(man$subprog, c("AC", "MC"))
  expect_identical(man$file, c("AC_air_chemistry.csv", "MC_metal_chemistry_mosses.csv"))
  expect_identical(man$type, c("data", "data"))
  expect_identical(man$url, c("https://files.example/AC.csv", "https://files.example/MC.csv"))
  expect_identical(man$sha256, c("a", "b"))
  expect_equal(man$size_mb, c(2.01, 0.01))

  doc <- im_manifest("2", "documentation")
  expect_identical(doc$file, "substance_codes.csv")
  expect_true(all(is.na(doc$subprog)))
})

test_that("a file list nothing in which can be classified is reported, not empty", {
  # The failure mode of autumn 2026, as it would look to the old parser:
  # names with no folder, and a type that is neither data nor documentation.
  local_mocked_bindings(im_api_dataset = function(version = NULL) {
    list(file = data.frame(name = "AC_air_chemistry.csv", type = "open"))
  })
  expect_warning(man <- im_manifest("2"), "not in a form this package recognises")
  expect_equal(nrow(man), 21L)
  expect_true(all(is.na(man$url)))
})

test_that("files come from the listed address, and only an https one", {
  local_mocked_bindings(im_api_dataset = new_style_record())
  man <- im_manifest("2")
  src <- file_source("MC_metal_chemistry_mosses.csv", "data", "2", man)
  expect_identical(src$url, "https://files.example/MC.csv")
  expect_identical(src$sha256, "b")

  # Not listed, or listed in the clear: the address is built by hand, and
  # there is no checksum to hold it to.
  built <- im_file_url("PC_precipitation_chemistry.csv", "data", "2")
  expect_identical(file_source("PC_precipitation_chemistry.csv", "data", "2", man),
                   list(url = built, sha256 = NA_character_))
  man$url[man$subprog == "MC"] <- "http://files.example/MC.csv"
  expect_match(file_source("MC_metal_chemistry_mosses.csv", "data", "2", man)$url,
               "^https://slu\\.open\\.care\\.snd\\.se/")
  expect_identical(file_source("x.csv", "data", "2", NULL)$url,
                   im_file_url("x.csv", "data", "2"))
})

test_that("a download that does not match its checksum is not cached", {
  skip_if(is.null(get0("sha256sum", envir = asNamespace("tools"), mode = "function")),
          "tools::sha256sum() needs R 4.5 or later")
  withr::local_options(icpim.cache_dir = withr::local_tempdir(), icpim.quiet = TRUE)
  body <- charToRaw("AREA,SUBST,VALUE,YYYYMM\nEE01,CD,0.1,201501\n")
  ref <- withr::local_tempfile()
  writeBin(body, ref)
  good <- unname(tools::sha256sum(ref))

  fetched <- character()
  local_mocked_bindings(
    curl_download = function(url, destfile, ...) {
      fetched <<- c(fetched, url)
      writeBin(body, destfile)
      destfile
    },
    .package = "curl"
  )
  dest <- file.path(im_cache_dir(IM_BUNDLED_VERSION), "MC_metal_chemistry_mosses.csv")

  local_mocked_bindings(im_api_dataset = new_style_record(c("a", strrep("0", 64), "c")))
  expect_error(im_download("MC", version = IM_BUNDLED_VERSION), "checksum")
  expect_false(file.exists(dest))

  # Checksums compare without regard to case.
  local_mocked_bindings(im_api_dataset = new_style_record(c("a", toupper(good), "c")))
  expect_identical(im_download("MC", version = IM_BUNDLED_VERSION), dest)
  expect_true(file.exists(dest))
  expect_identical(unique(fetched), "https://files.example/MC.csv")
})

test_that("a checksum that cannot be checked does not block a download", {
  f <- withr::local_tempfile()
  writeLines("x", f)
  expect_true(is.na(sha256_matches(f, NA_character_)))
  expect_true(is.na(sha256_matches(f, "")))
})

test_that("the code lists are fetched from their listed addresses", {
  withr::local_options(icpim.cache_dir = withr::local_tempdir(), icpim.quiet = TRUE)
  local_mocked_bindings(im_api_dataset = new_style_record(c("a", "b", NA)))
  fetched <- character()
  local_mocked_bindings(
    curl_download = function(url, destfile, ...) {
      fetched <<- c(fetched, url)
      stop("HTTP 503")
    },
    .package = "curl"
  )
  im_update_codes("9", quiet = TRUE)
  # The one list the repository listed comes from there; the rest from the
  # address built by hand.
  expect_true("https://files.example/substance_codes.csv" %in% fetched)
  expect_true(any(grepl("/9/documentation/pretreatment_codes\\.csv$", fetched)))
})
