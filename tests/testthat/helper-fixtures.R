# A corrected copy of the bundled PC extract: sodium already carries its code,
# as it does in the published data from version 2 onwards. Built from the real
# extract so it differs from it in exactly the one respect under test.
corrected_pc_file <- function(env = parent.frame()) {
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
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  writeLines(c(src[1], body), path, useBytes = TRUE)
  path
}

# Plant a PC file in a temporary cache for the bundled release, so im_read()
# works offline with no repair and no code-list fetch. The file is the
# corrected extract, as that release publishes it.
local_planted_pc <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_options(icpim.cache_dir = dir,
                       icpim.version = IM_BUNDLED_VERSION,
                       .local_envir = env)
  file.copy(corrected_pc_file(env),
            file.path(im_cache_dir(create = TRUE),
                      "PC_precipitation_chemistry.csv"))
  invisible(dir)
}

# The live tests need the data repository, not just a network. On many
# institutional networks DNS works and the data hosts are blocked, so
# skip_if_offline() alone lets them run and fail. One API call decides, and
# im_latest_version() remembers a positive answer for the session.
#
# With ICPIM_REQUIRE_REPOSITORY=true, as in the live-tests workflow, an
# unreachable repository is a failure rather than a skip. That workflow exists
# to notice the repository changing under the package, and a change that made
# the API itself unanswerable would otherwise skip every test that could have
# noticed it.
skip_if_repository_unreachable <- function() {
  required <- identical(Sys.getenv("ICPIM_REQUIRE_REPOSITORY"), "true")
  if (!required) {
    skip_on_cran()
    skip_if_offline()
  }
  if (is.na(im_latest_version())) {
    if (required) {
      stop("The data repository could not be reached, and ",
           "ICPIM_REQUIRE_REPOSITORY is set.", call. = FALSE)
    }
    skip("the data repository cannot be reached")
  }
}
