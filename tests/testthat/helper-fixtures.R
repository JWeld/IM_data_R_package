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
