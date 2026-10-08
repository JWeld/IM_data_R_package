#' Fill the cache without reading anything
#'
#' **To get data, use [im_read()], not this.** `im_read()` downloads whatever
#' it needs on its own, so `im_download("PC")` followed by `im_read("PC")` does
#' nothing the second call would not have done alone.
#'
#' This is for managing the cache rather than obtaining data, and there are
#' three things it does that [im_read()] cannot:
#'
#' * **Fetch everything at once.** `im_read()` takes one subprogramme;
#'   `im_download("all")` fetches all 21 (about 95 MB), which is how you
#'   prepare to work offline. Reading them all instead would load 1.2 million
#'   rows into memory only to discard them. For a release other than the one
#'   bundled with the package, the code lists are fetched too, so decoding
#'   works offline as well.
#' * **Replace a cached file.** `overwrite = TRUE` re-fetches one that is stale
#'   or damaged. `im_read()` will always prefer what is already cached.
#' * **Give you the files, not the data.** It returns paths, so you can hand
#'   the CSVs to something that is not R without parsing them first.
#'
#' Downloads are atomic: each file is written to a temporary path and only
#' moved into place once complete, so an interrupted download cannot leave a
#' truncated file that later looks cached. Files are fetched from the address
#' the repository lists for them, and on R 4.5 or later each is checked
#' against the SHA-256 checksum the repository publishes before it is cached.
#'
#' @param subprog Character vector of two-letter subprogramme codes, e.g.
#'   `"PC"`. Use `"all"` for every subprogramme. See [im_subprogrammes].
#' @param overwrite Logical. Re-download files that are already cached?
#' @param quiet Logical. Suppress progress messages.
#' @param version Dataset version. Defaults to [im_version()].
#'
#' @return The local paths of the cached files, invisibly. The data itself
#'   comes from [im_read()].
#' @seealso [im_read()] to read a subprogramme, which downloads it for you;
#'   [im_cache_list()] for what is already cached.
#' @export
#' @examples
#' \donttest{
#' if (curl::has_internet()) {
#'   # A temporary cache, so the example leaves nothing behind. In normal use
#'   # leave the default, which persists between sessions.
#'   op <- options(icpim.cache_dir = tempfile())
#'
#'   try({
#'     # Warm the cache; note that nothing is returned to work with.
#'     im_download("MC")        # one small subprogramme, ~14 kB
#'     mc <- im_read("MC")      # this is the call that gives you data
#'   })
#'   options(op)
#' }
#' }
im_download <- function(subprog, overwrite = FALSE, quiet = NULL,
                        version = im_version()) {
  quiet <- quiet %||% getOption("icpim.quiet", FALSE)
  version <- with_quiet(quiet, resolve_version(version))
  codes <- resolve_subprog(subprog, several.ok = TRUE, version = version)
  meta  <- known_subprogs(version)
  meta  <- meta[match(codes, meta$subprog), ]
  dir   <- im_cache_dir(version, create = TRUE)

  # Addresses, checksums and sizes come from the repository when it can be
  # reached, so the figure quoted is the one for this release rather than a
  # remembered one.
  sizes <- stats::setNames(rep(NA_real_, length(codes)), codes)
  man <- suppressWarnings(tryCatch(im_manifest(version, "data"), error = function(e) NULL))
  if (!is.null(man)) sizes[codes] <- man$size_mb[match(codes, man$subprog)]

  paths <- vapply(seq_len(nrow(meta)), function(i) {
    file <- meta$file[i]
    # basename(): for a release this package was not built against, the name
    # comes from the repository's manifest, and a name with a directory in it
    # must not write outside the cache.
    dest <- file.path(dir, basename(file))
    if (file.exists(dest) && !overwrite) {
      if (!quiet) cli::cli_alert_info("{.field {meta$subprog[i]}} already cached.")
      return(dest)
    }
    src <- file_source(file, "data", version, man)
    if (!quiet) {
      sz <- sizes[[meta$subprog[i]]]
      cli::cli_alert_info(
        "Downloading {.field {meta$subprog[i]}} ({meta$name[i]}){if (is.na(sz)) '' else paste0(', ~', sz, ' MB')} ..."
      )
    }
    fetch_file(src$url, dest, quiet = quiet, version = version, sha256 = src$sha256)
    dest
  }, character(1))

  # Reading offline needs the code lists as well as the data. For a release
  # other than the bundled one they are fetched on the first read, which
  # offline falls back to the bundled lists - so a cache filled here for
  # working offline would have decoded against the wrong release.
  if (!identical(version, IM_BUNDLED_VERSION) &&
      (overwrite || !code_cache_complete(version))) {
    im_update_codes(version, quiet = quiet)
  }

  if (!quiet) {
    cli::cli_alert_success("Cached {length(paths)} file{?s} in {.path {dir}}")
  }
  invisible(paths)
}

# Atomic download. Errors are turned into one clear message rather than curl's.
# `sha256`, where the repository published one, is checked before the file is
# moved into place, so a file that arrived damaged is never cached.
fetch_file <- function(url, dest, quiet = TRUE, version = im_version(),
                       sha256 = NA_character_) {
  tmp <- paste0(dest, ".part-", Sys.getpid())
  on.exit(unlink(tmp), add = TRUE)

  ok <- tryCatch(
    {
      # curl's byte-by-byte progress is useful at a prompt and pure noise in a
      # script or a log, so it follows interactivity rather than `quiet`.
      curl::curl_download(
        url, tmp,
        quiet = quiet || !interactive(),
        mode = "wb",
        handle = im_handle()
      )
      TRUE
    },
    error = function(e) {
      # Five quite different causes, and the advice differs completely
      # between them: no connection, a repository that cannot be reached, a
      # repository that refuses the request, a release that was never
      # published, or a file that has moved within a release that exists. A
      # 404 blamed on the network sends the reader looking in the wrong place
      # - and an outage blamed on the version sends them re-pinning a release
      # that exists. The existence check cannot tell absent from unreachable
      # on its own, so ask about the dataset as a whole first.
      status <- http_status(e)
      cause <- if (!curl::has_internet()) {
        c("i" = "There is no network connection.")
      } else if (is.null(im_api_dataset(NULL))) {
        c("i" = "The repository could not be reached. Try again later.")
      } else if (status %in% c(401L, 403L)) {
        # What the old download address answered to everything once the
        # repository moved its files. Not a missing file: a refusal.
        c("i" = "The repository refused the request (HTTP {status}). It may
                 have moved its files.",
          "i" = "Please report this at {.url {IM_BUG_REPORTS}}.")
      } else if (!im_version_exists(version)) {
        c("i" = "Version {.val {version}} is not published.",
          "i" = "Pin one that is with {.code options(icpim.version = ...)};
                 {.fn im_latest_version} says which is newest.")
      } else {
        c("i" = "Version {.val {version}} exists, so the file may have been
                 renamed or withdrawn.",
          "i" = "See {.fn im_manifest} for what that release publishes.")
      }
      cli::cli_abort(
        c(
          "Could not download {.url {url}}.",
          # Interpolated, not pasted in: cli reads a bare string as a template,
          # and an error text containing braces would be evaluated as R.
          "x" = "{conditionMessage(e)}",
          cause,
          "i" = paste(
            "The files can also be downloaded by hand from",
            "{.url https://doi.org/{IM_DOI_CONCEPT}} into",
            "{.path {dirname(dest)}}."
          )
        ),
        call = NULL
      )
    }
  )

  # A repository error page is HTML, not CSV, and would otherwise be cached and
  # then fail confusingly at parse time. Likewise a zero-byte answer - which
  # readLines() cannot see, since an empty file has no first line to check -
  # would be installed as cached and then fail on every later read until the
  # user finds `overwrite = TRUE`.
  if (ok) {
    if (!isTRUE(file.size(tmp) > 0)) {
      cli::cli_abort(
        c("The server returned an empty file for {.url {url}}.",
          "i" = "Nothing was cached. Try again, or download by hand from
                 {.url https://doi.org/{IM_DOI_CONCEPT}}."),
        call = NULL
      )
    }
    if (is_web_page(tmp)) {
      cli::cli_abort(
        c("The server returned a web page rather than a CSV file.",
          "i" = "Version {.val {version}} may not exist."),
        call = NULL
      )
    }
    if (isFALSE(sha256_matches(tmp, sha256))) {
      cli::cli_abort(
        c("{.url {url}} did not match the checksum the repository publishes
           for it.",
          "i" = "Nothing was cached. The download was probably damaged in
                 transit; try again."),
        call = NULL
      )
    }
    if (!file.rename(tmp, dest)) {
      cli::cli_abort(
        c("Downloaded {.url {url}} but could not move it into the cache.",
          "i" = "Check that {.path {dirname(dest)}} is writable."),
        call = NULL
      )
    }
  }
  invisible(dest)
}

# The HTTP status behind a failed download, or NA. curl reports it only in
# the message of a `curl_error_http_returned_error`.
http_status <- function(e) {
  if (!inherits(e, "curl_error_http_returned_error")) return(NA_integer_)
  m <- regmatches(conditionMessage(e),
                  regexpr("[0-9]{3}(?=[^0-9]*$)", conditionMessage(e), perl = TRUE))
  if (length(m)) as.integer(m) else NA_integer_
}

# Does a file match its published SHA-256? NA when that cannot be said: no
# checksum was published, or this R has no way to compute one.
# tools::sha256sum() arrived in R 4.5.0, and the package supports older
# versions rather than take a dependency for one check; it is looked up at
# run time so that R CMD check under an older R does not report it missing.
sha256_matches <- function(path, expected) {
  expected <- chr1(expected)
  if (is.na(expected)) return(NA)
  hash <- get0("sha256sum", envir = asNamespace("tools"), mode = "function",
               inherits = FALSE)
  if (is.null(hash)) return(NA)
  identical(tolower(unname(hash(path))), tolower(expected))
}

# Does a downloaded file start like HTML rather than CSV?
is_web_page <- function(path) {
  first <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  length(first) > 0L && grepl("^\\s*<", first)
}

# Path to a cached file, downloading it first if needed.
im_local_path <- function(subprog, version = im_version(), quiet = NULL) {
  version <- with_quiet(quiet, resolve_version(version))
  code <- resolve_subprog(subprog, version = version)
  file <- subprog_file(code, version)
  dest <- file.path(im_cache_dir(version, create = FALSE), basename(file))
  if (!file.exists(dest)) {
    im_download(code, quiet = quiet, version = version)
  }
  dest
}
