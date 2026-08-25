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
#'   rows into memory only to discard them.
#' * **Replace a cached file.** `overwrite = TRUE` re-fetches one that is stale
#'   or damaged. `im_read()` will always prefer what is already cached.
#' * **Give you the files, not the data.** It returns paths, so you can hand
#'   the CSVs to something that is not R without parsing them first.
#'
#' Downloads are atomic: each file is written to a temporary path and only
#' moved into place once complete, so an interrupted download cannot leave a
#' truncated file that later looks cached.
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
#'   # Warm the cache; note that nothing is returned to work with.
#'   im_download("MC")          # one small subprogramme, ~14 kB
#'   mc <- im_read("MC")        # this is the call that gives you data
#' }
#' }
im_download <- function(subprog, overwrite = FALSE, quiet = NULL,
                        version = im_version()) {
  quiet <- quiet %||% getOption("icpim.quiet", FALSE)
  codes <- resolve_subprog(subprog, several.ok = TRUE, version = version)
  meta  <- known_subprogs(version)
  meta  <- meta[match(codes, meta$subprog), ]
  dir   <- im_cache_dir(version, create = TRUE)

  # Sizes come from the repository when it can be reached, so the figure
  # quoted is the one for this release rather than a remembered one.
  sizes <- stats::setNames(rep(NA_real_, length(codes)), codes)
  man <- suppressWarnings(tryCatch(im_manifest(version, "data"), error = function(e) NULL))
  if (!is.null(man)) sizes[codes] <- man$size_mb[match(codes, man$subprog)]

  paths <- vapply(seq_len(nrow(meta)), function(i) {
    file <- meta$file[i]
    dest <- file.path(dir, file)
    if (file.exists(dest) && !overwrite) {
      if (!quiet) cli::cli_alert_info("{.field {meta$subprog[i]}} already cached.")
      return(dest)
    }
    url <- im_file_url(file, "data", version)
    if (!quiet) {
      sz <- sizes[[meta$subprog[i]]]
      cli::cli_alert_info(
        "Downloading {.field {meta$subprog[i]}} ({meta$name[i]}){if (is.na(sz)) '' else paste0(', ~', sz, ' MB')} ..."
      )
    }
    fetch_file(url, dest, quiet = quiet, version = version)
    dest
  }, character(1))

  if (!quiet) {
    cli::cli_alert_success("Cached {length(paths)} file{?s} in {.path {dir}}")
  }
  invisible(paths)
}

# Atomic download. Errors are turned into one clear message rather than curl's.
fetch_file <- function(url, dest, quiet = TRUE, version = im_version()) {
  tmp <- paste0(dest, ".part-", Sys.getpid())
  on.exit(unlink(tmp), add = TRUE)

  ok <- tryCatch(
    {
      # curl's byte-by-byte progress is useful at a prompt and pure noise in a
      # script or a log, so it follows interactivity rather than `quiet`.
      curl::curl_download(
        url, tmp,
        quiet = quiet || !interactive(),
        mode = "wb"
      )
      TRUE
    },
    error = function(e) {
      # Three quite different causes, and the advice differs completely
      # between them: no connection, a release that was never published, or a
      # file that has moved within a release that exists. A 404 blamed on the
      # network sends the reader looking in the wrong place.
      cause <- if (!curl::has_internet()) {
        c("i" = "There is no network connection.")
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
          "x" = conditionMessage(e),
          cause,
          "i" = paste(
            "The files can also be downloaded by hand from",
            "{.url https://doi.org/10.5878/z376-2m63} into",
            "{.path {dirname(dest)}}."
          )
        ),
        call = NULL
      )
    }
  )

  # A repository error page is HTML, not CSV, and would otherwise be cached and
  # then fail confusingly at parse time.
  if (ok) {
    first <- readLines(tmp, n = 1L, warn = FALSE, encoding = "UTF-8")
    if (length(first) && grepl("^\\s*<", first)) {
      cli::cli_abort(
        c("The server returned a web page rather than a CSV file.",
          "i" = "Version {.val {version}} may not exist."),
        call = NULL
      )
    }
    file.rename(tmp, dest)
  }
  invisible(dest)
}

# Path to a cached file, downloading it first if needed.
im_local_path <- function(subprog, version = im_version(), quiet = NULL) {
  code <- resolve_subprog(subprog, version = version)
  file <- subprog_file(code, version)
  dest <- file.path(im_cache_dir(version, create = FALSE), file)
  if (!file.exists(dest)) {
    im_download(code, quiet = quiet, version = version)
  }
  dest
}
