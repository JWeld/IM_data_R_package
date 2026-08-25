#' Download the published files for one or more subprogrammes
#'
#' Fetches the CSV files from the repository into the cache. [im_read()] calls
#' this for you, so you rarely need it directly; it is useful for warming the
#' cache before going offline, or for fetching everything at once.
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
#' @return The local paths of the cached files, invisibly.
#' @export
#' @examples
#' \donttest{
#' if (curl::has_internet()) {
#'   # Fetch one small subprogramme (~14 kB):
#'   im_download("MC")
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
