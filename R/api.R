# Talking to the repository ------------------------------------------------
#
# Everything version-specific is asked for at run time rather than baked into
# the package, so that a new annual release needs no code change. The bundled
# tables are a fallback for working offline, not the source of truth.

# Two version constants that are easy to confuse, so they sit together.
#
# IM_DEFAULT_VERSION is the release a session reads unless the user pins
# another with options(icpim.version=). It is the only place the default is
# written: .onLoad sets the option from it and im_version() falls back to it.
#
# IM_BUNDLED_VERSION is the release the bundled code lists were built from.
# The two are normally equal, and data-raw/make_data.R stamps the data so a
# test catches them drifting - but they are distinct, because pinning a newer
# release than the bundled lists is exactly the case warn_code_fallback()
# reports.
IM_DEFAULT_VERSION <- "1"
IM_BUNDLED_VERSION <- "1"

# One API call per version per session.
im_api_dataset <- function(version = NULL) {
  key <- paste0("api_", version %||% "latest")
  if (!is.null(the[[key]])) return(the[[key]])

  url <- if (is.null(version)) {
    sprintf("%s/%s", IM_API_BASE, IM_DATASET_ID)
  } else {
    sprintf("%s/%s/%s", IM_API_BASE, IM_DATASET_ID, version)
  }

  # A version that is absent stays absent, so remember it and stop asking. A
  # network failure is transient - a user who reconnects mid-session must not
  # be stuck with a cached failure - so that is never remembered.
  if (identical(the[[paste0(key, "_absent")]], TRUE)) return(NULL)

  raw <- tryCatch(curl::curl_fetch_memory(url), error = function(e) NULL)
  if (is.null(raw) || raw$status_code != 200L) return(NULL)

  js <- tryCatch(
    jsonlite::fromJSON(rawToChar(raw$content), simplifyVector = TRUE),
    error = function(e) NULL
  )
  # The API answers 200 with a null body for a version that does not exist,
  # so the status code alone does not tell you whether it is there.
  if (is.null(js) || is.null(js$dataset) || !length(js$dataset)) {
    the[[paste0(key, "_absent")]] <- TRUE
    return(NULL)
  }

  the[[key]] <- js$dataset
  js$dataset
}

#' Which dataset versions exist
#'
#' `im_latest_version()` asks the repository what the newest published version
#' is. `im_version_exists()` checks a particular one.
#'
#' These are the basis of [im_check_version()], which is the one you would
#' normally call. All of them need a network connection and return `NA` (or
#' `FALSE`) without one.
#'
#' @param version Version to check, as a string.
#'
#' @return `im_latest_version()` returns a string, or `NA` if the repository
#'   could not be reached. `im_version_exists()` returns a single logical.
#' @export
#' @examples
#' \donttest{
#' if (curl::has_internet()) {
#'   im_latest_version()
#' }
#' }
im_latest_version <- function() {
  d <- im_api_dataset(NULL)
  if (is.null(d)) return(NA_character_)
  chr1(d$versionNumber)
}

#' @rdname im_latest_version
#' @export
im_version_exists <- function(version) {
  !is.null(im_api_dataset(as.character(version)))
}

# Is version string `a` newer than `b`? as.numeric() turns "2.0.1" into NA,
# and an NA here reached if() in im_check_version and errored - on exactly the
# kind of version string this package has never seen, which is the case the
# check exists for. numeric_version() compares dotted strings correctly;
# anything neither of them can parse compares FALSE rather than crashing.
version_newer <- function(a, b) {
  cmp <- tryCatch(numeric_version(a) > numeric_version(b),
                  error = function(e) NA)
  if (isTRUE(cmp) || isFALSE(cmp)) return(cmp)
  isTRUE(suppressWarnings(as.numeric(a) > as.numeric(b)))
}

#' Check whether a newer dataset version has been published
#'
#' The deposit is updated annually. This package pins a version so that
#' analyses stay reproducible, which means it will not move to a new release
#' on its own - but it can tell you that one exists.
#'
#' Nothing here changes what you are reading. To move, set
#' `options(icpim.version = "2")` deliberately, rerun your analysis, and
#' compare.
#'
#' @param quiet Logical. Return the result without printing anything.
#'
#' @return Invisibly, a list with `current`, `latest` and `newer_available`.
#' @export
#' @examples
#' \donttest{
#' if (curl::has_internet()) {
#'   im_check_version()
#' }
#' }
im_check_version <- function(quiet = FALSE) {
  current <- im_version()
  latest  <- im_latest_version()
  newer   <- !is.na(latest) && version_newer(latest, current)

  if (!quiet) {
    if (is.na(latest)) {
      cli::cli_alert_warning("Could not reach the repository to check for updates.")
    } else if (newer) {
      cli::cli_alert_info(c(
        "Version {.val {latest}} is available; you are reading {.val {current}}."
      ))
      cli::cli_alert_info(
        "Move with {.code options(icpim.version = \"{latest}\")} when you are ready."
      )
    } else {
      cli::cli_alert_success("Version {.val {current}} is the latest.")
    }
  }
  invisible(list(current = current, latest = latest, newer_available = newer))
}

#' The files published in a given version
#'
#' Reads the file list straight from the repository, so a release that adds,
#' renames or resizes a file is picked up without this package being changed.
#' Falls back to the bundled catalogue when offline.
#'
#' @param version Dataset version. Defaults to [im_version()].
#' @param type `"data"` for the subprogramme files, `"documentation"` for the
#'   manual and code lists, or `"all"`.
#'
#' @return A tibble with `subprog`, `name`, `file`, `type` and `size_mb`.
#'   The `subprog` and `name` columns are `NA` for documentation files.
#' @export
#' @examples
#' \donttest{
#' if (curl::has_internet()) {
#'   im_manifest()
#' }
#' }
im_manifest <- function(version = im_version(),
                        type = c("data", "documentation", "all")) {
  type <- match.arg(type)
  d <- im_api_dataset(as.character(version))

  # A record can arrive without its file list, not only not at all - or with
  # a list that has no names or no types, which leaves every typed view of it
  # empty. All of these mean the same thing here: the repository's answer is
  # unusable.
  f <- if (is.null(d)) NULL else d$file
  if (is.null(f) || is.null(f$name) || !length(f$name) || is.null(f$type)) {
    cli::cli_warn(c(
      "Could not read the file list from the repository; using the bundled
       catalogue.",
      "i" = "It describes version {.val {IM_BUNDLED_VERSION}} and may be out of date."
    ))
    out <- tibble::tibble(
      subprog = im_subprogrammes$subprog,
      name    = im_subprogrammes$name,
      file    = im_subprogrammes$file,
      type    = "data",
      size_mb = NA_real_
    )
    return(if (type == "documentation") out[0, ] else out)
  }

  nm <- as.character(f$name)
  out <- tibble::tibble(
    file    = nm,
    type    = as.character(f$type),
    size_mb = round(num_n(f$contentSize, length(nm)) / 1024^2, 2)
  )
  # %in%, not ==: an NA type must not become an NA subscript downstream.
  out$subprog <- ifelse(
    out$type %in% "data",
    toupper(sub("_.*$", "", out$file)),
    NA_character_
  )
  # Descriptive names come from the bundled catalogue where we have them; a
  # subprogramme new in this release falls back to its file name.
  out$name <- im_subprogrammes$name[match(out$subprog, im_subprogrammes$subprog)]
  derived <- gsub("_", " ", sub("\\.csv$", "", sub("^[A-Za-z]+_", "", out$file)))
  out$name[is.na(out$name) & !is.na(out$subprog)] <-
    derived[is.na(out$name) & !is.na(out$subprog)]

  out <- out[, c("subprog", "name", "file", "type", "size_mb")]
  out <- out[order(is.na(out$subprog), out$file), ]

  switch(type,
    data          = out[out$type %in% "data", ],
    documentation = out[out$type %in% "documentation", ],
    all           = out
  )
}
