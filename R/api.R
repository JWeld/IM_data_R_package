# Talking to the repository ------------------------------------------------
#
# Everything version-specific is asked for at run time rather than baked into
# the package, so that a new annual release needs no code change. The bundled
# tables are a fallback for working offline, not the source of truth.

# Two version constants that are easy to confuse, so they sit together.
#
# IM_DEFAULT_VERSION is what a session reads unless the user pins a release
# with options(icpim.version=). It is "latest": the newest published release,
# looked up in the repository once per session by resolve_version(). It is the
# only place the default is written: .onLoad sets the option from it and
# im_version() falls back to it.
#
# IM_BUNDLED_VERSION is the release the bundled code lists were built from,
# and the release read when "latest" cannot be resolved and nothing is cached.
# data-raw/make_data.R stamps the data with it so a test catches the two
# drifting apart. Reading a newer release than the bundled lists is exactly
# the case warn_code_fallback() reports.
IM_DEFAULT_VERSION <- "latest"
IM_BUNDLED_VERSION <- "2"

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
  # be stuck with a cached failure - so that is never remembered for long:
  # only for IM_API_RETRY_AFTER seconds, because one im_read() asks several
  # times over and on a network that swallows packets each ask would otherwise
  # wait out the full timeout.
  if (identical(the[[paste0(key, "_absent")]], TRUE)) return(NULL)
  failed_at <- the[[paste0(key, "_failed_at")]]
  if (!is.null(failed_at) &&
      difftime(Sys.time(), failed_at, units = "secs") < IM_API_RETRY_AFTER) {
    return(NULL)
  }
  failed <- function() {
    the[[paste0(key, "_failed_at")]] <- Sys.time()
    NULL
  }

  raw <- tryCatch(curl::curl_fetch_memory(url, handle = im_handle(timeout = 30)),
                  error = function(e) NULL)
  if (is.null(raw) || raw$status_code != 200L) return(failed())

  # A 200 that is not JSON is somebody else's page - a captive portal, a proxy
  # error - and says nothing about the dataset, so it counts as a network
  # failure, not as an answer.
  js <- tryCatch(
    jsonlite::fromJSON(rawToChar(raw$content), simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (!is.list(js) || !"dataset" %in% names(js)) return(failed())

  # The API answers 200 with a null dataset for a version that does not exist,
  # so the status code alone does not tell you whether it is there. Only this
  # answer - the repository's own, parsed - is remembered as absence.
  if (is.null(js$dataset) || !length(js$dataset)) {
    the[[paste0(key, "_absent")]] <- TRUE
    return(NULL)
  }

  the[[paste0(key, "_failed_at")]] <- NULL
  the[[key]] <- js$dataset
  js$dataset
}

# Has the repository said that this version does not exist? FALSE also when
# it has not been asked, or could not answer: only a definite "no" counts.
version_absent <- function(version) {
  isTRUE(the[[paste0("api_", version, "_absent")]])
}

# Seconds before a failed lookup is tried again.
IM_API_RETRY_AFTER <- 30

# libcurl's own connection timeout is five minutes, which a session that is
# merely offline-ish would sit through in silence. `timeout` caps the whole
# transfer, so it is for the small API answers only; a file download gets the
# connection limit and a stall detector instead, since a slow link fetching
# 20 MB is not a failure.
im_handle <- function(timeout = NULL) {
  h <- curl::new_handle(connecttimeout = 10, low_speed_limit = 1,
                        low_speed_time = 60)
  if (!is.null(timeout)) curl::handle_setopt(h, timeout = timeout)
  h
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
#'   could not be reached or did not answer with a release number.
#'   `im_version_exists()` returns a single logical.
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
  v <- chr1(d$versionNumber)
  # It becomes a cache directory and part of every address, so only a release
  # number is believed.
  if (is_version_number(v)) v else NA_character_
}

# A release number as the repository writes them: "2", "2.1". The one check
# between a version string and a file path or an address.
is_version_number <- function(x) {
  length(x) == 1L && !is.na(x) && grepl("^[0-9]+(\\.[0-9]+)*$", x)
}

#' @rdname im_latest_version
#' @export
im_version_exists <- function(version) {
  version <- resolve_version(version)
  !is.null(im_api_dataset(version))
}

# Turn a version setting into a concrete version. "latest" is the one symbolic
# value, and it is resolved once per session: a session reads one release
# throughout, even if the network comes and goes, and im_version() - the
# default of nearly every `version` argument - does not go to the repository on
# every call. Anything else must be a release number, and is otherwise taken
# as written; the repository decides whether it exists.
resolve_version <- function(version) {
  version <- tryCatch(as.character(version), error = function(e) character())
  version <- version[!is.na(version) & nzchar(version)]
  if (!length(version)) version <- IM_DEFAULT_VERSION
  version <- trimws(version[[1]])
  if (!identical(tolower(version), "latest")) {
    # Anything else becomes a cache directory, an argument to
    # im_cache_clear()'s unlink() and part of every address, so it must be a
    # release number: "1/../../x" would otherwise reach outside the cache.
    if (!is_version_number(version)) {
      cli::cli_abort(c(
        "{.val {version}} is not a dataset version.",
        "i" = "Use a release number such as {.val {IM_BUNDLED_VERSION}}, or
               {.val latest}."
      ), call = NULL)
    }
    return(version)
  }
  if (!is.null(the$resolved_latest)) return(the$resolved_latest)

  quiet  <- isTRUE(getOption("icpim.quiet", FALSE))
  latest <- im_latest_version()
  if (!is.na(latest)) {
    if (!quiet) {
      cli::cli_alert_info(c(
        "Reading version {.val {latest}} of the dataset, the newest release. ",
        "Pin it with {.code options(icpim.version = \"{latest}\")} so the ",
        "analysis keeps returning the same numbers after the next release."
      ))
    }
    the$resolved_latest <- latest
    return(latest)
  }

  # Offline. The newest release already in the cache is the best available
  # answer - it is what this machine last read - and failing that, the release
  # this package was built against. Either way, say which and why: silently
  # reading an older release than "latest" promises is the failure to avoid.
  cached   <- newest_cached_version()
  fallback <- if (is.na(cached)) IM_BUNDLED_VERSION else cached
  if (!quiet) {
    cli::cli_alert_warning(c(
      "Could not reach the repository to find the newest release; reading ",
      "version {.val {fallback}} ",
      if (is.na(cached)) "(the release this package was built against) "
      else "(the newest release in the cache) ",
      "for the rest of this session."
    ))
  }
  the$resolved_latest <- fallback
  fallback
}

# Is the session set to follow the newest release rather than a pinned one?
following_latest <- function() {
  v <- getOption("icpim.version", IM_DEFAULT_VERSION)
  identical(tolower(as.character(v %||% IM_DEFAULT_VERSION)[1]), "latest")
}

# The newest release with data in the cache, or NA. Data means a published
# CSV: a directory holding only the fetched code lists, or the remains of an
# interrupted download, has nothing in it to read, and settling on it offline
# would fail at the first im_read().
newest_cached_version <- function() {
  root <- cache_root()
  if (!dir.exists(root)) return(NA_character_)
  dirs <- list.files(root, pattern = "^v[0-9]+(\\.[0-9]+)*$")
  dirs <- dirs[vapply(dirs, function(d) {
    length(list.files(file.path(root, d), pattern = "\\.csv$")) > 0L
  }, logical(1))]
  if (!length(dirs)) return(NA_character_)
  v <- sub("^v", "", dirs)
  v[order(numeric_version(v), decreasing = TRUE)][[1]]
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
#' The deposit is updated annually. By default a session reads the newest
#' release, so this normally confirms that what you are reading is the latest.
#' It matters once you have pinned a version with `options(icpim.version=)`,
#' which is the right thing to do for an analysis that must keep returning the
#' same numbers: a pinned session never moves on its own, but this tells you
#' that a newer release exists.
#'
#' It also catches a session that started offline. `"latest"` is resolved once
#' per session, so a session that could not reach the repository when it
#' started keeps reading the fallback release even after the network returns;
#' this says so.
#'
#' Nothing here changes what you are reading. To move, set
#' `options(icpim.version = "3")` deliberately, rerun your analysis, and
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
    } else if (newer && following_latest()) {
      cli::cli_alert_info(c(
        "Version {.val {latest}} is available, but this session settled on ",
        "{.val {current}} because the repository could not be reached when ",
        "it started."
      ))
      cli::cli_alert_info(
        "Restart R to read {.val {latest}}, or set {.code options(icpim.version = \"{latest}\")}."
      )
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
#' @return A tibble with `subprog`, `name`, `file`, `type`, `size_mb`, `url`
#'   and `sha256`. The `subprog` and `name` columns are `NA` for documentation
#'   files. `url` is where the repository serves the file, and `sha256` its
#'   published checksum; both are `NA` in the bundled catalogue used offline.
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
  version <- resolve_version(version)
  d <- im_api_dataset(version)
  out <- if (is.null(d)) NULL else parse_file_list(d$file)

  if (is.null(out)) {
    # A version the repository has just said does not exist is a different
    # problem from a repository that could not be read, and both differ from
    # an answer this package cannot make sense of. The advice differs too:
    # pick a version that exists, try again later, or report it.
    if (is.null(d) && version_absent(version)) {
      cli::cli_warn(c(
        "Version {.val {version}} is not published; using the bundled
         catalogue.",
        "i" = "It describes version {.val {IM_BUNDLED_VERSION}}.
               {.fn im_latest_version} says which release is newest."
      ))
    } else if (is.null(d)) {
      cli::cli_warn(c(
        "Could not read the file list from the repository; using the bundled
         catalogue.",
        "i" = "It describes version {.val {IM_BUNDLED_VERSION}} and may be out of date."
      ))
    } else {
      cli::cli_warn(c(
        "The repository's file list is not in a form this package recognises;
         using the bundled catalogue.",
        "i" = "It describes version {.val {IM_BUNDLED_VERSION}} and may be out of date.",
        "i" = "Please report this at {.url {IM_BUG_REPORTS}}: the repository
               has probably changed how it lists its files."
      ))
    }
    out <- tibble::tibble(
      subprog = im_subprogrammes$subprog,
      name    = im_subprogrammes$name,
      file    = im_subprogrammes$file,
      type    = "data",
      size_mb = NA_real_,
      url     = NA_character_,
      sha256  = NA_character_
    )
    return(if (type == "documentation") out[0, ] else out)
  }

  switch(type,
    data          = out[out$type %in% "data", ],
    documentation = out[out$type %in% "documentation", ],
    all           = out
  )
}

# The repository's file list as a manifest, or NULL if it cannot be read as
# one.
#
# The list has come in two shapes. Until autumn 2026 each file had a bare
# name and a `type` of "data" or "documentation". Since then the name carries
# the folder ("data/AC_air_chemistry.csv"), `type` is the access level
# ("open"), and each file has its own `url` and `sha256`. Read under the old
# rules, the new list classified nothing as data and the manifest came back
# empty without a word. So the folder decides where there is one, `type`
# where there is not - and a list in which nothing can be classified is
# unusable, which im_manifest() says out loud, rather than a release with no
# files.
parse_file_list <- function(f) {
  if (is.null(f) || is.null(f$name) || !length(f$name)) return(NULL)
  nm <- as.character(f$name)
  n  <- length(nm)

  folder <- ifelse(grepl("/", nm, fixed = TRUE), sub("/.*$", "", nm), NA_character_)
  type   <- ifelse(is.na(folder), chr_n(f$type, n), folder)
  if (!any(type %in% c("data", "documentation"))) return(NULL)

  out <- tibble::tibble(
    file    = basename(nm),
    type    = type,
    size_mb = round(num_n(f$contentSize, n) / 1024^2, 2),
    url     = chr_n(f$url, n),
    sha256  = tolower(chr_n(f$sha256, n))
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

  out <- out[, c("subprog", "name", "file", "type", "size_mb", "url", "sha256")]
  out[order(is.na(out$subprog), out$file), ]
}
