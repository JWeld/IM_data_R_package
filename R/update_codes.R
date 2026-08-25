# Version-aware code lists -------------------------------------------------
#
# The bundled lookups come from the version named in IM_BUNDLED_VERSION. A
# later release can add substances, parameters or methods, so for any other
# version the published lists are fetched and cached, and used in preference
# to the bundled ones. Nothing needs editing here when that happens.

IM_CODE_FILES <- c(
  substances     = "substance_codes.csv",
  parameters     = "parameters_and_codes_by_subprogramme.csv",
  determinations = "determination_codes.csv",
  pretreatments  = "pretreatment_codes.csv",
  sites          = "IM_sites_info.csv"
)

# Read one published lookup CSV.
#
# vroom rather than read.csv: it strips the UTF-8 BOM these files carry, and
# reads UTF-8 whatever the session locale is. read.csv(fileEncoding =
# "UTF-8-BOM") truncates them to 41 rows under a C locale, because it tries to
# transcode the Scandinavian site names into the native encoding and fails.
#
# `na = character()` is not optional: sodium's substance code is the string
# "NA" and any NA handling at all destroys it.
read_doc_csv <- function(path) {
  x <- vroom::vroom(
    path,
    delim = ",",
    col_types = vroom::cols(.default = vroom::col_character()),
    na = character(),
    show_col_types = FALSE,
    progress = FALSE
  )
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  # The published lists are space-padded to fixed width; the data files are
  # not, so untrimmed codes join to nothing.
  x[] <- lapply(x, function(col) {
    col <- trimws(col)
    col[col == "NULL" | !nzchar(col)] <- NA_character_
    col
  })
  x
}

# Turn the raw documentation frames into the package's tidy lookups.
build_code_tables <- function(raw) {
  out <- list()
  if (!is.null(raw$substances)) {
    s <- raw$substances
    out$substances <- tibble::tibble(
      code = s$SubstanceCode, name = s$Name,
      group = suppressWarnings(as.integer(s$Group)),
      cas = s$CASnumber, description = s$Description
    )
    out$substances <- out$substances[!is.na(out$substances$code), ]
  }
  if (!is.null(raw$parameters)) {
    p <- raw$parameters
    out$parameters <- tibble::tibble(
      subprog = p$Subprogramme, subprog_name = p$SubprogName,
      code = p$Parameter, name = p$ParamName, list = p$ParamList,
      unit = p$Unit,
      minimum = suppressWarnings(as.numeric(p$Minimum)),
      maximum = suppressWarnings(as.numeric(p$Maximum))
    )
    out$parameters <- out$parameters[!is.na(out$parameters$code), ]
  }
  if (!is.null(raw$determinations)) {
    d <- raw$determinations
    out$determinations <- tibble::tibble(
      code = d$DeterminationCode, name = d$Description, note = d$NOTE
    )
    out$determinations <- out$determinations[!is.na(out$determinations$code), ]
  }
  if (!is.null(raw$pretreatments)) {
    p <- raw$pretreatments
    out$pretreatments <- tibble::tibble(code = p$PretreatmentCode, name = p$Description)
    out$pretreatments <- out$pretreatments[!is.na(out$pretreatments$code), ]
  }
  if (!is.null(raw$sites)) {
    s <- raw$sites
    out$sites <- tibble::tibble(
      area = s$Acode, country = s$CountryCod, name = s$Name,
      latitude = as.numeric(s$Latitude), longitude = as.numeric(s$Longitude),
      active = as.integer(s$Active) == 1L
    )
  }
  out
}

#' Fetch the code lists published with a given version
#'
#' The lookups bundled with this package come from version 1. A later release
#' can add substances, parameters, methods or sites, so for any other version
#' the published lists are downloaded and cached, then used in preference to
#' the bundled ones.
#'
#' This happens on its own the first time you read a version the package was
#' not built against, so you do not normally need to call it. Use it to
#' refresh a cached copy, or to warm the cache before going offline.
#'
#' @param version Dataset version. Defaults to [im_version()].
#' @param quiet Logical. Suppress progress messages.
#'
#' @return Invisibly, the path of the cached code lists, or `NULL` if they
#'   could not be fetched.
#' @export
#' @examples
#' \donttest{
#' if (curl::has_internet()) {
#'   # A temporary cache, so the example leaves nothing behind. In normal use
#'   # leave the default, which persists between sessions.
#'   op <- options(icpim.cache_dir = tempfile())
#'   im_update_codes()
#'   options(op)
#' }
#' }
im_update_codes <- function(version = im_version(), quiet = NULL) {
  quiet <- quiet %||% getOption("icpim.quiet", FALSE)
  dest <- code_cache_path(version)

  raw <- list()
  for (nm in names(IM_CODE_FILES)) {
    tmp <- tempfile(fileext = ".csv")
    ok <- tryCatch({
      curl::curl_download(im_file_url(IM_CODE_FILES[[nm]], "documentation", version),
                          tmp, quiet = TRUE, mode = "wb")
      TRUE
    }, error = function(e) FALSE)
    if (!ok) {
      if (!quiet) {
        cli::cli_alert_warning(
          "Could not fetch {.file {IM_CODE_FILES[[nm]]}} for version {version}."
        )
      }
      unlink(tmp)
      next
    }
    raw[[nm]] <- read_doc_csv(tmp)
    unlink(tmp)
  }
  if (!length(raw)) return(invisible(NULL))

  tabs <- build_code_tables(raw)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  saveRDS(tabs, dest)
  if (!quiet) {
    cli::cli_alert_success(
      "Cached the version {version} code lists ({length(tabs)} table{?s})."
    )
  }
  invisible(dest)
}

code_cache_path <- function(version) {
  file.path(im_cache_dir(version, create = FALSE), "_code_lists.rds")
}

# The lookup table to use for a given version: cached published lists if we
# have them, otherwise the bundled ones.
codes_for <- function(which, version = im_version()) {
  bundled <- switch(which,
    substances     = im_substances,
    parameters     = im_parameters,
    determinations = im_determinations,
    pretreatments  = im_pretreatments,
    sites          = im_sites
  )
  if (identical(as.character(version), IM_BUNDLED_VERSION)) return(bundled)

  p <- code_cache_path(version)
  if (!file.exists(p)) {
    # First read of an unfamiliar release: fetch once, and remember the
    # attempt so a failure does not retry on every call.
    key <- paste0("codes_tried_", version)
    if (!isTRUE(the[[key]])) {
      the[[key]] <- TRUE
      im_update_codes(version, quiet = TRUE)
    }
  }
  if (!file.exists(p)) return(warn_code_fallback(version, bundled))

  tabs <- tryCatch(readRDS(p), error = function(e) NULL)
  if (is.null(tabs) || is.null(tabs[[which]])) {
    return(warn_code_fallback(version, bundled))
  }
  tabs[[which]]
}

# Falling back to another release's code lists is the one failure here that is
# silent and wrong rather than merely absent: names would be decoded against
# the wrong vocabulary, and a code added in the newer release would come back
# NA with nothing to say why. Warn once per version per session.
#
# This also catches the maintainer's mistake of moving the default version
# without rebuilding the bundled lookups: the package then reads a release it
# has no code lists for, and says so the first time anyone runs it.
warn_code_fallback <- function(version, bundled) {
  key <- paste0("codes_warned_", version)
  if (!isTRUE(the[[key]])) {
    the[[key]] <- TRUE
    cli::cli_warn(c(
      "Decoding version {.val {version}} with the code lists published for
       version {.val {IM_BUNDLED_VERSION}}.",
      "!" = "Codes added since then will not resolve, and any that were
             redefined will decode to the older meaning.",
      "i" = "Run {.run icpim::im_update_codes()} with a network connection to
             fetch the lists for this release.",
      "i" = "If you maintain this package, this also means the bundled lookups
             are older than the default version: rerun
             {.file data-raw/make_data.R}."
    ))
  }
  bundled
}
