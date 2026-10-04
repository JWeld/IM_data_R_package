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

# Rows with no code cannot be joined to anything. `[["code"]]`, not `$code`: a
# file whose header has changed builds a table with no such column, which
# valid_code_table() then rejects - and `$` on a tibble would warn about the
# column on the way.
keep_coded <- function(tab) tab[!is.na(tab[["code"]]), ]

# Turn the raw documentation frames into the package's tidy lookups.
build_code_tables <- function(raw) {
  out <- list()
  if (!is.null(raw$substances)) {
    s <- raw$substances
    out$substances <- keep_coded(tibble::tibble(
      code = s$SubstanceCode, name = s$Name,
      group = suppressWarnings(as.integer(s$Group)),
      cas = s$CASnumber, description = s$Description
    ))
  }
  if (!is.null(raw$parameters)) {
    p <- raw$parameters
    out$parameters <- keep_coded(tibble::tibble(
      subprog = p$Subprogramme, subprog_name = p$SubprogName,
      code = p$Parameter, name = p$ParamName, list = p$ParamList,
      unit = p$Unit,
      minimum = suppressWarnings(as.numeric(p$Minimum)),
      maximum = suppressWarnings(as.numeric(p$Maximum))
    ))
  }
  if (!is.null(raw$determinations)) {
    d <- raw$determinations
    out$determinations <- keep_coded(tibble::tibble(
      code = d$DeterminationCode, name = d$Description, note = d$NOTE
    ))
  }
  if (!is.null(raw$pretreatments)) {
    p <- raw$pretreatments
    out$pretreatments <- keep_coded(
      tibble::tibble(code = p$PretreatmentCode, name = p$Description)
    )
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

# Is this a lookup table that can be joined against? A published list whose
# header was renamed builds into a table with no `code` column and no rows,
# and every code then decodes to NA - so a table is checked before it is
# cached and again when it is read back, and one that fails is treated as not
# fetched: the bundled list, with its warning, beats an empty one in silence.
valid_code_table <- function(tab, which) {
  need <- if (identical(which, "sites")) c("area", "name") else c("code", "name")
  ok <- is.data.frame(tab) && all(need %in% names(tab)) && nrow(tab) > 0L &&
    !all(is.na(tab[[need[1]]]))
  # Sodium is the canary: if "NA" is not among the substance codes, something
  # on the way here treated it as missing.
  if (ok && identical(which, "substances")) ok <- "NA" %in% tab$code
  ok
}

# The cached lists for a version, or an empty list if there are none that can
# be read.
read_code_cache <- function(version) {
  p <- code_cache_path(version)
  if (!file.exists(p)) return(list())
  tabs <- tryCatch(readRDS(p), error = function(e) NULL)
  if (is.list(tabs)) tabs else list()
}

#' Fetch the code lists published with a given version
#'
#' The lookups bundled with this package come from one release. A later release
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
#'   try(im_update_codes())
#'   options(op)
#' }
#' }
im_update_codes <- function(version = im_version(), quiet = NULL) {
  quiet <- quiet %||% getOption("icpim.quiet", FALSE)
  version <- resolve_version(version)
  dest <- code_cache_path(version)

  tabs <- list()
  # One handle for the five files, so the connection to the repository is
  # reused rather than opened afresh for each.
  h <- im_handle()
  for (nm in names(IM_CODE_FILES)) {
    tmp <- tempfile(fileext = ".csv")
    # Each list on its own, start to finish: a download that fails, an error
    # page served with status 200, or a file whose columns have changed loses
    # that one list, not the other four and not the read that asked for them.
    tab <- tryCatch({
      curl::curl_download(im_file_url(IM_CODE_FILES[[nm]], "documentation", version),
                          tmp, quiet = TRUE, mode = "wb", handle = h)
      problem <- downloaded_csv_problem(tmp)
      if (!is.null(problem)) stop("not a CSV file: ", problem)
      build_code_tables(stats::setNames(list(read_doc_csv(tmp)), nm))[[nm]]
    }, error = function(e) NULL)
    unlink(tmp)
    if (!valid_code_table(tab, nm)) {
      if (!quiet) {
        cli::cli_alert_warning(
          "Could not fetch a usable {.file {IM_CODE_FILES[[nm]]}} for version {version}."
        )
      }
      next
    }
    tabs[[nm]] <- tab
  }
  if (!length(tabs)) return(invisible(NULL))

  # Lists fetched now replace their cached copies; a list that failed this
  # time keeps the copy it had, so a refresh on a bad connection cannot make
  # the cache worse than it was.
  old  <- read_code_cache(version)
  tabs <- c(tabs, old[setdiff(names(old), names(tabs))])

  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (!write_atomically(dest, function(p) saveRDS(tabs, p))) return(invisible(NULL))
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
  version <- resolve_version(version)
  bundled <- switch(which,
    substances     = im_substances,
    parameters     = im_parameters,
    determinations = im_determinations,
    pretreatments  = im_pretreatments,
    sites          = im_sites
  )
  if (identical(version, IM_BUNDLED_VERSION)) return(bundled)

  # First read of an unfamiliar release: fetch once, and remember the attempt
  # so a failure does not retry on every call. A cache that exists but lacks
  # this list - an earlier fetch got some files and not others - is fetched
  # again too, rather than left to fall back for ever.
  tab <- read_code_cache(version)[[which]]
  tried <- paste0("codes_tried_", version)
  if (!valid_code_table(tab, which) && !isTRUE(the[[tried]])) {
    the[[tried]] <- TRUE
    im_update_codes(version, quiet = TRUE)
    tab <- read_code_cache(version)[[which]]
  }
  if (!valid_code_table(tab, which)) return(warn_code_fallback(version, bundled))
  tab
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
