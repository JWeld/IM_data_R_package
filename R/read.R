# Column typing -----------------------------------------------------------
#
# The published files are read as character throughout and coerced here, rather
# than letting the CSV reader guess. Two reasons, both of which silently
# corrupt data otherwise:
#
#   * SCODE is a zero-padded four-digit station code ("0001"). Guessed as
#     integer it becomes 1, and no longer joins or groups correctly.
#   * The substance code for sodium is the literal string "NA", which every
#     default `na.strings` / `na =` setting in R treats as missing.
#
# Reading everything as text with no NA strings at all makes both impossible,
# and leaves this file as the single place where "empty means missing" is
# decided.

IM_NUMERIC_COLS <- c(
  "VALUE", "LEVEL", "SPOOL", "SIZE", "NEEDLES", "DAY",
  "TAXONKEY_GBIF", "MEDIUM_TAXONKEY_GBIF"
)

#' Read a subprogramme
#'
#' Downloads (if needed), caches, reads and tidies one subprogramme of the ICP
#' IM open dataset. This is the main entry point of the package.
#'
#' # What this does beyond reading the CSV
#'
#' * **Types every column explicitly.** `SCODE` keeps its leading zeros;
#'   `VALUE` is numeric; nothing is guessed.
#' * **Corrects the sodium code on read.** A known issue affecting **version 1
#'   only**: the substance code for sodium is blank in 48,441 rows across
#'   twelve subprogrammes, the literal string `"NA"` having been consumed as a
#'   missing value before publication. It is corrected in version 2 onwards.
#'   By default this is put right on read for version 1 and left alone
#'   afterwards; see the `repair` argument.
#' * **Decodes the code lists.** `SUBST`/`PARAM`, `DETER` and `PRETRE` gain
#'   readable companion columns (`substance`, `determination`, `pretreatment`).
#' * **Parses dates.** `YYYYMM` becomes `date`, `year` and `month`. Annual
#'   values are published with month `00`; these get `month = NA` and a `date`
#'   at 1 January, rather than failing to parse.
#' * **Decodes the flags.** `FLAGSTA` becomes `stat` and `FLAGQUA` becomes
#'   `quality`. Read the note on `FLAGSTA` below before aggregating anything.
#'
#' # Column naming
#'
#' Columns in UPPERCASE are exactly as published, so they match the ICP IM
#' Manual and the dataset README one for one. Columns in lowercase are added
#' by this package.
#'
#' # `FLAGSTA` is part of the key, not an annotation
#'
#' In `AM` (meteorology) especially, the same site, level, month and parameter
#' carries several rows distinguished only by `FLAGSTA`: the monthly mean
#' (`X`), minimum (`A`), maximum (`Z`), and the averaged daily extremes
#' (`XA`, `XZ`). 47% of air- and soil-temperature keys have more than one.
#' Averaging without filtering mixes means with extremes; for `PREC` it mixes
#' monthly sums (`S`) with maximum daily sums (`SZ`) and monthly maxima (`Z`).
#' Use the `stat` argument, or filter on `stat`/`FLAGSTA` yourself.
#'
#' @param subprog Two-letter subprogramme code, e.g. `"PC"` for precipitation
#'   chemistry. See [im_subprogrammes] for the full list.
#' @param sites Optional character vector of site codes (the `AREA` column),
#'   e.g. `c("SE14", "SE15")`. Case-insensitive.
#' @param countries Optional character vector of two-letter ISO country codes,
#'   e.g. `"SE"`.
#' @param years Optional numeric vector of years to keep, e.g. `2000:2019`.
#' @param substances Optional character vector of substance or parameter codes
#'   to keep, matched against `SUBST` or `PARAM` as appropriate, e.g.
#'   `c("SO4S", "NO3N")`. Use `"NA"` for sodium.
#' @param stat Optional statistic filter, applied to `FLAGSTA`. Either raw
#'   codes (`"X"`, `"S"`) or the decoded names in `stat` (`"mean"`, `"sum"`,
#'   `"maximum"`). `"primary"` keeps only unflagged primary values.
#' @param decode Logical. Join the code lists to add readable name columns?
#' @param repair Correct the blank sodium substance code. `"auto"`, the
#'   default, applies the correction to version 1 and leaves every later
#'   version untouched, since the issue is fixed from version 2 onwards. A
#'   correctly coded file needs no repair and is passed through unchanged
#'   whatever this is set to. `TRUE` forces the correction, `FALSE` returns
#'   the file exactly as published. Under `"auto"`, blank codes found in a
#'   version that should not have them are reported rather than assumed to be
#'   sodium.
#' @param quiet Logical. Suppress progress and one-time notes.
#' @param version Dataset version. Defaults to [im_version()].
#'
#' @return A [tibble][tibble::tibble] with one row per published observation.
#' @seealso [im_widen()] to pivot to one column per substance,
#'   [im_subprogrammes] for what is available.
#' @export
#' @examples
#' \donttest{
#' if (curl::has_internet()) {
#'   # Precipitation chemistry at the four Swedish sites, this century:
#'   pc <- im_read("PC", countries = "SE", years = 2000:2019)
#'   head(pc)
#'
#'   # Sodium: the code really is the string "NA".
#'   na <- im_read("PC", substances = "NA")
#'   unique(na$substance)
#'
#'   # Monthly mean air temperature only, not the extremes:
#'   temp <- im_read("AM", substances = "TEMP", stat = "mean")
#' }
#' }
im_read <- function(subprog,
                    sites = NULL,
                    countries = NULL,
                    years = NULL,
                    substances = NULL,
                    stat = NULL,
                    decode = TRUE,
                    repair = "auto",
                    quiet = NULL,
                    version = im_version()) {
  quiet <- quiet %||% getOption("icpim.quiet", FALSE)
  code  <- resolve_subprog(subprog, version = version)
  path  <- im_local_path(code, version = version, quiet = quiet)

  repair_now <- resolve_repair(repair, version)
  out <- im_read_file(path, repair = repair_now, quiet = quiet)

  # If a release this package does not expect to be affected turns out to have
  # blank substance codes, say so rather than silently dropping them: it means
  # either the correction did not land, or these are genuinely missing codes
  # that must not be assumed to be sodium.
  n_blank <- attr(out, "icpim_blank_subst") %||% 0L
  if (identical(repair, "auto") && !repair_now && n_blank > 0) {
    cli::cli_warn(c(
      "{n_blank} row{?s} in {.field {code}} have a blank substance code, which
       version {version} was not expected to contain.",
      "i" = "In version 1 these were sodium. Do not assume that here.",
      "i" = "Use {.code repair = TRUE} to code them as sodium anyway, or
             {.code repair = FALSE} to keep them missing and silence this."
    ))
  }
  attr(out, "icpim_blank_subst") <- NULL

  # Filters, applied before decoding so the joins are as small as possible.
  if (!is.null(sites)) {
    out <- out[toupper(out$AREA) %in% toupper(sites), , drop = FALSE]
  }
  if (!is.null(countries)) {
    cc <- toupper(countries)
    # COUNTRY holds full names; AREA is prefixed with the ISO code.
    out <- out[substr(toupper(out$AREA), 1, 2) %in% cc, , drop = FALSE]
  }
  if (!is.null(years)) {
    out <- out[!is.na(out$year) & out$year %in% years, , drop = FALSE]
  }
  if (!is.null(substances)) {
    key <- im_key_col(out)
    out <- out[!is.na(out[[key]]) & out[[key]] %in% substances, , drop = FALSE]
  }
  if (!is.null(stat)) {
    out <- filter_stat(out, stat)
  }

  if (isTRUE(decode)) out <- im_decode(out, quiet = quiet, version = version)

  # Nudge on FLAGSTA once per session, only when it actually bites.
  if (!quiet && !the$warned_flagsta && "FLAGSTA" %in% names(out) &&
      is.null(stat) && has_mixed_stats(out)) {
    the$warned_flagsta <- TRUE
    cli::cli_alert_info(c(
      "{.field {code}} mixes statistics: the same site, level and month can ",
      "carry a mean, a minimum and a maximum as separate rows. ",
      "Filter on {.code stat} before aggregating."
    ))
  }

  out <- tibble::as_tibble(out)

  # Set last: row subsetting keeps attributes, but the record should report the
  # rows actually returned, and the release they came from.
  set_provenance(
    out,
    subprog          = code,
    file             = basename(path),
    dataset_version  = as.character(version),
    doi              = im_dataset_info(version)$doi,
    downloaded       = tryCatch(file.mtime(path), error = function(e) NA),
    read_at          = Sys.time(),
    package_version  = as.character(utils::packageVersion("icpim")),
    rows_read        = nrow(out),
    # Counted on what is returned, not on the file, so it cannot look larger
    # than `rows_read` after filtering. In version 1 no row carried the sodium
    # code already, so every "NA" here is one this package put back.
    sodium_corrected = if (isTRUE(repair_now) && "SUBST" %in% names(out)) {
      sum(out$SUBST == "NA", na.rm = TRUE)
    } else 0L
  )
}

#' Read one published CSV file from disk
#'
#' The reading and typing half of [im_read()], without the download, cache or
#' filtering. Use it for a file you fetched by hand, or for the bundled
#' examples in [im_example()].
#'
#' It does not decode the code lists; pass the result to [im_decode()] if you
#' want readable names.
#'
#' @param path Path to a published ICP IM CSV file.
#' @param repair Logical. Correct the blank sodium substance code, a known
#'   issue in version 1 of the deposit? Files from version 2 onwards have the
#'   code already and pass through unchanged either way. The version-aware
#'   `"auto"` setting belongs to [im_read()], which knows which release it is
#'   reading; here the choice is explicit.
#' @param quiet Logical. Suppress the note about corrected rows.
#'
#' @return A tibble, with `date`, `year` and `month` added. Carries an
#'   `icpim_blank_subst` attribute recording how many rows had a blank
#'   substance code before any correction.
#' @export
#' @examples
#' pc <- im_read_file(im_example("sample_PC.csv"))
#' str(pc[, c("AREA", "SCODE", "SUBST", "VALUE", "date")])
im_read_file <- function(path, repair = TRUE, quiet = TRUE) {
  raw <- vroom::vroom(
    path,
    delim = ",",
    col_types = vroom::cols(.default = vroom::col_character()),
    na = character(),          # nothing is missing at read time; see below
    progress = FALSE,
    show_col_types = FALSE
  )
  raw <- tibble::as_tibble(raw)

  # Restore sodium before empty strings are turned into NA, which is the step
  # that would otherwise make the repair impossible.
  #
  # Where the code is already correct - version 2 onwards - there are no blanks
  # and this is a no-op, so a correctly coded file passes through untouched.
  n_blank <- 0L
  if ("SUBST" %in% names(raw)) {
    blank <- !is.na(raw$SUBST) & !nzchar(trimws(raw$SUBST))
    n_blank <- sum(blank)
    if (isTRUE(repair) && n_blank > 0) {
      raw$SUBST[blank] <- "NA"
      if (!quiet && !the$warned_sodium) {
        the$warned_sodium <- TRUE
        cli::cli_alert_info(c(
          "Restored the sodium substance code in {n_blank} row{?s} ",
          "(blank in version 1; the code is the string {.val NA}). ",
          "Use {.code repair = FALSE} for the file exactly as published."
        ))
      }
    }
  }

  # Now apply the one NA rule the dataset README states: an empty cell means
  # not available. Codes have already been protected above.
  raw[] <- lapply(raw, function(x) {
    x <- trimws(x)
    x[!nzchar(x)] <- NA_character_
    x
  })

  for (nm in intersect(IM_NUMERIC_COLS, names(raw))) {
    raw[[nm]] <- suppressWarnings(as.numeric(raw[[nm]]))
  }

  out <- im_add_dates(raw)
  # Recorded so im_read() can tell a corrected file from an uncorrected one
  # without reading it twice.
  attr(out, "icpim_blank_subst") <- n_blank
  # A bare path says nothing about which release it came from, so those fields
  # stay unknown here; im_read() fills them in.
  set_provenance(
    out,
    subprog          = NA_character_,
    file             = basename(path),
    dataset_version  = NA_character_,
    doi              = NA_character_,
    downloaded       = tryCatch(file.mtime(path), error = function(e) NA),
    read_at          = Sys.time(),
    package_version  = as.character(utils::packageVersion("icpim")),
    rows_read        = nrow(out),
    sodium_corrected = if (isTRUE(repair)) n_blank else 0L
  )
}

# TRUE/FALSE pass through; "auto" repairs only version 1, the sole release the
# blank sodium code affects.
resolve_repair <- function(repair, version) {
  if (isTRUE(repair) || isFALSE(repair)) return(repair)
  if (identical(repair, "auto")) return(identical(as.character(version), "1"))
  cli::cli_abort('{.arg repair} must be TRUE, FALSE or "auto".')
}

# YYYYMM -> date/year/month. Month 00 marks an annual value (9,630 rows).
im_add_dates <- function(x) {
  if (!"YYYYMM" %in% names(x)) return(x)
  ym <- x$YYYYMM
  yr <- suppressWarnings(as.integer(substr(ym, 1, 4)))
  mo <- suppressWarnings(as.integer(substr(ym, 5, 6)))
  mo[!is.na(mo) & (mo < 1 | mo > 12)] <- NA_integer_
  x$year  <- yr
  x$month <- mo
  x$date  <- as.Date(sprintf(
    "%04d-%02d-01",
    ifelse(is.na(yr), 1L, yr),
    ifelse(is.na(mo), 1L, mo)
  ))
  x$date[is.na(yr)] <- as.Date(NA)
  x
}

# Which column holds the measured quantity for this subprogramme.
im_key_col <- function(x) {
  if ("SUBST" %in% names(x)) "SUBST" else "PARAM"
}

filter_stat <- function(x, stat) {
  if (!"FLAGSTA" %in% names(x)) {
    cli::cli_warn("This subprogramme has no {.field FLAGSTA}; {.arg stat} ignored.")
    return(x)
  }
  want <- as.character(stat)
  if ("primary" %in% want) {
    keep <- is.na(x$FLAGSTA)
    want <- setdiff(want, "primary")
  } else {
    keep <- rep(FALSE, nrow(x))
  }
  if (length(want)) {
    codes <- im_flags$code[im_flags$type == "FLAGSTA" & im_flags$name %in% want]
    codes <- union(codes, want)
    keep <- keep | (!is.na(x$FLAGSTA) & x$FLAGSTA %in% codes)
  }
  x[keep, , drop = FALSE]
}

has_mixed_stats <- function(x) {
  if (!"FLAGSTA" %in% names(x) || !nrow(x)) return(FALSE)
  length(unique(x$FLAGSTA[!is.na(x$FLAGSTA)])) > 1L
}
