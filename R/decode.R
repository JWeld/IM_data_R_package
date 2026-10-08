#' Add readable names for the code columns
#'
#' Joins the published code lists onto a table so that `SUBST`, `PARAM`,
#' `DETER`, `PRETRE`, `FLAGSTA` and `FLAGQUA` gain readable companion columns.
#' [im_read()] calls this by default; use it directly if you read a file
#' yourself or passed `decode = FALSE`.
#'
#' Codes in the published lookup files are space-padded to a fixed width
#' (`"NA      "`, `"AOT40   "`) while the data files are not, so a naive join
#' matches almost nothing. The bundled lookups are trimmed, which is why this
#' works.
#'
#' # Two code lists, not one
#'
#' The determinand column draws on two published lists, and a companion column
#' says which: `LISTSUB` for `SUBST`, `PARLIST` for `PARAM`. `"DB"` means the
#' substance list ([im_substances]) and `"IM"` means the parameter list
#' ([im_parameters]). Roughly 26,000 rows across seven subprogrammes use `IM`
#' codes - `AOT40`, `SOL_G`, `CEC_E`, `C/N`, `BDEN` - and looking those up in
#' the substance list alone returns nothing.
#'
#' The lists overlap in 77 codes, but in 76 of them the parameter list
#' repeats a substance, tagged `"DB"`, with the same meaning. Only `ABS` means
#' two things: *Absorbance* in the substance list and *Number of branches on
#' the current tree where algae are missing* in the parameter list.
#'
#' # `ABS` in `AL` is the branch count throughout
#'
#' Decoding follows `LISTSUB`/`PARLIST` as published, and for `ABS` in `AL`
#' that gives the wrong name. Every `ABS` row there is the branch count - unit
#' `branches`, values 0 to 3 - but from 2008 the count is tagged `"DB"`, so
#' those rows (245 in versions 1 and 2) decode as *Absorbance*. Read `ABS` in
#' `AL` as the branch count whatever its `parameter` column says.
#'
#' @param x A data frame from [im_read()] or [im_read_file()].
#' @param quiet Logical. Suppress notes about codes that could not be matched.
#' @param version Dataset version the code lists should come from. Defaults to
#'   [im_version()]. Lists published with a version this package was not built
#'   against are fetched and cached automatically, so a release that adds
#'   substances decodes without any change here.
#'
#' @return `x` with added lowercase columns: `substance` or `parameter`,
#'   and where the relevant column exists, `determination`, `pretreatment`,
#'   `stat` and `quality`.
#' @export
#' @examples
#' raw <- im_read_file(im_example("sample_PC.csv"))
#' # Version 2's code lists are bundled, so naming it keeps this offline.
#' im_decode(raw, version = "2")
im_decode <- function(x, quiet = TRUE, version = im_version()) {
  stopifnot(is.data.frame(x))

  if ("SUBST" %in% names(x)) {
    x$substance <- decode_codes(x$SUBST, x[["LISTSUB"]], version = version)
    if (!quiet) report_unmatched(x$SUBST, x$substance, "substance")
  }
  if ("PARAM" %in% names(x)) {
    x$parameter <- decode_codes(x$PARAM, x[["PARLIST"]], version = version)
    if (!quiet) report_unmatched(x$PARAM, x$parameter, "parameter")
  }
  if ("DETER" %in% names(x)) {
    d <- codes_for("determinations", version)
    x$determination <- lookup(x$DETER, d$code, d$name)
  }
  if ("PRETRE" %in% names(x)) {
    p <- codes_for("pretreatments", version)
    x$pretreatment <- lookup(x$PRETRE, p$code, p$name)
  }
  if ("FLAGSTA" %in% names(x)) {
    f <- im_flags[im_flags$type == "FLAGSTA", ]
    x$stat <- lookup(x$FLAGSTA, f$code, f$name)
    x$stat[is.na(x$FLAGSTA)] <- "primary"
  }
  if ("FLAGQUA" %in% names(x)) {
    f <- im_flags[im_flags$type == "FLAGQUA", ]
    x$quality <- lookup(x$FLAGQUA, f$code, f$name)
  }
  x
}

lookup <- function(codes, from, to) {
  to[match(codes, from)]
}

# The first list that names each code, taking the next one wherever the
# previous left NA.
#
# Index assignment rather than ifelse(): ifelse() collapses a zero-length test
# to logical(0), so a table filtered down to no rows - which im_read() returns
# rather than erroring - would decode to a logical column that no longer binds
# to the character one of a non-empty read.
first_named <- function(x, ...) {
  for (y in list(...)) {
    miss <- is.na(x)
    x[miss] <- y[miss]
  }
  x
}

# The determinand column holds codes from two different published lists, and
# the companion column (LISTSUB for SUBST, PARLIST for PARAM) says which:
#
#   "DB" -> substance_codes.csv          (im_substances)
#   "IM" -> parameters_..._subprogramme  (im_parameters)
#
# Codes in the IM list are absent from the substance list, so a lookup there
# alone returns nothing for them. The lists overlap in 77 codes, but only ABS
# means two things: "Absorbance" in the substance list, "Number of branches on
# the current tree where algae are missing" in the parameter list. The tag is
# followed as published even so. In AL every ABS row is the branch count
# (unit branches, values 0-3), yet from 2008 it is tagged DB, and those rows
# decode as "Absorbance" - a fault in the published tags, documented in
# im_decode() rather than corrected here.
#
# No code carries two different meanings *within* the parameter list, so the
# parameter lookup can be flattened across subprogrammes.
decode_codes <- function(codes, lists = NULL, version = im_version()) {
  substances <- codes_for("substances", version)
  parameters <- codes_for("parameters", version)
  subst <- substances[!duplicated(substances$code), ]
  par   <- unique(parameters[, c("code", "name")])
  par   <- par[!duplicated(par$code), ]

  from_subst <- lookup(codes, subst$code, subst$name)
  from_par   <- lookup(codes, par$code, par$name)

  if (is.null(lists)) {
    # No discriminator: prefer the substance list, fall back to parameters.
    return(first_named(from_subst, from_par))
  }

  out <- from_subst              # DB list, or unstated
  im  <- !is.na(lists) & lists == "IM"
  out[im] <- from_par[im]        # IM list

  # A handful of rows name a list that does not hold the code (94 in AC);
  # falling back beats returning NA.
  first_named(out, from_subst, from_par)
}

report_unmatched <- function(codes, decoded, what) {
  bad <- unique(codes[!is.na(codes) & is.na(decoded)])
  if (length(bad)) {
    cli::cli_alert_warning(
      "{length(bad)} {what} code{?s} not in the published list: {.val {utils::head(bad, 8)}}"
    )
  }
  invisible(NULL)
}

#' Look up the published code lists
#'
#' Convenience accessor for the bundled lookup tables, which are also available
#' directly as [im_substances], [im_parameters], [im_determinations],
#' [im_pretreatments] and [im_flags].
#'
#' @param type Which list to return.
#' @param pattern Optional regular expression, matched case-insensitively
#'   against both code and name.
#' @param version Dataset version the lists should come from. Defaults to
#'   [im_version()].
#'
#' @return A tibble.
#' @export
#' @examples
#' im_codes("substance", "sodium", version = "2")   # the bundled release
#' im_codes("flag")
im_codes <- function(type = c("substance", "parameter", "determination",
                              "pretreatment", "flag"),
                     pattern = NULL,
                     version = im_version()) {
  type <- match.arg(type)
  tbl <- switch(type,
    substance     = codes_for("substances", version),
    parameter     = codes_for("parameters", version),
    determination = codes_for("determinations", version),
    pretreatment  = codes_for("pretreatments", version),
    flag          = im_flags
  )
  if (!is.null(pattern)) {
    hit <- grepl(pattern, tbl$code, ignore.case = TRUE) |
      grepl(pattern, tbl$name, ignore.case = TRUE)
    tbl <- tbl[hit, , drop = FALSE]
  }
  tbl
}
