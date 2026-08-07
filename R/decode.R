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
#' It also matters where the lists overlap. 77 codes appear in both, and `ABS`
#' means *Absorbance* in one and *Number of branches on the current tree where
#' algae are missing* in the other. Both readings occur in `AL`, in the same
#' file, told apart only by `PARLIST`.
#'
#' @param x A data frame from [im_read()] or [im_read_file()].
#' @param quiet Logical. Suppress notes about codes that could not be matched.
#'
#' @return `x` with added lowercase columns: `substance` or `parameter`,
#'   and where the relevant column exists, `determination`, `pretreatment`,
#'   `stat` and `quality`.
#' @export
#' @examples
#' \donttest{
#' if (curl::has_internet()) {
#'   raw <- im_read("MC", decode = FALSE)
#'   im_decode(raw)
#' }
#' }
im_decode <- function(x, quiet = TRUE) {
  stopifnot(is.data.frame(x))

  if ("SUBST" %in% names(x)) {
    x$substance <- decode_codes(x$SUBST, x[["LISTSUB"]])
    if (!quiet) report_unmatched(x$SUBST, x$substance, "substance")
  }
  if ("PARAM" %in% names(x)) {
    x$parameter <- decode_codes(x$PARAM, x[["PARLIST"]])
    if (!quiet) report_unmatched(x$PARAM, x$parameter, "parameter")
  }
  if ("DETER" %in% names(x)) {
    x$determination <- lookup(x$DETER, im_determinations$code, im_determinations$name)
  }
  if ("PRETRE" %in% names(x)) {
    x$pretreatment <- lookup(x$PRETRE, im_pretreatments$code, im_pretreatments$name)
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

# The determinand column holds codes from two different published lists, and
# the companion column (LISTSUB for SUBST, PARLIST for PARAM) says which:
#
#   "DB" -> substance_codes.csv          (im_substances)
#   "IM" -> parameters_..._subprogramme  (im_parameters)
#
# This matters rather than being pedantry: 77 codes appear in both lists, and
# `ABS` means "Absorbance" in one and "Number of branches on the current tree
# where algae are missing" in the other. Both occur in AL, in the same file,
# distinguished only by PARLIST.
#
# No code carries two different meanings *within* the parameter list, so the
# parameter lookup can be flattened across subprogrammes.
decode_codes <- function(codes, lists = NULL) {
  subst <- im_substances[!duplicated(im_substances$code), ]
  par   <- unique(im_parameters[, c("code", "name")])
  par   <- par[!duplicated(par$code), ]

  from_subst <- lookup(codes, subst$code, subst$name)
  from_par   <- lookup(codes, par$code, par$name)

  if (is.null(lists)) {
    # No discriminator: prefer the substance list, fall back to parameters.
    return(ifelse(is.na(from_subst), from_par, from_subst))
  }

  out <- ifelse(
    !is.na(lists) & lists == "IM",
    from_par,     # IM list
    from_subst    # DB list, or unstated
  )
  # A handful of rows name a list that does not hold the code (94 in AC);
  # falling back beats returning NA.
  ifelse(is.na(out), ifelse(is.na(from_subst), from_par, from_subst), out)
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
#'
#' @return A tibble.
#' @export
#' @examples
#' im_codes("substance", "sodium")
#' im_codes("flag")
im_codes <- function(type = c("substance", "parameter", "determination",
                              "pretreatment", "flag"),
                     pattern = NULL) {
  type <- match.arg(type)
  tbl <- switch(type,
    substance     = im_substances,
    parameter     = im_parameters,
    determination = im_determinations,
    pretreatment  = im_pretreatments,
    flag          = im_flags
  )
  if (!is.null(pattern)) {
    hit <- grepl(pattern, tbl$code, ignore.case = TRUE) |
      grepl(pattern, tbl$name, ignore.case = TRUE)
    tbl <- tbl[hit, , drop = FALSE]
  }
  tbl
}
