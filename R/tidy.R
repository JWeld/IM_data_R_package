#' Pivot to one column per substance or parameter
#'
#' The published data are long: one row per measured value. Most analyses want
#' one row per sample and one column per determinand. This pivots safely.
#'
#' The identifying columns default to everything that distinguishes a sample —
#' including `FLAGSTA`, which matters: in `AM` the same site, level and month
#' carries a mean, a minimum and a maximum as separate rows, and dropping the
#' flag from the key silently collapses them. If a key still has duplicates,
#' this errors rather than quietly taking the first value; `values_fn` gives
#' you a way to resolve them deliberately.
#'
#' Units are dropped from the value columns because they vary between rows.
#' Check them first with [im_units()] and convert if needed.
#'
#' @param x A tibble from [im_read()].
#' @param names_from Column supplying the new column names. Defaults to
#'   `SUBST` or `PARAM`, whichever the subprogramme uses.
#' @param values_from Column supplying the values. Defaults to `VALUE`.
#' @param id_cols Optional character vector of columns to keep as identifiers.
#'   Defaults to the sample-identifying columns present in `x`.
#' @param values_fn Optional function to combine duplicate values, e.g. `mean`.
#'   Without it, duplicates are an error.
#'
#' @return A wider tibble.
#' @export
#' @examples
#' \donttest{
#' if (curl::has_internet()) {
#'   pc <- im_read("PC", sites = "SE14", years = 2015:2019)
#'   im_widen(pc)
#' }
#' }
im_widen <- function(x,
                     names_from = NULL,
                     values_from = "VALUE",
                     id_cols = NULL,
                     values_fn = NULL) {
  stopifnot(is.data.frame(x))
  names_from <- names_from %||% im_key_col(x)

  if (is.null(id_cols)) {
    candidates <- c(
      "COUNTRY", "SUBPROG", "AREA", "INST", "SCODE", "MEDIUM", "LEVEL",
      "YYYYMM", "DAY", "date", "year", "month", "SPOOL", "FLAGSTA", "stat",
      "TREE_OR_QUARTER", "CLASS", "NAME_GBIF", "MEDIUM_NAME_GBIF"
    )
    id_cols <- intersect(candidates, names(x))
  }
  id_cols <- setdiff(id_cols, c(names_from, values_from))

  dup <- sum(duplicated(x[, c(id_cols, names_from), drop = FALSE]))
  if (dup > 0 && is.null(values_fn)) {
    cli::cli_abort(c(
      "{dup} duplicate key{?s} would collapse silently.",
      "i" = "Add columns to {.arg id_cols}, or set {.arg values_fn} (e.g. {.code mean}).",
      "i" = "In {.field AM} the usual cause is {.field FLAGSTA}: filter with {.code stat=} first."
    ))
  }

  tidyr::pivot_wider(
    x,
    id_cols = dplyr::all_of(id_cols),
    names_from = dplyr::all_of(names_from),
    values_from = dplyr::all_of(values_from),
    values_fn = values_fn
  )
}

#' Report the units used for each determinand
#'
#' Units are published per row and are not always consistent within a
#' subprogramme: foliage chemistry, for instance, reports the same elements in
#' both `mg/kg` and `ug/g`. Check this before pivoting or aggregating.
#'
#' @param x A tibble from [im_read()].
#'
#' @return A tibble with one row per determinand and unit, with counts. A
#'   determinand appearing more than once uses more than one unit.
#' @export
#' @examples
#' \donttest{
#' if (curl::has_internet()) {
#'   im_units(im_read("FC"))
#' }
#' }
im_units <- function(x) {
  stopifnot(is.data.frame(x))
  key <- im_key_col(x)
  out <- dplyr::count(x, .data[[key]], .data$UNIT, name = "n")
  out <- dplyr::arrange(out, .data[[key]], dplyr::desc(.data$n))
  dplyr::group_by(out, .data[[key]]) |>
    dplyr::mutate(n_units = dplyr::n()) |>
    dplyr::ungroup()
}

#' Drop or mark values below the detection limit
#'
#' Values below detection are published as the detection limit itself with
#' `FLAGQUA == "L"`. Treating them as measurements biases means upward; simply
#' dropping them biases downward. This makes the choice explicit.
#'
#' @param x A tibble from [im_read()].
#' @param action What to do with below-detection values: `"half"` replaces them
#'   with half the reported detection limit (the usual convention),
#'   `"drop"` removes the rows, `"na"` sets `VALUE` to `NA`, and `"keep"`
#'   leaves them but is useful together with `estimated`.
#' @param estimated Logical. Also drop values flagged as estimated (`"E"`)?
#'
#' @return A tibble.
#' @export
#' @examples
#' \donttest{
#' if (curl::has_internet()) {
#'   pc <- im_read("PC", sites = "SE14")
#'   im_detection_limit(pc, action = "half")
#' }
#' }
im_detection_limit <- function(x, action = c("half", "drop", "na", "keep"),
                               estimated = FALSE) {
  action <- match.arg(action)
  stopifnot(is.data.frame(x))
  if (!"FLAGQUA" %in% names(x)) {
    cli::cli_warn("No {.field FLAGQUA} column; nothing to do.")
    return(x)
  }
  below <- !is.na(x$FLAGQUA) & x$FLAGQUA == "L"
  x <- switch(action,
    half = {
      x$VALUE[below] <- x$VALUE[below] / 2
      x
    },
    drop = x[!below, , drop = FALSE],
    na   = {
      x$VALUE[below] <- NA_real_
      x
    },
    keep = x
  )
  if (isTRUE(estimated)) {
    x <- x[is.na(x$FLAGQUA) | x$FLAGQUA != "E", , drop = FALSE]
  }
  x
}
