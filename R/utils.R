`%||%` <- function(x, y) if (is.null(x)) y else x

# Internal accessor so package code does not depend on lazy-data promises
# being visible to R CMD check.
subprog_meta <- function() icpim::im_subprogrammes

#' Match a subprogramme code
#'
#' Accepts a two-letter code in any case, or the full file name. Errors with
#' the list of valid codes rather than a downstream parse failure.
#'
#' @param x Character.
#' @param several.ok Allow more than one, and the keyword `"all"`.
#' @return Upper-case subprogramme code(s).
#' @noRd
resolve_subprog <- function(x, several.ok = FALSE) {
  meta <- subprog_meta()
  if (!is.character(x) || !length(x)) {
    cli::cli_abort("{.arg subprog} must be a character vector of codes.")
  }
  if (several.ok && length(x) == 1L && tolower(x) == "all") {
    return(meta$subprog)
  }
  if (!several.ok && length(x) != 1L) {
    cli::cli_abort("{.arg subprog} must be a single code; got {length(x)}.")
  }
  up <- toupper(trimws(x))
  # Allow a full file name too.
  up <- ifelse(up %in% toupper(meta$file),
               meta$subprog[match(up, toupper(meta$file))],
               up)
  bad <- setdiff(up, meta$subprog)
  if (length(bad)) {
    cli::cli_abort(c(
      "Unknown subprogramme code{?s}: {.val {bad}}.",
      "i" = "Valid codes: {.val {meta$subprog}}",
      "i" = "See {.code im_subprogrammes} for what each one holds."
    ))
  }
  up
}

utils::globalVariables(c(
  "im_subprogrammes", "im_sites", "im_substances", "im_parameters",
  "im_determinations", "im_pretreatments", "im_flags"
))
