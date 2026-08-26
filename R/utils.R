`%||%` <- function(x, y) if (is.null(x)) y else x

# One character value from an API field. The repository's answers are not
# under this package's control, and a record can arrive without a field: NULL
# or zero-length must become NA rather than character(0), which turns a later
# `if (is.na(x))` into "argument is of length zero". Empty strings count as
# absent too.
chr1 <- function(x) {
  x <- tryCatch(as.character(x), error = function(e) NA_character_)
  if (!length(x) || is.na(x[[1]]) || !nzchar(x[[1]])) NA_character_ else x[[1]]
}

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
resolve_subprog <- function(x, several.ok = FALSE, version = im_version()) {
  meta <- known_subprogs(version)
  if (!is.character(x) || !length(x)) {
    cli::cli_abort("{.arg subprog} must be a character vector of codes.")
  }
  # identical() rather than ==: tolower(NA) == "all" is NA, which reaches if()
  # as an NA condition and errors before the validation below can report the
  # bad code.
  if (several.ok && length(x) == 1L && identical(tolower(x), "all")) {
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

# Subprogrammes available in a given release: the bundled catalogue for the
# version it was built from, otherwise whatever the repository lists, so a
# release that adds a subprogramme works without changing this package.
known_subprogs <- function(version = im_version()) {
  meta <- subprog_meta()
  if (identical(as.character(version), IM_BUNDLED_VERSION)) return(meta)
  man <- suppressWarnings(tryCatch(
    im_manifest(version, "data"),
    error = function(e) NULL
  ))
  if (is.null(man) || !nrow(man)) return(meta)
  man <- man[!is.na(man$subprog), c("subprog", "name", "file")]
  # Same columns whichever branch we came down, so a caller cannot read a
  # column that happens to exist only for the bundled version. A subprogramme
  # new in this release gets NA rather than a missing column.
  i <- match(man$subprog, meta$subprog)
  man$collection <- meta$collection[i]
  man$key        <- meta$key[i]
  man[, names(meta)]
}

# The published file name for a subprogramme in a given release.
subprog_file <- function(code, version = im_version()) {
  meta <- known_subprogs(version)
  meta$file[match(code, meta$subprog)]
}

utils::globalVariables(c(
  "im_subprogrammes", "im_sites", "im_substances", "im_parameters",
  "im_determinations", "im_pretreatments", "im_flags"
))
