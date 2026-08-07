# Provenance ---------------------------------------------------------------
#
# Which dataset version an analysis used cannot be inferred from the version of
# this package: `icpim.version` is a runtime option, so two people running the
# same package version can be reading different releases. The record therefore
# travels with the data.

PROV_ATTR <- "icpim_provenance"

set_provenance <- function(x, ...) {
  attr(x, PROV_ATTR) <- structure(list(...), class = "icpim_provenance")
  x
}

# Carry the record across an operation that would otherwise drop it.
copy_provenance <- function(new, old) {
  p <- attr(old, PROV_ATTR, exact = TRUE)
  if (!is.null(p)) attr(new, PROV_ATTR) <- p
  new
}

#' What this data came from
#'
#' Reports the dataset version, DOI and file behind an object returned by
#' [im_read()], together with when it was downloaded and which version of this
#' package read it.
#'
#' The version of `icpim` does not record which data you used, and cannot:
#' `icpim.version` is a runtime option, so the same package version reads any
#' release. The record is attached to the data instead, which is what makes it
#' answer the question "what did this analysis actually use".
#'
#' # What preserves it
#'
#' The record survives `[`, [dplyr::filter()], [dplyr::mutate()],
#' [dplyr::select()], [dplyr::arrange()], `head()` and `saveRDS()`, so it is
#' still there after the usual reshaping and after a saved object is reloaded.
#' [im_widen()], [im_decode()] and [im_detection_limit()] carry it forward
#' explicitly.
#'
#' It is dropped by operations that build a genuinely new table, notably
#' [dplyr::summarise()] and joins. Capture it before aggregating if you need
#' it afterwards.
#'
#' @param x An object from [im_read()].
#'
#' @return An `icpim_provenance` object: a list with `subprog`, `file`,
#'   `dataset_version`, `doi`, `downloaded`, `read_at`, `package_version` and
#'   `rows_read`. Returns `NULL`, with a warning, if `x` carries no record.
#' @export
#' @examples
#' pc <- im_read_file(im_example("sample_PC.csv"))
#' im_provenance(pc)
im_provenance <- function(x) {
  p <- attr(x, PROV_ATTR, exact = TRUE)
  if (is.null(p)) {
    cli::cli_warn(c(
      "No provenance record on this object.",
      "i" = "It is dropped by {.fn dplyr::summarise} and by joins.",
      "i" = "Objects from {.fn im_read} carry one."
    ))
    return(invisible(NULL))
  }
  p
}

#' @export
print.icpim_provenance <- function(x, ...) {
  fmt <- function(v) {
    if (is.null(v) || (length(v) == 1L && is.na(v))) "unknown" else as.character(v)
  }
  line <- function(label, value) {
    cat(sprintf("  %-16s %s\n", label, fmt(value)))
  }
  cat("ICP IM data provenance\n")
  line("subprogramme", x$subprog)
  line("file", x$file)
  line("dataset version", x$dataset_version)
  line("DOI", if (is.null(x$doi) || is.na(x$doi)) NA else paste0("https://doi.org/", x$doi))
  line("downloaded", if (is.null(x$downloaded) || is.na(x$downloaded)) NA
       else format(x$downloaded, "%Y-%m-%d %H:%M"))
  line("read at", format(x$read_at, "%Y-%m-%d %H:%M"))
  line("read by", paste("icpim", x$package_version))
  line("rows read", format(x$rows_read, big.mark = ","))
  if (!is.null(x$sodium_corrected) && !is.na(x$sodium_corrected) &&
      x$sodium_corrected > 0) {
    line("sodium corrected", paste(format(x$sodium_corrected, big.mark = ","), "rows"))
  }
  invisible(x)
}

#' @export
format.icpim_provenance <- function(x, ...) {
  paste(utils::capture.output(print(x)), collapse = "\n")
}
