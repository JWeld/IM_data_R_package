#' Small extracts of the real data, bundled for offline use
#'
#' Three genuine extracts from version 1 of the deposit, small enough to ship
#' with the package. They exist so that examples, tests and the vignette run
#' without a network connection, and because each one carries a trap worth
#' demonstrating:
#'
#' * `sample_PC.csv` - precipitation chemistry, site SE14, 2015-2019. Contains
#'   60 rows carrying the version 1 sodium issue, where the substance code is
#'   blank, and a station code of `"0176"` that must keep its leading zero.
#' * `sample_AM.csv` - meteorology, site AT01, 2002. Air and soil temperature
#'   and precipitation, carrying all of `X`, `A`, `Z`, `XA`, `XZ`, `S` and `SZ`,
#'   so the same key appears five times over.
#' * `sample_BB.csv` - the complete bird subprogramme, 71 rows, every one of
#'   them an annual value published with month `00`.
#' * `sample_BI.csv` - tree bioelements, the three sampling occasions carrying
#'   the version 1 sodium issue in `PARAM` rather than `SUBST`. Sodium is
#'   measured in tree biomass too, so the blank code is not confined to the
#'   chemistry subprogrammes.
#'
#' @param file Name of the example file. If `NULL`, the available names are
#'   returned.
#'
#' @return A file path, or a character vector of available names.
#' @export
#' @examples
#' im_example()
#' pc <- im_read_file(im_example("sample_PC.csv"))
#' # Sodium restored, and the station code kept its leading zero:
#' unique(pc$SCODE)
#' sum(pc$SUBST == "NA", na.rm = TRUE)
im_example <- function(file = NULL) {
  dir <- system.file("extdata", package = "icpim")
  if (is.null(file)) return(list.files(dir, pattern = "\\.csv$"))
  path <- file.path(dir, file)
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "No example file {.val {file}}.",
      "i" = "Available: {.val {list.files(dir, pattern = '\\\\.csv$')}}"
    ))
  }
  path
}
