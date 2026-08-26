#' Measure what a release actually contains
#'
#' Row counts, site counts and year ranges are properties of a particular
#' release, so they are measured from the data rather than recorded in this
#' package. That means they stay right when a new version is published.
#'
#' Files are downloaded and cached as needed, so the first call over `"all"`
#' fetches the whole deposit (about 95 MB). Later calls read from the cache.
#'
#' @param subprog Subprogramme code(s), or `"all"`.
#' @param version Dataset version. Defaults to [im_version()].
#' @param quiet Logical. Suppress progress messages.
#'
#' @return A tibble with one row per subprogramme: `subprog`, `name`,
#'   `n_rows`, `n_sites`, `first_year`, `last_year`, `n_determinands` and
#'   `size_mb`.
#' @seealso [im_manifest()] for the file list without downloading anything.
#' @export
#' @examples
#' \donttest{
#' if (curl::has_internet()) {
#'   # A temporary cache, so the example leaves nothing behind. In normal use
#'   # leave the default, which persists between sessions.
#'   op <- options(icpim.cache_dir = tempfile())
#'
#'   # One small subprogramme
#'   try(im_coverage("MC"))
#'   options(op)
#' }
#' }
im_coverage <- function(subprog = "all", version = im_version(), quiet = NULL) {
  quiet <- quiet %||% getOption("icpim.quiet", FALSE)
  codes <- resolve_subprog(subprog, several.ok = TRUE, version = version)
  meta  <- known_subprogs(version)

  man <- suppressWarnings(tryCatch(im_manifest(version, "data"), error = function(e) NULL))

  rows <- lapply(codes, function(code) {
    x <- im_read(code, version = version, decode = FALSE, quiet = TRUE)
    key <- im_key_col(x)
    tibble::tibble(
      subprog        = code,
      name           = meta$name[match(code, meta$subprog)],
      n_rows         = nrow(x),
      n_sites        = length(unique(x$AREA)),
      first_year     = suppressWarnings(min(x$year, na.rm = TRUE)),
      last_year      = suppressWarnings(max(x$year, na.rm = TRUE)),
      n_determinands = length(unique(x[[key]][!is.na(x[[key]])])),
      size_mb        = if (is.null(man)) NA_real_ else man$size_mb[match(code, man$subprog)]
    )
  })
  out <- do.call(rbind, rows)
  out$first_year[!is.finite(out$first_year)] <- NA_integer_
  out$last_year[!is.finite(out$last_year)]   <- NA_integer_
  out
}
