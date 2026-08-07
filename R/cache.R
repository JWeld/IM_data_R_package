#' Where `icpim` stores downloaded files
#'
#' The published files total roughly 95 MB, so they are downloaded once and
#' cached rather than shipped with the package. The cache lives under
#' [tools::R_user_dir()] and is keyed by dataset version, so pinning a
#' different version with `options(icpim.version=)` never mixes files from
#' two releases.
#'
#' Set `options(icpim.cache_dir = "/some/path")` to override, or the
#' environment variable `ICPIM_CACHE_DIR`. Using a project-local directory is
#' a good way to make an analysis self-contained.
#'
#' @param version Dataset version. Defaults to [im_version()].
#' @param create Logical. Create the directory if it does not exist?
#'
#' @return The cache path, as a character vector of length one.
#' @export
#' @examples
#' # Where files would be stored (does not create anything):
#' im_cache_dir(create = FALSE)
im_cache_dir <- function(version = im_version(), create = FALSE) {
  root <- getOption("icpim.cache_dir", Sys.getenv("ICPIM_CACHE_DIR", ""))
  if (!nzchar(root)) root <- tools::R_user_dir("icpim", which = "cache")
  path <- file.path(root, paste0("v", version))
  if (isTRUE(create) && !dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

#' List and clear cached files
#'
#' @param version Dataset version. Defaults to [im_version()].
#'
#' @return `im_cache_list()` returns a tibble with one row per cached file and
#'   columns `file`, `size_mb` and `modified`; it has zero rows if nothing is
#'   cached. `im_cache_clear()` returns, invisibly, the paths it removed.
#' @export
#' @examples
#' im_cache_list()
im_cache_list <- function(version = im_version()) {
  dir <- im_cache_dir(version, create = FALSE)
  files <- if (dir.exists(dir)) {
    list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  } else {
    character()
  }
  info <- file.info(files)
  tibble::tibble(
    file     = basename(files),
    size_mb  = round(info$size / 1024^2, 2),
    modified = info$mtime
  )
}

#' @rdname im_cache_list
#' @param confirm Logical. If `TRUE` (the default in an interactive session),
#'   ask before deleting. Set `FALSE` to clear without prompting.
#' @export
im_cache_clear <- function(version = im_version(), confirm = interactive()) {
  dir <- im_cache_dir(version, create = FALSE)
  if (!dir.exists(dir)) {
    cli::cli_alert_info("Nothing cached for version {version}.")
    return(invisible(character()))
  }
  files <- list.files(dir, full.names = TRUE)
  if (!length(files)) {
    cli::cli_alert_info("Nothing cached for version {version}.")
    return(invisible(character()))
  }
  size <- round(sum(file.info(files)$size) / 1024^2, 1)
  if (isTRUE(confirm)) {
    cli::cli_alert_warning(
      "About to delete {length(files)} file{?s} ({size} MB) from {.path {dir}}."
    )
    ans <- readline("Proceed? [y/N] ")
    if (!tolower(trimws(ans)) %in% c("y", "yes")) {
      cli::cli_alert_info("Cancelled; nothing deleted.")
      return(invisible(character()))
    }
  }
  unlink(files)
  cli::cli_alert_success("Removed {length(files)} file{?s} ({size} MB).")
  invisible(files)
}
