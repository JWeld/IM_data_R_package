# Constants describing the published deposit -----------------------------

# Each release has its own DOI. The one below belongs to version 1, the
# release the bundled tables were built from; every other version's DOI is
# looked up, never assumed. The concept DOI is version-independent and always
# resolves to the newest release.
IM_DOI_V1      <- "10.5878/z376-2m63"
IM_DOI_CONCEPT <- "10.5878/x6fn-gw26"
IM_DATASET_ID  <- "2024-180"

# Facts about version 1, so citing the pinned version works offline.
IM_V1_INFO <- list(
  title = paste(
    "The International Cooperative Programme on Integrated Monitoring of",
    "Air Pollution Effects on Ecosystems (ICP IM)"
  ),
  doi       = IM_DOI_V1,
  version   = "1",
  year      = "2026",
  publisher = "Swedish University of Agricultural Sciences",
  licence   = "CC BY 4.0"
)

IM_API_BASE  <- "https://api.researchdata.se/dataset"
IM_FILE_BASE <- "https://doris.snd.se/api/file"

#' Identifiers for the underlying dataset
#'
#' Returns the DOI, identifier and descriptive metadata for the dataset version
#' currently being read.
#'
#' Every release has its own DOI, so these follow [im_version()]. Only version
#' 1 is known offline, because that is the release this package was built
#' against; any other version is looked up in the repository. If it cannot be
#' reached, `doi` is `NA` rather than a DOI belonging to a different release -
#' citing the wrong version is worse than admitting the value is unknown.
#'
#' Use `concept = TRUE` for the concept DOI, which is version-independent and
#' always resolves to the newest release. Cite the version you actually
#' analysed, not the concept DOI.
#'
#' @param version Dataset version. Defaults to [im_version()].
#' @param concept Logical. Return the concept DOI (always latest) instead of
#'   the DOI of this particular version?
#'
#' @return A length-one character vector (`im_doi()`), possibly `NA`; or a list
#'   (`im_dataset_info()`).
#' @export
#' @examples
#' im_doi()
#' im_doi(concept = TRUE)
#' im_dataset_info()$title
im_doi <- function(version = im_version(), concept = FALSE) {
  if (isTRUE(concept)) return(IM_DOI_CONCEPT)
  info <- im_dataset_info(version)
  if (is.na(info$doi)) {
    cli::cli_warn(c(
      "The DOI for version {.val {version}} could not be determined.",
      "i" = "Only version {.val {IM_BUNDLED_VERSION}} is known without a
             network connection."
    ))
  }
  info$doi
}

#' @rdname im_doi
#' @export
im_dataset_info <- function(version = im_version()) {
  version <- as.character(version)
  paper <- "https://doi.org/10.1038/s41597-026-07181-8"

  base <- if (identical(version, IM_BUNDLED_VERSION)) {
    IM_V1_INFO
  } else {
    d <- im_api_dataset(version)
    if (is.null(d)) {
      # Unknown rather than wrong.
      list(title = IM_V1_INFO$title, doi = NA_character_, version = version,
           year = NA_character_, publisher = IM_V1_INFO$publisher,
           licence = IM_V1_INFO$licence)
    } else {
      list(
        title     = d$title$en %||% IM_V1_INFO$title,
        doi       = as.character(d$doi),
        version   = as.character(d$versionNumber),
        year      = substr(as.character(d$publishedDate), 1, 4),
        publisher = d$principal$name$en %||% IM_V1_INFO$publisher,
        licence   = IM_V1_INFO$licence
      )
    }
  }

  c(base, list(
    dataset_id  = IM_DATASET_ID,
    concept_doi = IM_DOI_CONCEPT,
    url         = if (is.na(base$doi)) NA_character_ else paste0("https://doi.org/", base$doi),
    paper       = paper
  ))
}

#' Cite the ICP IM dataset
#'
#' The dataset is published under CC BY 4.0, which requires attribution. This
#' prints the citation for the data itself, which is *not* the same as the
#' citation for this package (see `citation("icpim")`).
#'
#' The citation is for the version being read, since each release has its own
#' DOI. If that version's DOI cannot be established, this says so instead of
#' printing a citation that points at the wrong release.
#'
#' @param version Dataset version. Defaults to [im_version()].
#'
#' @return The citation, invisibly, as a character vector. Called for its
#'   side effect of printing.
#' @export
#' @examples
#' im_cite()
im_cite <- function(version = im_version()) {
  info <- im_dataset_info(version)

  if (is.na(info$doi)) {
    txt <- c(
      paste0("Cannot cite version ", version, ": its DOI is not known here."),
      paste0("Only version ", IM_BUNDLED_VERSION,
             " is known offline; the rest are looked up in the repository."),
      "Connect to the network and try again, or take the DOI from",
      paste0("https://doi.org/", IM_DOI_CONCEPT, ", which resolves to the newest release.")
    )
    cat(txt, sep = "\n")
    return(invisible(txt))
  }

  txt <- c(
    paste0(
      "Weldon, J. et al. (", info$year, "). ", info$title,
      ", version ", info$version, ". ", info$publisher,
      ". https://doi.org/", info$doi
    ),
    "",
    paste0(
      "Accompanying paper: A long-term ecosystem monitoring dataset from the ",
      "ICP Integrated Monitoring network: biogeochemical data from 1977-2020 ",
      "across 14 European countries. Scientific Data 13 (2026). ",
      info$paper
    ),
    "",
    "Licensed CC BY 4.0. Attribution is required.",
    "For the package itself, see citation(\"icpim\")."
  )
  cat(txt, sep = "\n")
  invisible(txt)
}

# File URL construction ---------------------------------------------------

im_file_url <- function(file, type = c("data", "documentation"),
                        version = im_version()) {
  type <- match.arg(type)
  sprintf(
    "%s/%s/%s/%s?filePath=%s",
    IM_FILE_BASE, IM_DATASET_ID, version, type,
    utils::URLencode(file, reserved = TRUE)
  )
}

#' The dataset version this session reads
#'
#' The deposit is updated annually and each update is a new version with its
#' own DOI. `icpim` pins version 1 by default so that results stay
#' reproducible; set `options(icpim.version = "2")` to move to a later one.
#'
#' Changing the version changes the cache key, so files are re-downloaded
#' rather than silently mixed across versions.
#'
#' @return A length-one character vector.
#' @export
#' @examples
#' im_version()
im_version <- function() {
  as.character(getOption("icpim.version", "1"))
}
