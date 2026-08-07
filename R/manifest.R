# Constants describing the published deposit -----------------------------

# The version DOI (this package targets version 1) and the concept DOI, which
# always resolves to the most recent version. The dataset is updated annually,
# so these are deliberately kept apart: analyses should pin a version.
IM_DOI_VERSION <- "10.5878/z376-2m63"
IM_DOI_CONCEPT <- "10.5878/x6fn-gw26"
IM_DATASET_ID  <- "2024-180"

IM_API_BASE  <- "https://api.researchdata.se/dataset"
IM_FILE_BASE <- "https://doris.snd.se/api/file"

#' Identifiers for the underlying dataset
#'
#' Returns the DOIs, dataset identifier and citation for the deposit that this
#' package reads.
#'
#' `im_doi()` gives the *version* DOI by default. Use `concept = TRUE` for the
#' concept DOI, which always resolves to the newest version. Cite the version
#' you actually analysed.
#'
#' @param concept Logical. Return the concept DOI (always latest) instead of
#'   the version DOI?
#'
#' @return A length-one character vector (`im_doi()`), or a list (`im_dataset_info()`).
#' @export
#' @examples
#' im_doi()
#' im_doi(concept = TRUE)
#' im_dataset_info()$title
im_doi <- function(concept = FALSE) {
  if (isTRUE(concept)) IM_DOI_CONCEPT else IM_DOI_VERSION
}

#' @rdname im_doi
#' @export
im_dataset_info <- function() {
  list(
    title = paste(
      "The International Cooperative Programme on Integrated Monitoring of",
      "Air Pollution Effects on Ecosystems (ICP IM)"
    ),
    dataset_id  = IM_DATASET_ID,
    doi         = IM_DOI_VERSION,
    concept_doi = IM_DOI_CONCEPT,
    version     = "1",
    publisher   = "Swedish University of Agricultural Sciences",
    licence     = "CC BY 4.0",
    url         = paste0("https://doi.org/", IM_DOI_VERSION),
    paper       = "https://doi.org/10.1038/s41597-026-07181-8"
  )
}

#' Cite the ICP IM dataset
#'
#' The dataset is published under CC BY 4.0, which requires attribution. This
#' prints the citation for the data itself, which is *not* the same as the
#' citation for this package (see `citation("icpim")`).
#'
#' @return The citation, invisibly, as a character vector. Called for its
#'   side effect of printing.
#' @export
#' @examples
#' im_cite()
im_cite <- function() {
  info <- im_dataset_info()
  txt <- c(
    paste0(
      "Weldon, J. et al. (2026). ", info$title,
      ", version ", info$version, ". Swedish University of Agricultural ",
      "Sciences. https://doi.org/", info$doi
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
