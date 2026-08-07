#' The ICP IM subprogrammes
#'
#' One row per subprogramme in the published deposit. Row counts, site counts
#' and year ranges are measured from version 1, not asserted.
#'
#' The `key` column records which column holds the measured quantity:
#' chemistry subprogrammes use `SUBST` and biological ones use `PARAM`. The
#' `collection` column is the `SUBPROG_COLLECTION` value used in the files,
#' which corresponds to the reporting formats in the ICP IM Manual (`CHEM`,
#' and `BIO1`/`BIO2` for the B1 and B2 formats).
#'
#' @format A tibble with 21 rows and 10 columns:
#' \describe{
#'   \item{subprog}{Two-letter subprogramme code}
#'   \item{name}{Descriptive name}
#'   \item{file}{File name in the deposit}
#'   \item{collection}{Reporting format group: `CHEM`, `BIO1` or `BIO2`}
#'   \item{key}{Column holding the determinand: `SUBST` or `PARAM`}
#'   \item{size_mb}{Approximate download size}
#'   \item{n_rows}{Rows in version 1}
#'   \item{n_sites}{Distinct sites in version 1}
#'   \item{first_year, last_year}{Year range in version 1}
#' }
#' @source \doi{10.5878/z376-2m63}
#' @examples
#' im_subprogrammes[, c("subprog", "name", "n_rows")]
"im_subprogrammes"

#' ICP IM monitoring sites
#'
#' Site codes, names and coordinates, from `IM_sites_info.csv` in the deposit.
#' Join to data with `AREA` (in the data) against `area` (here).
#'
#' Note that the deposit lists 55 sites while any one subprogramme covers
#' fewer; not every site reports every subprogramme.
#'
#' @format A tibble with 55 rows and 6 columns:
#' \describe{
#'   \item{area}{Site code, e.g. `"SE14"`. Matches `AREA` in the data files}
#'   \item{country}{Two-letter ISO country code}
#'   \item{name}{Site name}
#'   \item{latitude, longitude}{Decimal degrees, WGS 84}
#'   \item{active}{Whether the site was still active at publication}
#' }
#' @source \doi{10.5878/z376-2m63}
#' @examples
#' subset(im_sites, country == "SE")
"im_sites"

#' Substance codes
#'
#' The published substance code list, trimmed of the fixed-width padding that
#' the source file carries.
#'
#' Note that sodium's code is the literal string `"NA"`. It survives here
#' because the build script reads with no `NA` strings at all; the same care
#' is needed in any code that reads the deposit directly.
#'
#' @format A tibble with 1,606 rows and 5 columns:
#' \describe{
#'   \item{code}{Substance code, as used in the `SUBST` column}
#'   \item{name}{Substance name}
#'   \item{group}{Numeric group identifier}
#'   \item{cas}{CAS registry number, where known}
#'   \item{description}{Free-text description, where given}
#' }
#' @source \doi{10.5878/z376-2m63}
#' @examples
#' subset(im_substances, code == "NA")
"im_substances"

#' Parameter codes by subprogramme
#'
#' Parameter codes for the biological subprogrammes, with the unit and
#' plausible range published for each.
#'
#' @format A tibble with 544 rows and 8 columns:
#' \describe{
#'   \item{subprog}{Subprogramme the parameter belongs to}
#'   \item{subprog_name}{Descriptive subprogramme name}
#'   \item{code}{Parameter code, as used in the `PARAM` column}
#'   \item{name}{Parameter name}
#'   \item{list}{Parameter list the code belongs to}
#'   \item{unit}{Suggested unit}
#'   \item{minimum, maximum}{Plausible range, where published}
#' }
#' @source \doi{10.5878/z376-2m63}
#' @examples
#' subset(im_parameters, subprog == "VG")
"im_parameters"

#' Determination method codes
#'
#' @format A tibble with 3 columns: `code`, `name` and `note`.
#' @source \doi{10.5878/z376-2m63}
"im_determinations"

#' Pretreatment method codes
#'
#' @format A tibble with 2 columns: `code` and `name`.
#' @source \doi{10.5878/z376-2m63}
"im_pretreatments"

#' Quality and status flags
#'
#' The `FLAGQUA` and `FLAGSTA` vocabularies.
#'
#' The deposit's `README.txt` lists five `FLAGSTA` codes. The AM (meteorology)
#' section of the ICP IM Manual defines five more - `A`, `Z`, `XA`, `XZ` and
#' `SZ` - and these do occur, in 36,522 rows of version 1. All ten are
#' recorded here.
#'
#' `FLAGSTA` is part of the observation key rather than an annotation: the
#' same site, level, month and parameter can carry a mean, a minimum and a
#' maximum as separate rows. See [im_read()].
#'
#' @format A tibble with 13 rows and 4 columns:
#' \describe{
#'   \item{type}{`"FLAGQUA"` or `"FLAGSTA"`}
#'   \item{code}{The flag as it appears in the data}
#'   \item{name}{Short readable name, used for the `stat` and `quality` columns}
#'   \item{description}{Fuller definition}
#' }
#' @source ICP IM Manual edition 8, sections 4.3.2 and 7.2.5.
#' @examples
#' subset(im_flags, type == "FLAGSTA")
"im_flags"
