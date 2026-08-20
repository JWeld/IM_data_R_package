# Build the bundled lookup tables from the published documentation files.
#
# Run with:  Rscript data-raw/make_data.R
#
# The published lookups need three repairs before they are usable:
#   1. Codes are space-padded to fixed width ("NA      ") but the data files
#      are not, so an untrimmed join matches nothing.
#   2. The files carry a UTF-8 BOM, which otherwise ends up in the first
#      column name.
#   3. Missing values are the literal string "NULL".
#
# Sodium's code is the string "NA", so nothing here may use a reader that
# treats "NA" as missing. read_doc_csv() sets `na = character()` deliberately.
#
# Rerun this only when the *stable* metadata changes: a renamed file, a new
# subprogramme, or a correction to the code lists that you want bundled.
# Row counts, sizes and year ranges are no longer recorded here - they are
# measured at run time by im_manifest() and im_coverage() - so an ordinary
# annual release needs no change to this script.

library(tibble)
pkgload::load_all(quiet = TRUE)   # for read_doc_csv() and IM_BUNDLED_VERSION

doc_url <- function(f) {
  sprintf(
    "https://doris.snd.se/api/file/2024-180/%s/documentation?filePath=%s",
    IM_BUNDLED_VERSION, utils::URLencode(f, reserved = TRUE)
  )
}

# vroom rather than read.csv: it strips the BOM, and it reads UTF-8 regardless
# of the session locale. read.csv(fileEncoding = "UTF-8-BOM") silently
# truncates these files to 41 rows under a C locale, because it tries to
# transcode the Scandinavian site names into the native encoding and fails.
read_doc <- function(f) {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  utils::download.file(doc_url(f), tmp, quiet = TRUE, mode = "wb")
  # The package's own reader, so the build and the runtime cannot drift apart.
  read_doc_csv(tmp)
}

# Rows with no code cannot be joined to anything, so they are discarded - but
# not silently. A malformed future release would otherwise lose rows here with
# nothing in the build log to show for it.
drop_uncoded <- function(x, what) {
  keep <- !is.na(x$code)
  dropped <- sum(!keep)
  message("  ", what, ": ", sum(keep), " kept, ", dropped, " dropped (no code)")
  x[keep, , drop = FALSE]
}

# Substances --------------------------------------------------------------
subs <- read_doc("substance_codes.csv")
im_substances <- tibble(
  code        = subs$SubstanceCode,
  name        = subs$Name,
  group       = suppressWarnings(as.integer(subs$Group)),
  cas         = subs$CASnumber,
  description = subs$Description
)
im_substances <- drop_uncoded(im_substances, "substances")
stopifnot("NA" %in% im_substances$code)  # sodium survived
stopifnot(im_substances$name[im_substances$code == "NA"] == "Sodium")

# Parameters by subprogramme ----------------------------------------------
par <- read_doc("parameters_and_codes_by_subprogramme.csv")
im_parameters <- tibble(
  subprog      = par$Subprogramme,
  subprog_name = par$SubprogName,
  code         = par$Parameter,
  name         = par$ParamName,
  list         = par$ParamList,
  unit         = par$Unit,
  minimum      = suppressWarnings(as.numeric(par$Minimum)),
  maximum      = suppressWarnings(as.numeric(par$Maximum))
)
im_parameters <- drop_uncoded(im_parameters, "parameters")

# Determination and pretreatment methods -----------------------------------
det <- read_doc("determination_codes.csv")
im_determinations <- tibble(
  code = det$DeterminationCode,
  name = det$Description,
  note = det$NOTE
)
im_determinations <- drop_uncoded(im_determinations, "determinations")

pre <- read_doc("pretreatment_codes.csv")
im_pretreatments <- tibble(
  code = pre$PretreatmentCode,
  name = pre$Description
)
im_pretreatments <- drop_uncoded(im_pretreatments, "pretreatments")

# Sites -------------------------------------------------------------------
sit <- read_doc("IM_sites_info.csv")
im_sites <- tibble(
  area      = sit$Acode,
  country   = sit$CountryCod,
  name      = sit$Name,
  latitude  = as.numeric(sit$Latitude),
  longitude = as.numeric(sit$Longitude),
  active    = as.integer(sit$Active) == 1L
)

# Flags -------------------------------------------------------------------
# From the ICP IM Manual edition 8, section 4.3.2, plus the AM-specific table
# in section 7.2.5. The dataset README lists only the five general FLAGSTA
# codes; A, Z, XA, XZ and SZ are documented in the AM section and do occur in
# the data (36,522 rows between them), so all ten are recorded here.
im_flags <- tibble::tribble(
  ~type,      ~code, ~name,                    ~description,
  "FLAGQUA",  "L",   "below detection",        "Less than detection limit; the detection limit is given as the value",
  "FLAGQUA",  "E",   "estimated",              "Estimated from measured value",
  "FLAGQUA",  "V",   "verified, no value",     "Species verified but no value given (subprogramme BB)",
  "FLAGSTA",  "X",   "mean",                   "Arithmetic average / monthly average",
  "FLAGSTA",  "W",   "weighted mean",          "Weighted mean",
  "FLAGSTA",  "S",   "sum",                    "Sum",
  "FLAGSTA",  "SE",  "standard error",         "Standard error",
  "FLAGSTA",  "M",   "mode",                   "Mode; used for predominant wind direction",
  "FLAGSTA",  "A",   "minimum",                "Monthly minimum (subprogramme AM)",
  "FLAGSTA",  "Z",   "maximum",                "Monthly maximum (subprogramme AM)",
  "FLAGSTA",  "XA",  "mean of daily minima",   "Average monthly minimum (subprogramme AM)",
  "FLAGSTA",  "XZ",  "mean of daily maxima",   "Average monthly maximum (subprogramme AM)",
  "FLAGSTA",  "SZ",  "maximum daily sum",      "Maximum daily sum; used for precipitation (subprogramme AM)"
)

# Subprogrammes -----------------------------------------------------------
# Only what does not change between releases. Sizes come from the repository
# via im_manifest(); row counts, site counts and year ranges are measured by
# im_coverage(). Nothing here needs editing when a new version is published.
im_subprogrammes <- tibble::tribble(
  ~subprog, ~name,                        ~file,                            ~collection, ~key,
  "AC", "Air chemistry",                 "AC_air_chemistry.csv",            "CHEM", "SUBST",
  "AL", "Aerial green algae",            "AL_aerial_green_algae.csv",       "BIO1", "PARAM",
  "AM", "Meteorology",                   "AM_meteorology.csv",              "CHEM", "SUBST",
  "BB", "Birds",                         "BB_birds.csv",                    "BIO2", "PARAM",
  "BI", "Tree bioelements",              "BI_tree_bioelements.csv",         "BIO2", "PARAM",
  "EP", "Epiphytes",                     "EP_epiphytes.csv",                "BIO2", "PARAM",
  "FC", "Foliage chemistry",             "FC_foliage_chemistry.csv",        "CHEM", "SUBST",
  "FD", "Forest damage",                 "FD_forest_damage.csv",            "BIO1", "PARAM",
  "GW", "Groundwater chemistry",         "GW_groundwater_chemistry.csv",    "CHEM", "SUBST",
  "LC", "Lakewater chemistry",           "LC_lakewater_chemistry.csv",      "CHEM", "SUBST",
  "LF", "Litterfall chemistry",          "LF_litterfall_chemistry.csv",     "CHEM", "SUBST",
  "MB", "Microbial decomposition",       "MB_microbial_decomposition.csv",  "CHEM", "SUBST",
  "MC", "Metal chemistry in mosses",     "MC_metal_chemistry_mosses.csv",   "CHEM", "SUBST",
  "PC", "Precipitation chemistry",       "PC_precipitation_chemistry.csv",  "CHEM", "SUBST",
  "RW", "Runoff water chemistry",        "RW_runoff_water_chemistry.csv",   "CHEM", "SUBST",
  "SC", "Soil chemistry",                "SC_soil_chemistry.csv",           "CHEM", "SUBST",
  "SF", "Stemflow",                      "SF_stemflow.csv",                 "CHEM", "SUBST",
  "SW", "Soilwater chemistry",           "SW_soilwater_chemistry.csv",      "CHEM", "SUBST",
  "TF", "Throughfall chemistry",         "TF_throughfall_chemistry.csv",    "CHEM", "SUBST",
  "VG", "Vegetation",                    "VG_vegetation.csv",               "BIO1", "PARAM",
  "VS", "Vegetation structure",          "VS_vegetation_structure.csv",     "BIO2", "PARAM"
)

# Stamp which release these were actually built from. IM_BUNDLED_VERSION is a
# constant in R/api.R and can be edited independently; the stamp records what
# was really fetched, so a test can catch the two drifting apart. Without it,
# bumping the constant without rerunning this script would leave the older
# release's code lists in place, labelled as the newer one's, in silence.
attr(im_subprogrammes, "dataset_version") <- IM_BUNDLED_VERSION
attr(im_subprogrammes, "built") <- as.character(Sys.Date())

usethis::use_data(
  im_subprogrammes, im_sites, im_substances, im_parameters,
  im_determinations, im_pretreatments, im_flags,
  overwrite = TRUE, compress = "xz"
)

message("Wrote ", nrow(im_substances), " substances, ",
        nrow(im_parameters), " parameters, ",
        nrow(im_sites), " sites.")
