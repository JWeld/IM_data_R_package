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
# treats "NA" as missing. read.csv(na.strings = "") is used deliberately.

library(tibble)

doc_url <- function(f) {
  sprintf(
    "https://doris.snd.se/api/file/2024-180/1/documentation?filePath=%s",
    utils::URLencode(f, reserved = TRUE)
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

  x <- vroom::vroom(
    tmp,
    delim = ",",
    col_types = vroom::cols(.default = vroom::col_character()),
    na = character(),         # NOT "NA": that is sodium
    show_col_types = FALSE,
    progress = FALSE
  )
  x <- as.data.frame(x, stringsAsFactors = FALSE)

  # Trim the fixed-width padding from every character column.
  x[] <- lapply(x, function(col) {
    col <- trimws(col)
    col[col == "NULL" | !nzchar(col)] <- NA_character_
    col
  })
  x
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
im_substances <- im_substances[!is.na(im_substances$code), ]
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
im_parameters <- im_parameters[!is.na(im_parameters$code), ]

# Determination and pretreatment methods -----------------------------------
det <- read_doc("determination_codes.csv")
im_determinations <- tibble(
  code = det$DeterminationCode,
  name = det$Description,
  note = det$NOTE
)
im_determinations <- im_determinations[!is.na(im_determinations$code), ]

pre <- read_doc("pretreatment_codes.csv")
im_pretreatments <- tibble(
  code = pre$PretreatmentCode,
  name = pre$Description
)
im_pretreatments <- im_pretreatments[!is.na(im_pretreatments$code), ]

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
# Row counts, site counts and year ranges are measured from version 1 of the
# deposit, not asserted: see data-raw/profile_subprogrammes.R.
im_subprogrammes <- tibble::tribble(
  ~subprog, ~name,                        ~file,                            ~collection, ~key,    ~size_mb, ~n_rows, ~n_sites, ~first_year, ~last_year,
  "AC", "Air chemistry",                 "AC_air_chemistry.csv",            "CHEM", "SUBST",  2.0,   27002L, 38L, 1987L, 2019L,
  "AL", "Aerial green algae",            "AL_aerial_green_algae.csv",       "BIO1", "PARAM",  0.6,    6461L, 10L, 1999L, 2019L,
  "AM", "Meteorology",                   "AM_meteorology.csv",              "CHEM", "SUBST",  6.9,  105278L, 36L, 1967L, 2019L,
  "BB", "Birds",                         "BB_birds.csv",                    "BIO2", "PARAM",  0.01,     71L,  2L, 2000L, 2010L,
  "BI", "Tree bioelements",              "BI_tree_bioelements.csv",         "BIO2", "PARAM",  1.4,   13203L,  6L, 1991L, 2019L,
  "EP", "Epiphytes",                     "EP_epiphytes.csv",                "BIO2", "PARAM",  0.1,    1166L, 11L, 1993L, 2019L,
  "FC", "Foliage chemistry",             "FC_foliage_chemistry.csv",        "CHEM", "SUBST",  2.4,   23963L, 31L, 1986L, 2019L,
  "FD", "Forest damage",                 "FD_forest_damage.csv",            "BIO1", "PARAM", 11.1,  130380L, 31L, 1998L, 2019L,
  "GW", "Groundwater chemistry",         "GW_groundwater_chemistry.csv",    "CHEM", "SUBST",  4.6,   64513L, 13L, 1989L, 2020L,
  "LC", "Lakewater chemistry",           "LC_lakewater_chemistry.csv",      "CHEM", "SUBST",  4.4,   68724L,  8L, 1991L, 2019L,
  "LF", "Litterfall chemistry",          "LF_litterfall_chemistry.csv",     "CHEM", "SUBST",  2.6,   27995L, 21L, 1987L, 2019L,
  "MB", "Microbial decomposition",       "MB_microbial_decomposition.csv",  "CHEM", "SUBST",  0.07,    779L,  9L, 1993L, 2019L,
  "MC", "Metal chemistry in mosses",     "MC_metal_chemistry_mosses.csv",   "CHEM", "SUBST",  0.01,    125L,  4L, 2000L, 2016L,
  "PC", "Precipitation chemistry",       "PC_precipitation_chemistry.csv",  "CHEM", "SUBST", 11.5,  162851L, 39L, 1977L, 2019L,
  "RW", "Runoff water chemistry",        "RW_runoff_water_chemistry.csv",   "CHEM", "SUBST",  9.5,  150050L, 28L, 1987L, 2019L,
  "SC", "Soil chemistry",                "SC_soil_chemistry.csv",           "CHEM", "SUBST",  0.6,    8617L, 18L, 1993L, 2018L,
  "SF", "Stemflow",                      "SF_stemflow.csv",                 "CHEM", "SUBST",  3.1,   33720L, 19L, 1992L, 2019L,
  "SW", "Soilwater chemistry",           "SW_soilwater_chemistry.csv",      "CHEM", "SUBST", 12.4,  178557L, 31L, 1986L, 2019L,
  "TF", "Throughfall chemistry",         "TF_throughfall_chemistry.csv",    "CHEM", "SUBST", 11.8,  125837L, 33L, 1990L, 2019L,
  "VG", "Vegetation",                    "VG_vegetation.csv",               "BIO1", "PARAM",  3.6,   41100L, 27L, 1990L, 2019L,
  "VS", "Vegetation structure",          "VS_vegetation_structure.csv",     "BIO2", "PARAM",  2.1,   22939L, 20L, 1991L, 2019L
)

usethis::use_data(
  im_subprogrammes, im_sites, im_substances, im_parameters,
  im_determinations, im_pretreatments, im_flags,
  overwrite = TRUE, compress = "xz"
)

message("Wrote ", nrow(im_substances), " substances, ",
        nrow(im_parameters), " parameters, ",
        nrow(im_sites), " sites.")
