# icpim

<!-- badges: start -->
[![R-CMD-check](https://github.com/JWeld/IM_data_R_package/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JWeld/IM_data_R_package/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## **IMPORTANT! THIS PACKAGE IS EXPERIMENTAL AND UNDER DEVELOPMENT - ITS OUTPUTS ARE NOT VERIFIED AND IT SHOULD NOT BE USED YET FOR REAL WORK! It has been developed by Claude Code (Opus 5 and Fable)**

Access the open dataset of the **International Cooperative Programme on
Integrated Monitoring of Air Pollution Effects on Ecosystems (ICP IM)** from R.

The dataset ([doi:10.5878/z376-2m63](https://doi.org/10.5878/z376-2m63))
publishes long-term integrated ecosystem monitoring from European forested
catchments: 21 subprogrammes, 55 sites in 14 countries, roughly 1.2 million
observations from 1967 to 2020, covering deposition, soil, soil water,
groundwater, runoff, vegetation and biota. It is updated annually and released
under CC BY 4.0.

`icpim` downloads and caches the published files, reads them with the correct
column types, decodes the code lists into readable names, and handles several
encoding traps that are easy to walk into and hard to spot afterwards.

## Installation

```r
# install.packages("pak")
pak::pak("JWeld/IM_data_R_package")
```

## Usage

```r
library(icpim)

# What is available
im_subprogrammes

# Read a subprogramme; the first call downloads and caches it
pc <- im_read("PC", countries = "SE", years = 2000:2019)

# One column per determinand
im_widen(pc)

# Monthly mean air temperature, not the extremes
temp <- im_read("AM", substances = "TEMP", stat = "mean")
```

## Why not just `read.csv()`

Three things in the published files will bite.

**Sodium's substance code is the string `NA`.** Every default CSV reader in R
treats that as a missing value, which silently deletes every sodium record and
breaks any `SUBST ==` comparison:

```r
read.csv(text = "SUBST,VALUE\nNA,2.4\nCL,4.4")$SUBST
#> [1] NA   "CL"
```

The same thing happened once before publication, and is a known issue
affecting **version 1 of the dataset only**: sodium's code arrives blank in
48,441 rows across twelve subprogrammes. It is corrected from version 2
onwards, and `icpim` corrects it on read. The default `repair = "auto"`
applies the correction to version 1 and leaves later versions untouched, so
scripts keep working across the change.

**Station codes are zero-padded.** `SCODE` is `"0176"`, not `176`. Read as a
number it stops joining. And `SCODE` distinguishes *collectors*, not
replicates - a site can have several gauges covering different periods, so
summing a flux over all of them double-counts the overlap.

**`FLAGSTA` is part of the key, not an annotation.** In meteorology the same
site, level, month and parameter carries up to five rows - mean, minimum,
maximum, and the averaged daily extremes - distinguished only by that flag.
47% of temperature keys have more than one. Averaging without filtering biases
mean air temperature by +0.42 °C, and for precipitation it mixes monthly sums
with maximum *daily* sums.

`vignette("icpim")` covers each of these, and the evidence behind the sodium
repair.

## Reproducibility

The deposit is updated annually and each release has its own DOI. `icpim` pins
version 1 by default so scripts keep returning the same numbers; move
deliberately with `options(icpim.version = "2")`. The cache is keyed by
version, so releases are never mixed.

Files are cached under `tools::R_user_dir("icpim", "cache")`. Set
`options(icpim.cache_dir = "data-raw/icpim")` to keep an analysis
self-contained, or run `im_download("all")` (about 95 MB) before going offline.

`im_check_version()` tells you whether a newer release has appeared. The
package never moves on its own, since that would change the numbers an
analysis returns. Nothing version-specific is hard-coded: the file list and
sizes come from `im_manifest()`, row counts and year ranges from
`im_coverage()`, and code lists for an unfamiliar release are fetched on
first use. A renamed file, an added subprogramme or a new substance code
therefore needs no update to this package.

## Citation

The data are CC BY 4.0 and require attribution. `im_cite()` prints the
citation for the dataset, which is not the same as `citation("icpim")` for
this package.

> Weldon, J. et al. (2026). The International Cooperative Programme on
> Integrated Monitoring of Air Pollution Effects on Ecosystems (ICP IM),
> version 1. Swedish University of Agricultural Sciences.
> <https://doi.org/10.5878/z376-2m63>

The accompanying data paper is in *Scientific Data* 13 (2026),
[doi:10.1038/s41597-026-07181-8](https://doi.org/10.1038/s41597-026-07181-8).

Some data are withheld at the data owners' request and are available from the
ICP IM Programme Centre (`im-database@slu.se`), who can also advise on best
practice.
