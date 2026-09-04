# icpim 0.1.0

First release.

## Reading

* `im_read()` downloads, caches, reads and tidies one subprogramme, with
  filters for site, country, year and substance.
* `im_read_file()` does the reading and typing half for a file already on disk.
* `im_download()` warms the cache; `"all"` fetches every subprogramme.
* Files are cached under `tools::R_user_dir("icpim", "cache")`, keyed by
  dataset version. Downloads are atomic, so an interrupted transfer cannot
  leave a truncated file that later looks cached.

## Handling of the published files

* **Sodium's substance code is corrected on read.** The ICP IM code for sodium
  is the literal string `"NA"`, and a known issue affecting **version 1 of the
  dataset only** leaves it blank in 48,444 rows across thirteen subprogrammes.
  It is corrected in the data itself from version 2 onwards.
  `im_read(repair = "auto")`, the default, applies the correction to version 1
  and leaves later versions untouched, so scripts survive the change; `TRUE`
  forces it and `FALSE` returns the file exactly as published. A file that
  already carries the code is passed through unchanged whatever the setting,
  and blank codes appearing in a version that should not have them are
  reported rather than assumed to be sodium. The correction covers both code
  columns: sodium is measured in tree biomass too, where the determinand
  column is `PARAM`, and three rows of `BI` carry the blank there. The total
  restored, 48,444, matches the producing script's own count exactly.
* Tables published without a column that is empty throughout a subprogramme
  are handled: the producer prunes such columns, so the column set varies
  between subprogrammes and between releases. `im_units()` now says which
  column is missing instead of failing with a tidy-select error.
* Every column is typed explicitly rather than guessed, so `SCODE` keeps its
  leading zeros.
* `YYYYMM` is parsed to `date`, `year` and `month`. Annual values published
  with month `00` get `month = NA` rather than failing to parse.
* The bundled code lists are trimmed of the fixed-width padding the published
  lookups carry (`"NA      "`), without which a join matches almost nothing.
* Lookup files are read with vroom rather than `read.csv(fileEncoding=)`,
  which truncates them to 41 rows under a C locale.

## Decoding and tidying

* `im_decode()` adds `substance`/`parameter`, `determination`, `pretreatment`,
  `stat` and `quality`. `im_codes()` searches the code lists.
* Decoding honours `LISTSUB`/`PARLIST`, which say whether a code belongs to the
  substance list (`"DB"`) or the IM parameter list (`"IM"`). Around 26,000 rows
  across seven subprogrammes use `IM` codes - `AOT40`, `SOL_G`, `CEC_E`,
  `C/N`, `BDEN` - that are absent from the substance list entirely. It also
  disambiguates the 77 codes present in both: `ABS` is *Absorbance* in one and
  *Number of branches on the current tree where algae are missing* in the
  other, and both occur in `AL`.
* `im_widen()` pivots to one column per determinand, keeping `FLAGSTA` in the
  key and erroring rather than collapsing duplicates silently. The error names
  the columns that vary within the colliding keys - usually `DETER`, the same
  sample analysed by two methods - and reports what fraction of rows is
  affected.
* Filters report rather than empty the table in silence. `sites`, `countries`,
  `years` and `substances` each warn when they match nothing, naming
  what was asked for and what the subprogramme actually holds - a typo used to
  be indistinguishable from the data genuinely having none. The warning is a
  diagnostic, so `quiet = TRUE` does not silence it; use `suppressWarnings()`.
* `countries` accepts the full country name as it appears in `COUNTRY`
  (`"Sweden"`) as well as the ISO code that prefixes `AREA` (`"SE"`). Only the
  code worked before, so filtering on the value visible in the data returned
  nothing.
* `im_read()` has no `stat` argument. Choosing which summary you want - the
  monthly mean rather than the maximum - is an analysis step, not part of
  fetching, and an argument named `stat` sitting beside a decoded column of the
  same name was more confusing than useful. The column is what you select on:
  `subset(x, stat == "mean")`, or `FLAGSTA == "X"` for the published code.
  The note that a subprogramme mixes statistics now fires whenever it applies,
  rather than only when the argument was left unset.
* `im_units()` reports determinands published in more than one unit.
* `im_detection_limit()` makes the treatment of below-detection values
  explicit.

## Following the annual dataset updates

The deposit is republished each year. Nothing version-specific is recorded in
this package, so an ordinary release needs no code change:

* **The default version is `"latest"`.** A session reads the newest published
  release, looked up in the repository the first time a version is needed and
  fixed for the rest of the session, so one session never mixes releases and
  a running analysis is not moved by a release appearing mid-way. Version 3,
  4 and so on will be read by default when they are published. Pin a release
  with `options(icpim.version = "2")` for an analysis that must keep
  returning the same numbers; `im_provenance()` records which release an
  object came from.
* If the repository cannot be reached when `"latest"` is resolved, the
  session reads the newest release already in the cache, or failing that the
  release the package was built against, and says which and why rather than
  reading an older release in silence. `im_check_version()` tells such a
  session when a newer release has become reachable.
* The bundled code lists were rebuilt from version 2, which is also the
  release cited offline. Version 2 corrected the sodium code, so
  `repair = "auto"` does nothing to it; the version 1 extracts in
  `inst/extdata` are kept as they were published, since they are what the
  repair is tested against.
* `im_manifest()` reads the file list and sizes from the repository, so a
  renamed file or an added subprogramme is picked up automatically.
* `im_coverage()` measures row counts, site counts, year ranges and
  determinand counts from the data rather than reciting stored figures.
* `im_subprogrammes` keeps only what is stable between releases.
* Code lists published with a version this package was not built against are
  fetched and cached on first use, so a release that adds substances decodes
  without an update. `im_update_codes()` refreshes them by hand.
* If those lists cannot be fetched, decoding falls back to the bundled ones
  and says so, once per session. That fallback is the one failure here that
  would otherwise be silent and wrong rather than merely absent, since names
  would resolve against the wrong vocabulary. It also catches the maintainer
  error of moving the default version without fetching the new lists.
* The bundled lookups carry a stamp of the release they were built from, and a
  test asserts it matches `IM_BUNDLED_VERSION`. That catches the other half of
  the same mistake: editing the constant without rerunning
  `data-raw/make_data.R`, which would leave the older lists in place labelled
  as the newer ones.
* `im_doi()`, `im_dataset_info()` and `im_cite()` follow the version being
  read, since each release has its own DOI. Where a version's DOI cannot be
  established they report `NA` and say so, rather than returning the DOI of a
  different release: citing the wrong version is worse than admitting the
  value is unknown.
* `im_cite()` leads with the data paper, which is the citation to give, and
  follows it with the deposit DOI for the release analysed. The paper was
  previously presented as an optional extra after the deposit. If a version's
  DOI cannot be resolved the paper is still printed, since it does not depend
  on which release was read.
* The deposit is attributed to the ICP Integrated Monitoring Programme Centre,
  which produces the data, rather than to the university that hosts the
  repository record. The repository's own `principal` field names the host, so
  the attribution is fixed rather than read from the API.
* `im_provenance()` reports the dataset version, DOI, file, download time and
  package version behind an object from `im_read()`. The record is attached to
  the data because the package version cannot stand in for it: `icpim.version`
  is a runtime option, so one package version reads any release, and two
  people running the same package version can be on different data. It
  survives subsetting, the common dplyr verbs, `im_widen()` and `saveRDS()`.
* `im_check_version()` reports whether a newer release exists than the one
  being read. `im_latest_version()` and `im_version_exists()` sit underneath
  it. A pinned version never moves on its own, because moving it changes the
  numbers an analysis returns.
* A monthly `dataset-watch` GitHub Action checks for a new release, runs
  `data-raw/verify_release.R` against it, and opens an issue if either the
  release is new or verification fails.

## Hardening

Five failures reported against the first working version. None was reachable
with the published files as they stand - every one of the 21 carries `AREA`,
`YYYYMM`, `VALUE` and `UNIT` - so these guard against a release whose column
set differs, or a session whose state does.

* A download that fails because the repository is down, with the network
  otherwise up, says so. It used to be reported as the version not being
  published, because the existence check cannot tell an absent release from
  an unreachable repository, and that sent the reader re-pinning a release
  that exists.
* `im_detection_limit()` errors naming the missing column when a table has no
  `VALUE`, matching `im_units()`. It used to return the table unchanged behind
  a bare tibble warning, which reads as "nothing was below detection".
* Filters name the column they need when it is absent. Without `AREA` or
  `year` the subscript was length zero and the failure surfaced as "Logical
  subscript `keep` must be size 1 or N", which says nothing about the cause.
* The default dataset version is written once, as `IM_DEFAULT_VERSION`. It was
  in three places, and `im_version()`'s fallback was unreachable because
  `.onLoad` always sets the option - so a maintainer could edit it and see no
  effect. The `dataset-watch` issue template named that unreachable edit; it
  now names the constants that work, and says why step 3 without step 2 fails.
* The warn-once guards are initialised by `.onLoad` as well as at build, and
  read with `isTRUE()`. An absent flag is not `FALSE`: `!NULL` has length zero,
  which errors inside `&&` rather than warning.
* The duplicate-key report honours `values_from`, so a non-default pivot no
  longer names its own value column as a culprit.

## Data

* `im_subprogrammes`, `im_sites`, `im_substances`, `im_parameters`,
  `im_determinations`, `im_pretreatments` and `im_flags`.
* `im_flags` records all ten `FLAGSTA` codes. The deposit's `README.txt` lists
  five; `A`, `Z`, `XA`, `XZ` and `SZ` are defined in the AM section of the ICP
  IM Manual and occur in 36,522 rows.
* `im_example()` gives four small real extracts for offline use.
