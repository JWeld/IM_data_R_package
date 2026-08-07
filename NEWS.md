# icpim 0.1.0

First release.

## Reading

* `im_read()` downloads, caches, reads and tidies one subprogramme, with
  filters for site, country, year, substance and statistic.
* `im_read_file()` does the reading and typing half for a file already on disk.
* `im_download()` warms the cache; `"all"` fetches every subprogramme.
* Files are cached under `tools::R_user_dir("icpim", "cache")`, keyed by
  dataset version. Downloads are atomic, so an interrupted transfer cannot
  leave a truncated file that later looks cached.

## Handling of the published files

* **Sodium's substance code is corrected on read.** The ICP IM code for sodium
  is the literal string `"NA"`, and a known issue affecting **version 1 of the
  dataset only** leaves it blank in 48,441 rows across twelve subprogrammes.
  It is corrected in the data itself from version 2 onwards.
  `im_read(repair = "auto")`, the default, applies the correction to version 1
  and leaves later versions untouched, so scripts survive the change; `TRUE`
  forces it and `FALSE` returns the file exactly as published. A file that
  already carries the code is passed through unchanged whatever the setting,
  and blank codes appearing in a version that should not have them are
  reported rather than assumed to be sodium.
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
* `im_units()` reports determinands published in more than one unit.
* `im_detection_limit()` makes the treatment of below-detection values
  explicit.

## Data

* `im_subprogrammes`, `im_sites`, `im_substances`, `im_parameters`,
  `im_determinations`, `im_pretreatments` and `im_flags`.
* `im_flags` records all ten `FLAGSTA` codes. The deposit's `README.txt` lists
  five; `A`, `Z`, `XA`, `XZ` and `SZ` are defined in the AM section of the ICP
  IM Manual and occur in 36,522 rows.
* `im_example()` gives three small real extracts for offline use.
