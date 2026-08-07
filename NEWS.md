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

* **Sodium's substance code is restored.** The ICP IM code for sodium is the
  literal string `"NA"`, and it arrives blank in 48,441 rows across twelve
  subprogrammes of version 1. `im_read(repair = TRUE)`, the default, puts it
  back; `repair = FALSE` returns the file exactly as published.
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
* `im_widen()` pivots to one column per determinand, keeping `FLAGSTA` in the
  key and erroring rather than collapsing duplicates silently.
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
