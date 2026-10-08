## Submission

icpim 0.1.0. This is a new submission.

The package provides access to an openly licensed (CC BY 4.0) research
dataset published at <doi:10.5878/x6fn-gw26>, described in Scientific Data
<doi:10.1038/s41597-026-07181-8>.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'James Weldon <james.weldon@slu.se>'

  New submission

  Possibly misspelled words in DESCRIPTION:
    ICP (2:19, 11:17)
    IM (11:21)
    al (12:15)
    et (12:12)
    pretreatment (17:34)

The new-submission note is expected. The spelling part of it is raised by
win-builder only; the local `--as-cran` check reports the new submission and
nothing else. The flagged words are all spelled as intended:

* ICP and IM are the abbreviated name of the monitoring programme whose data
  this package reads, the International Cooperative Programme on Integrated
  Monitoring of Air Pollution Effects on Ecosystems. The name is written out
  in full in the Description immediately before the abbreviation is used.
* et and al are the standard citation form, used for the reference to the
  data paper in the form CRAN asks for, Authors (year) <doi:...>.
* pretreatment is the dataset's own vocabulary. The published lookup file is
  pretreatment_codes.csv and the corresponding column in the data is PRETRE,
  so the package names it the same way the data does.

## Test environments

Checked at commit e652454, 2026-09-18:

* macOS 27 (local), R 4.6.1, `R CMD check --as-cran`: 0 errors, 0 warnings,
  the new-submission note only.
* GitHub Actions, all Status: OK:
  * ubuntu-latest, R devel (2026-09-17 r90559)
  * ubuntu-latest, R 4.6.1 (release)
  * ubuntu-latest, R 4.5.3 (oldrel-1)
  * macOS-latest, R 4.6.1 (release)
  * windows-latest, R 4.6.1 ucrt (release)

The code has changed since: the repository moved its download addresses
between 18 September and 8 October 2026, and every download failed until the
package followed it (see NEWS.md, "The repository's autumn 2026 change").
These results are to be repeated before submission.

Checked at an earlier commit, and to be repeated before submission:

* win-builder: R-devel and R-release. Both returned Status: OK apart from the
  note above. The code has changed since (see NEWS.md, "Found by reading the
  real files"), so these results do not yet cover what will be submitted.

The GitHub Actions checks also run the \donttest examples, but those are
wrapped in `try()`, so a failed download does not fail the check: after the
repository moved its files every download failed and the checks still passed.
The download paths are exercised instead by the live tests, which a separate
workflow runs against the repository on every push and weekly, and which fail
if the repository cannot be reached. Separately, `data-raw/verify_release.R`
was run at e652454 against the whole published deposit (21 files, 1,193,331
rows) and reported no problems.

## Network use and files written

The package downloads data files from the publisher's repository. Everything
that touches the network is handled as follows.

* No network access at load time.
* Downloads fail gracefully with an informative message: a missing
  connection, an unreachable repository, a repository that refuses the
  request, an unpublished dataset version, and a file that has moved within
  an existing version are distinguished and reported separately.
* Examples that need the repository are wrapped in `\donttest{}`, guarded by
  `if (curl::has_internet())`, and additionally wrapped in `try()`, since the
  repository can be unreachable even when the network is up. They fetch the
  smallest subprogramme (about 14 kB) and set
  `options(icpim.cache_dir = tempfile())` first, restoring the option
  afterwards, so a check run writes nothing outside the session temporary
  directory. Examples that need no network run unguarded against small
  extracts shipped in `inst/extdata`.
* No request can hang a check: connections time out after 10 seconds,
  metadata requests after 30, and a file download that stalls for 60 seconds
  is abandoned, in place of libcurl's five-minute default. A failed metadata
  lookup is not repeated for 30 seconds.
* The package is single-threaded and uses only https. Files are fetched
  from the address the repository lists for each, and only an https address
  is accepted from that list.
* Tests that reach the repository are skipped with `skip_on_cran()` and
  `skip_if_offline()`. The remaining tests run offline against those same
  bundled extracts. Under `R CMD check` the test suite refuses every network
  request and fails if any test makes one, so a test cannot reach the
  network unnoticed.
* Vignette chunks that would download are `eval = FALSE`.

In normal interactive use the downloaded files are cached under
`tools::R_user_dir("icpim", "cache")`, as permitted for R >= 4.0. The cache
is user-manageable: `im_cache_dir()` reports the location,
`im_cache_list()` shows its contents and `im_cache_clear()` empties it. A
user who prefers otherwise can set `options(icpim.cache_dir = ...)` or the
`ICPIM_CACHE_DIR` environment variable, including to a temporary directory.

## Licensing of included data

The package is MIT licensed. The lookup tables in `data/` and the small data
extracts in `inst/extdata` are derived from the ICP Integrated Monitoring open
dataset <doi:10.5878/x6fn-gw26>, which is CC BY 4.0. This is stated in the
`Copyright` field of DESCRIPTION and in the documentation of each dataset, and
the programme that collects the data is credited with a `dtc` role in
`Authors@R`.

## Reverse dependencies

None; this is a new package.
