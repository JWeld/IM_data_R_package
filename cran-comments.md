## Submission

icpim 0.1.0. This is a new submission.

The package provides access to an openly licensed (CC BY 4.0) research
dataset published at <doi:10.5878/z376-2m63>, described in Scientific Data
<doi:10.1038/s41597-026-07181-8>.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'James Weldon <weldondesign@gmail.com>'
  New submission

This is expected for a first submission.

## Test environments

* macOS 15 (local), R 4.5.3
* GitHub Actions: ubuntu-latest (R devel, release, oldrel-1),
  macOS-latest (release), windows-latest (release)

## Network use and files written

The package downloads data files from the publisher's repository. Everything
that touches the network is handled as follows.

* No network access at load time.
* Downloads fail gracefully with an informative message: a missing
  connection, an unpublished dataset version, and a file that has moved
  within an existing version are distinguished and reported separately.
* Examples that need the repository are wrapped in `\donttest{}` and further
  guarded by `if (curl::has_internet())`. They fetch the smallest
  subprogramme (about 14 kB) and set `options(icpim.cache_dir = tempfile())`
  first, so a check run writes nothing outside the session temporary
  directory. Examples that need no network run unguarded against small
  extracts shipped in `inst/extdata`.
* Tests that reach the repository are skipped with `skip_on_cran()` and
  `skip_if_offline()`. The remaining tests run offline against those same
  bundled extracts.
* Vignette chunks that would download are `eval = FALSE`.

In normal interactive use the downloaded files are cached under
`tools::R_user_dir("icpim", "cache")`, as permitted for R >= 4.0. The cache
is user-manageable: `im_cache_dir()` reports the location,
`im_cache_list()` shows its contents and `im_cache_clear()` empties it. A
user who prefers otherwise can set `options(icpim.cache_dir = ...)` or the
`ICPIM_CACHE_DIR` environment variable, including to a temporary directory.

## Reverse dependencies

None; this is a new package.
