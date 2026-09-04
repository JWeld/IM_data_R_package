# Integration check against the live repository.
#
#   Rscript data-raw/verify_release.R          # check the newest release
#   ICPIM_VERSION=2 Rscript data-raw/verify_release.R
#
# Nothing here is compared against numbers recorded in the package, because
# the package no longer records any. It checks two things instead:
#
#   1. Internal consistency  - every file reads, types are right, every code
#      and flag resolves, dates parse. These must hold in any release.
#   2. Drift worth a human   - a new or renamed subprogramme, a code the
#      published lists do not explain, a flag not in the manual.
#
# Exits non-zero on any problem, so it can run unattended in CI.
#
# Downloads ~95 MB on a cold cache. Set ICPIM_CACHE_DIR to reuse one.

devtools::load_all(quiet = TRUE)
options(icpim.quiet = TRUE)

if (nzchar(Sys.getenv("ICPIM_VERSION"))) {
  options(icpim.version = Sys.getenv("ICPIM_VERSION"))
}
version <- im_version()

# SCODE is a zero-padded four-digit station code (ICP IM Manual, section
# 4.3.1). A code of any other width means the leading zeros were lost, or the
# field changed shape in this release.
SCODE_WIDTH <- 4L

problems <- character()
note <- function(...) problems <<- c(problems, paste0(...))

cat("Checking ICP IM dataset version ", version, "\n\n", sep = "")

# --- Is this still the newest release? ------------------------------------
chk <- im_check_version(quiet = TRUE)
if (is.na(chk$latest)) {
  note("could not reach the repository to check for a newer version")
} else if (chk$newer_available) {
  cat("NOTE: version ", chk$latest, " has been published; this run checks ",
      version, ".\n\n", sep = "")
}

# --- Does the file list match what the package expects? -------------------
man <- im_manifest(version, "data")
bundled <- im_subprogrammes

added   <- setdiff(man$subprog, bundled$subprog)
removed <- setdiff(bundled$subprog, man$subprog)
if (length(added)) {
  note("subprogramme(s) new in this release: ", paste(added, collapse = ", "),
       " - add them to data-raw/make_data.R")
}
if (length(removed)) {
  note("subprogramme(s) gone from this release: ", paste(removed, collapse = ", "))
}
renamed <- man$file[match(bundled$subprog, man$subprog)] != bundled$file
renamed[is.na(renamed)] <- FALSE
if (any(renamed)) {
  note("file renamed: ",
       paste(bundled$file[renamed], "->",
             man$file[match(bundled$subprog[renamed], man$subprog)], collapse = "; "))
}

# --- Read everything ------------------------------------------------------
res <- list()
for (sp in man$subprog) {
  x <- tryCatch(im_read(sp, version = version), error = function(e) e)
  if (inherits(x, "error")) {
    note(sp, ": READ FAILED - ", conditionMessage(x))
    next
  }

  key <- if ("SUBST" %in% names(x)) "SUBST" else "PARAM"
  dec <- if (key == "SUBST") "substance" else "parameter"

  if (!is.numeric(x$VALUE)) note(sp, ": VALUE is ", class(x$VALUE)[1])
  if ("SCODE" %in% names(x)) {
    if (!is.character(x$SCODE)) note(sp, ": SCODE is ", class(x$SCODE)[1])
    bad <- unique(x$SCODE[nchar(x$SCODE) != SCODE_WIDTH])
    if (length(bad)) note(sp, ": SCODE not ", SCODE_WIDTH, " chars: ", paste(head(bad, 5), collapse = ","))
  }

  undec <- unique(x[[key]][!is.na(x[[key]]) & is.na(x[[dec]])])
  if (length(undec)) {
    note(sp, ": ", length(undec), " code(s) absent from the published lists: ",
         paste(head(undec, 10), collapse = ","))
  }

  for (fl in intersect(c("FLAGSTA", "FLAGQUA"), names(x))) {
    unk <- setdiff(unique(x[[fl]][!is.na(x[[fl]])]), im_flags$code[im_flags$type == fl])
    if (length(unk)) note(sp, ": ", fl, " value(s) not in the manual: ",
                          paste(unk, collapse = ","))
  }

  if (any(is.na(x$year))) note(sp, ": ", sum(is.na(x$year)), " rows with no year")

  chr <- names(x)[vapply(x, is.character, logical(1))]
  emp <- vapply(chr, function(cn) any(!is.na(x[[cn]]) & !nzchar(x[[cn]])), logical(1))
  if (any(emp)) note(sp, ": empty strings remain in ", paste(chr[emp], collapse = ","))

  wid <- tryCatch({
    w <- im_widen(x); paste0("ok(", ncol(w), ")")
  }, error = function(e) {
    if (grepl("duplicate key", conditionMessage(e))) "dup-key" else {
      note(sp, ": im_widen failed - ", conditionMessage(e)); "FAILED"
    }
  })

  res[[sp]] <- data.frame(
    subprog = sp,
    rows    = nrow(x),
    sites   = length(unique(x$AREA)),
    years   = paste0(min(x$year, na.rm = TRUE), "-", max(x$year, na.rm = TRUE)),
    blank_subst = if (key == "SUBST") {
      sum(is.na(x$SUBST)) + sum(x$SUBST == "NA", na.rm = TRUE)
    } else NA_integer_,
    widen = wid
  )
}

out <- do.call(rbind, res)
print(out, row.names = FALSE)
cat("\ntotal rows:", format(sum(out$rows), big.mark = ","), "\n\n")

if (length(problems)) {
  cat("PROBLEMS (", length(problems), "):\n", sep = "")
  cat(paste0("  - ", problems, collapse = "\n"), "\n")
  quit(status = 1)
}
cat("No problems found.\n")
