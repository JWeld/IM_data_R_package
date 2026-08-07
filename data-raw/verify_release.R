# Integration check against the real deposit.
#
#   Rscript data-raw/verify_release.R
#
# Reads every subprogramme through the package and checks it against what the
# package claims, rather than against itself. Run this after each annual
# dataset update, before bumping `icpim.version`: it is what catches a new
# substance code, a changed column, or a subprogramme whose row count has
# moved.
#
# Downloads ~95 MB on a cold cache. Set ICPIM_CACHE_DIR to reuse one.

devtools::load_all(quiet = TRUE)
options(icpim.quiet = TRUE)

meta <- im_subprogrammes
res <- list()
problems <- character()
note <- function(...) problems <<- c(problems, paste0(...))

for (i in seq_len(nrow(meta))) {
  sp <- meta$subprog[i]
  x <- tryCatch(im_read(sp), error = function(e) e)
  if (inherits(x, "error")) {
    note(sp, ": READ FAILED - ", conditionMessage(x))
    next
  }

  key <- if ("SUBST" %in% names(x)) "SUBST" else "PARAM"
  dec <- if (key == "SUBST") "substance" else "parameter"

  # The manifest is hand-recorded, so it is the thing most likely to drift.
  if (nrow(x) != meta$n_rows[i]) {
    note(sp, ": manifest says ", meta$n_rows[i], " rows, read ", nrow(x))
  }
  ns <- length(unique(x$AREA))
  if (ns != meta$n_sites[i]) {
    note(sp, ": manifest says ", meta$n_sites[i], " sites, read ", ns)
  }
  yr <- range(x$year, na.rm = TRUE)
  if (yr[1] != meta$first_year[i] || yr[2] != meta$last_year[i]) {
    note(sp, ": manifest years ", meta$first_year[i], "-", meta$last_year[i],
         ", read ", yr[1], "-", yr[2])
  }
  if (key != meta$key[i]) {
    note(sp, ": manifest key ", meta$key[i], " but file uses ", key)
  }

  # Typing
  if (!is.numeric(x$VALUE)) note(sp, ": VALUE is ", class(x$VALUE)[1])
  if ("SCODE" %in% names(x)) {
    if (!is.character(x$SCODE)) note(sp, ": SCODE is ", class(x$SCODE)[1])
    bad <- unique(x$SCODE[nchar(x$SCODE) != 4])
    if (length(bad)) note(sp, ": SCODE not 4 chars: ", paste(head(bad, 5), collapse = ","))
  }

  # Every code resolves to a name
  undec <- unique(x[[key]][!is.na(x[[key]]) & is.na(x[[dec]])])
  if (length(undec)) {
    note(sp, ": undecoded ", key, ": ", paste(head(undec, 10), collapse = ","))
  }

  # Every flag is one we know about
  for (fl in intersect(c("FLAGSTA", "FLAGQUA"), names(x))) {
    unk <- setdiff(unique(x[[fl]][!is.na(x[[fl]])]), im_flags$code[im_flags$type == fl])
    if (length(unk)) note(sp, ": unknown ", fl, ": ", paste(unk, collapse = ","))
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
    subprog = sp, rows = nrow(x), sites = ns,
    years = paste0(yr[1], "-", yr[2]),
    sodium = if (key == "SUBST") sum(x$SUBST == "NA", na.rm = TRUE) else NA_integer_,
    widen = wid
  )
}

out <- do.call(rbind, res)
print(out, row.names = FALSE)
cat("\ntotal rows:", format(sum(out$rows), big.mark = ","),
    "| sodium restored:", format(sum(out$sodium, na.rm = TRUE), big.mark = ","), "\n\n")

if (length(problems)) {
  cat("PROBLEMS (", length(problems), "):\n", sep = "")
  cat(paste0("  - ", problems, collapse = "\n"), "\n")
  quit(status = 1)
}
cat("No problems found.\n")
