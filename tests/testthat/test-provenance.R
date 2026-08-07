test_that("im_read_file records what a bare path can support", {
  pc <- im_read_file(im_example("sample_PC.csv"))
  p <- im_provenance(pc)
  expect_s3_class(p, "icpim_provenance")
  expect_equal(p$file, "sample_PC.csv")
  expect_equal(p$rows_read, nrow(pc))
  expect_equal(p$package_version, as.character(utils::packageVersion("icpim")))
  # A path says nothing about which release it came from; that is not guessed.
  expect_true(is.na(p$dataset_version))
  expect_true(is.na(p$doi))
})

test_that("the record survives the operations people actually use", {
  pc <- im_read_file(im_example("sample_PC.csv"))

  expect_s3_class(im_provenance(pc[pc$SUBST == "CL", ]), "icpim_provenance")
  expect_s3_class(im_provenance(dplyr::filter(pc, .data$SUBST == "CL")),
                  "icpim_provenance")
  expect_s3_class(im_provenance(dplyr::mutate(pc, z = 1)), "icpim_provenance")
  expect_s3_class(im_provenance(dplyr::arrange(pc, .data$date)),
                  "icpim_provenance")
  expect_s3_class(im_provenance(head(pc, 10)), "icpim_provenance")

  f <- withr::local_tempfile(fileext = ".rds")
  saveRDS(pc, f)
  expect_s3_class(im_provenance(readRDS(f)), "icpim_provenance")
})

test_that("im_widen and im_detection_limit carry it forward", {
  pc <- im_read_file(im_example("sample_PC.csv")) |> im_decode()
  # pivot_wider builds a new table and would otherwise drop it.
  expect_s3_class(im_provenance(im_widen(pc)), "icpim_provenance")
  expect_s3_class(im_provenance(im_detection_limit(pc, "half")),
                  "icpim_provenance")
  expect_s3_class(im_provenance(im_decode(pc)), "icpim_provenance")
})

test_that("an object with no record warns rather than failing silently", {
  plain <- tibble::tibble(a = 1)
  expect_warning(p <- im_provenance(plain), "No provenance record")
  expect_null(p)
})

test_that("printing shows the version and DOI, and reads as text", {
  pc <- im_read_file(im_example("sample_PC.csv"))
  txt <- paste(capture.output(print(im_provenance(pc))), collapse = " ")
  expect_match(txt, "ICP IM data provenance")
  expect_match(txt, "sample_PC.csv")
  # Unknown fields say so rather than showing NA.
  expect_match(txt, "unknown")
  expect_type(format(im_provenance(pc)), "character")
})

test_that("the sodium count never exceeds the rows returned", {
  pc <- im_read_file(im_example("sample_PC.csv"))
  p <- im_provenance(pc)
  expect_lte(p$sodium_corrected, p$rows_read)
  expect_equal(p$sodium_corrected, 60L)
})

test_that("a full read records the release it came from", {
  skip_on_cran()
  skip_if_offline()
  withr::local_options(
    icpim.cache_dir = withr::local_tempdir(),
    icpim.version = "1",
    icpim.quiet = TRUE
  )
  mc <- im_read("MC")
  p <- im_provenance(mc)
  expect_equal(p$subprog, "MC")
  expect_equal(p$dataset_version, "1")
  expect_equal(p$doi, "10.5878/z376-2m63")
  expect_equal(p$rows_read, nrow(mc))
  expect_s3_class(p$downloaded, "POSIXct")

  # Filtering must not leave the record claiming more than it returned.
  few <- im_read("MC", substances = "CD")
  pf <- im_provenance(few)
  expect_equal(pf$rows_read, nrow(few))
  expect_lte(pf$sodium_corrected, pf$rows_read)
})
