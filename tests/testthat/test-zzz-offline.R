# Runs last, by name. setup.R refuses and records every network request made
# while the suite runs offline; this is where a record turns into a failure.
# The refusal alone cannot fail the test that made the request, because the
# package catches the error and falls back - which is exactly how a test
# reached the repository unnoticed before.

test_that("no test reached the network under R CMD check", {
  skip_if(identical(Sys.getenv("NOT_CRAN"), "true"),
          "the network is allowed outside R CMD check")
  expect(
    !length(offline_requests$urls),
    paste0(
      "Tests reached for the network under R CMD check. Mock the repository ",
      "in the test that made these requests, or mark it live with ",
      "skip_if_repository_unreachable():\n",
      paste0("  ", unique(offline_requests$urls), collapse = "\n")
    )
  )
})
