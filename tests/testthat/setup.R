# Redirect both compilation caches away from the user's real cache dir.
# Set NEWSTAN_TEST_USE_USER_CACHE=true (e.g. in ~/.Renviron) to opt back
# into the warm persistent cache for fast local iteration.
if (!identical(Sys.getenv("NEWSTAN_TEST_USE_USER_CACHE"), "true")) {
  test_cache <- file.path(tempdir(), "newstan-test-cache")
  options(
    newstan_cache_dir = file.path(test_cache, "models"),
    newstan_pch_dir = file.path(test_cache, "pch")
  )
}
