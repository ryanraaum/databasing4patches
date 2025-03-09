test_that("hashtrack works", {
  savedtrack1hash <- expect_no_error(hashtrack(kaaterskill$tracks[[1]]))
  savedtrack2hash <- expect_no_error(hashtrack(huckleberry$tracks[[1]]))
  savedtrack3hash <- expect_no_error(hashtrack(sherrillnd$tracks[[1]]))

  loadedtrack1hash <- expect_no_error(hashtrack(load_gpx(
    fs::path_package("extdata", "kaaterskill.gpx",
                     package="databasing4patches"))$tracks[[1]]))
  loadedtrack2hash <- expect_no_error(hashtrack(load_gpx(
    fs::path_package("extdata", "huckleberry.gpx",
                     package="databasing4patches"))$tracks[[1]]))
  loadedtrack3hash <- expect_no_error(hashtrack(load_gpx(
    fs::path_package("extdata", "sherrillnd.gpx",
                     package="databasing4patches"))$tracks[[1]]))

  expect_equal(savedtrack1hash, loadedtrack1hash)
  expect_equal(savedtrack2hash, loadedtrack2hash)
  expect_equal(savedtrack3hash, loadedtrack3hash)
})
