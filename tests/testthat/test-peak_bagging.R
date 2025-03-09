
test_that("gpx_track_to_sf works", {
  kaaterskill_sf <- expect_no_error(gpx_track_to_sf(kaaterskill$tracks[[1]]))
  expect_true(inherits(kaaterskill_sf, "sf"))
})

test_that("make_peak_bagger_function works", {
  peak_bagger <- expect_no_error(make_peak_bagger_function(peaks))
  expect_true(is.function(peak_bagger))

  kaaterskill_peaks <- expect_no_error(peak_bagger(kaaterskill$tracks[[1]]))
  expect_true(nrow(kaaterskill_peaks) == 1)
  expect_true("Kaaterskill High Peak" %in% kaaterskill_peaks$peak_id)

  huckleberry_peaks <- expect_no_error(peak_bagger(huckleberry$tracks[[1]]))
  expect_true(is.null(huckleberry_peaks))

  sherrillnd_peaks <- expect_no_error(peak_bagger(sherrillnd$tracks[[1]]))
  expect_true(nrow(sherrillnd_peaks) == 2)
  expect_true("Sherrill" %in% sherrillnd_peaks$peak_id)
  expect_true("North Dome" %in% sherrillnd_peaks$peak_id)
})
