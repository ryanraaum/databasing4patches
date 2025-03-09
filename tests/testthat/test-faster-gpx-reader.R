test_that("faster load_gpx reader works", {
  gpxfile1 <- expect_no_condition(
    fs::path_package("extdata", "kaaterskill.gpx",
                     package="databasing4patches"))
  gpxfile2 <- expect_no_condition(
    fs::path_package("extdata", "huckleberry.gpx",
                     package="databasing4patches"))
  gpxfile3 <- expect_no_condition(
    fs::path_package("extdata", "sherrillnd.gpx",
                     package="databasing4patches"))
  tracks1 <- expect_no_condition(load_gpx(gpxfile1))
  tracks2 <- expect_no_condition(load_gpx(gpxfile2))
  tracks3 <- expect_no_condition(load_gpx(gpxfile3))

  KEY_TRACK_COLUMNS <- c("Elevation", "Time", "Latitude", "Longitude")
  for (t in list(tracks1, tracks2, tracks3)) {
    expect_true("tracks" %in% names(t))
    expect_true(length(t$tracks) == 1)
    expect_true(inherits(t$tracks[[1]], "data.frame"))
    for (col in KEY_TRACK_COLUMNS) {
      expect_true(col %in% colnames(t$tracks[[1]]))
    }
  }
})
