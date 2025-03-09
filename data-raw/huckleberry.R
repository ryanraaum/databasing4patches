
huckleberry <- load_gpx(fs::path_package("extdata", "huckleberry.gpx",
                                         package="databasing4patches"))
usethis::use_data(huckleberry, overwrite = TRUE)
