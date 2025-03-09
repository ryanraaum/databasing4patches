
kaaterskill <- load_gpx(fs::path_package("extdata", "kaaterskill.gpx",
                                         package="databasing4patches"))
usethis::use_data(kaaterskill, overwrite = TRUE)
