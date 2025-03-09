
sherrillnd <- load_gpx(fs::path_package("extdata", "sherrillnd.gpx",
                                         package="databasing4patches"))
usethis::use_data(sherrillnd, overwrite = TRUE)
