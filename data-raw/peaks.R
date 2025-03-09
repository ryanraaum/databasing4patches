## code to prepare `peaks` dataset goes here

peaks <- sf::read_sf("data-raw/peaks_sf.geojson") |>
  dplyr::select(name) |>
  dplyr::rename(peak_id = name)
usethis::use_data(peaks, overwrite = TRUE)
