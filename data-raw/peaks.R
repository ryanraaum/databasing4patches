## code to prepare `peaks` dataset goes here

peaks <- sf::read_sf("data-raw/peaks_sf.geojson") |>
  dplyr::select(name) |>
  dplyr::rename(peak_id = name)
usethis::use_data(peaks, overwrite = TRUE)


peaks_data <- sf::read_sf("data-raw/peaks_sf.geojson") |>
  dplyr::select(name, elev_m, elev_ft) |>
  dplyr::rename(peak_name = name,
                peak_elev_m = elev_m,
                peak_elev_ft = elev_ft)

peaks_coordinates <- peaks_data |>
  sf::st_coordinates() |>
  tibble::as_tibble() |>
  dplyr::rename(peak_lat = Y,
                peak_lon = X)

peaks_import_data <- peaks_data |>
  sf::st_drop_geometry() |>
  dplyr::bind_cols(peaks_coordinates)
