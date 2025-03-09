
GPS_CRS <- 4326

#' Convert GPX track data frame to sf data frame
#'
#' @param gpx_track A GPX track data frame
#' @param crs EPSG coordinate reference system number (default is GPS=4326)
#'
#' @returns An sf data frame
gpx_track_to_sf <- function(gpx_track, crs=GPS_CRS) {
  time <- elevation <- NULL # package check gets confused by piped variables
  sf::st_as_sf(gpx_track, coords=c("Longitude", "Latitude"), crs=crs) |>
    janitor::clean_names() |>
    dplyr::mutate(time = lubridate::as_datetime(time)) |>
    dplyr::select(elevation, time) |>
    dplyr::rename(gps_elevation = elevation)
}

#' Make peak bagger function for given list of peaks
#'
#' Given an sf data frame with a list of peaks, this will create a function
#' that determines which peaks a given track intersects. The sf data frame
#' must have a "peak_id" column and the required sf "geometry" column and
#' should be in the GPS (4326) coordinate reference system.
#'
#' @param peaks_sf An sf data frame of peaks
#'
#' @returns A function that takes a GPX track as an argument
#' @export
#'
#' @examples
#' peak_bagger <- make_peak_bagger_function(peaks)
#' peak_bagger(kaaterskill$tracks[[1]])
make_peak_bagger_function <- function(peaks_sf) {
  # package check gets confused by piped variables
  geometry <- peak_id <- time <- distance <- time_from_min <- NULL
  # make sure that we know what we are dealing with
  assertthat::assert_that(inherits(peaks_sf, "sf"),
                          msg="peaks_sf must be an sf data frame")
  assertthat::assert_that("peak_id" %in% colnames(peaks_sf),
                          msg="a `peak_id` column is required")

  # find the center and maximum distance from center
  # then calculate a distance cutoff for attempting to find peak matches
  peaks_center <- peaks_sf |>
    dplyr::summarize(geometry = sf::st_combine(geometry)) |>
    sf::st_convex_hull() |>
    sf::st_centroid()
  distance_from_center <- peaks_sf |>
    sf::st_distance(peaks_center)
  distance_cutoff <- round(max(distance_from_center) * 1.25)

  # return a function that finds tracks intersecting this specific set of peaks
  return(function(track) {
    found_peaks <- NULL
    points <- peaks_sf
    polygons <- sf::st_buffer(points, dist=100)
    track_sf <- gpx_track_to_sf(track)
    if ((nrow(track_sf) > 0) & all(sf::st_distance(track_sf[1,], peaks_center) < distance_cutoff)) {
      near_peaks <- sf::st_join(track_sf, polygons, join=sf::st_within, left=FALSE)
      if (nrow(near_peaks) > 0) {
        found_peaks <- near_peaks |>
          dplyr::mutate(distance = apply(sf::st_distance(geometry, points |> dplyr::filter(peak_id == peak_id)), 1, min)) |>
          dplyr::group_by(peak_id) |>
          dplyr::mutate(time_from_min = time - min(time)) |>
          dplyr::filter(time_from_min < 300) |> # just the first time
          dplyr::arrange(distance) |>
          dplyr::slice(1) |>
          dplyr::ungroup() |>
          dplyr::select(peak_id, time, distance) |>
          dplyr::arrange(time)
      }
    }
    return(found_peaks)
  })
}
