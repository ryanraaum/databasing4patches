
#' A stripped down fast GPX file reader
#'
#' If you want *ONLY* the tracks in a GPX file - no waypoints,
#' no routes - this reads that in quickly. Or, at least, a LOT
#' more quickly than the `gpx` package on CRAN.
#'
#' @param gpxfile A path to a GPX file
#'
#' @returns A list with a `tracks` sublist
#' @export
#'
#' @examples
#' gpxfile <- system.file("extdata", "kaaterskill.gpx",
#'                        package="databasing4patches")
#' gpxdata <- load_gpx(gpxfile)
#' gpxdata$tracks[[1]]
load_gpx <- function(gpxfile) {
  parsedfile <- XML::htmlTreeParse(file = gpxfile,
                         error = function(...) {},
                         useInternalNodes = T)

  tracks <- XML::xpathApply(parsedfile, path="//trk")
  ntracks <- length(tracks)
  result <- vector("list", ntracks)

  for (i in seq_len(ntracks)) {
    segments <- XML::xpathApply(tracks[[i]], path=".//trkseg")
    segments_list <- vector("list", length(segments))
    for (j in seq_along(segments)) {
      elevation <- as.numeric(XML::xpathApply(segments[[j]], path = ".//trkpt/ele", XML::xmlValue))
      time <- XML::xpathSApply(segments[[j]], path = ".//trkpt/time", XML::xmlValue)
      coords <- XML::xpathSApply(segments[[j]], path = ".//trkpt", XML::xmlAttrs)
      if (!is.null(ncol(coords))) {
        latitude <- as.numeric(coords["lat",])
        longitude <- as.numeric(coords["lon",])
        segments_list[[j]] <- tibble::tibble(Elevation=elevation,
                                             Time=time,
                                             Latitude=latitude,
                                             Longitude=longitude,
                                             `Segment ID`=j)
      }
    }
    result[[i]] <- dplyr::bind_rows(segments_list)
  }
  list(tracks=result)
}

# benchmark_faster_gpx_reader <- function() {
#   library(bench)
#
#   simple_gpx_file <- "tracks/kaaterskill.gpx"
#   complex_gpx_file <- "~/Dropbox/Personal/Hiking-GPS-Tracks/Catskills/avenza-export.gpx"
#
#   result = list()
#
#   result$simple = bench::mark(
#     gpx=nrow(gpx::read_gpx(simple_gpx_file)$tracks[[1]]),
#     xpath1=nrow(load_gpx(simple_gpx_file)$tracks[[1]]),
#     min_iterations = 20
#   )
#
#   result$complex = bench::mark(
#     gpx=nrow(gpx::read_gpx(complex_gpx_file)$tracks[[1]]),
#     xpath1=nrow(load_gpx(complex_gpx_file)$tracks[[1]]),
#     min_iterations = 20
#   )

#   result
# }
#
#
