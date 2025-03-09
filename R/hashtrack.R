
#' Make unique hash for GPX track
#'
#' @param track A GPX track data frame type object
#'
#' @returns A SHA1 hash
#' @export
#'
#' @examples
#' hashtrack(kaaterskill$tracks[[1]])
hashtrack <- function(track) {
  KEY_TRACK_COLUMNS <- c("Elevation", "Time", "Latitude", "Longitude")
  assertthat::assert_that(inherits(track, "data.frame"),
                          msg="supplied track is not a data frame")
  columns <- colnames(track)
  for (this_col in KEY_TRACK_COLUMNS) {
    assertthat::assert_that(this_col %in% columns,
                            msg=glue::glue("`{this_col}` not in track data frame"))
  }
  assertthat::assert_that(nrow(track) >= 5,
                          msg="track is too short; needs to have at least 5 points")
  core_track <- track[KEY_TRACK_COLUMNS,]
  core_track$Time <- lubridate::as_datetime(core_track$Time)
  core_track <- track[order(track$Time),]
  digest::sha1(core_track$Time[1:5])
}
