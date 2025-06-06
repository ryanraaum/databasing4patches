
# NOTE: seasons are winter, spring, summer, fall

create_table_functions <- list()

.create_peaks_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE peaks (
                          peak_id VARCHAR,
                          peak_lat FLOAT,
                          peak_lon FLOAT,
                          peak_elev_m INTEGER,
                          peak_elev_ft INTEGER,
                          peak_version TIMESTAMP,
                          peak_depreciated TIMESTAMP,
                          peak_name VARCHAR
  )")
}
create_table_functions$peaks <- .create_peaks_table

.create_peak_sets_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE peak_sets (
                          peak_set_id VARCHAR,
                          peak_set_name VARCHAR,
                          peak_set_type VARCHAR,
                          peak_set_single BOOLEAN,
                          peak_set_single_type VARCHAR,
                          peak_set_version TIMESTAMP,
                          peak_set_depreciated TIMESTAMP,
                          peak_set_notes VARCHAR
  )")
}
create_table_functions$peak_sets <- .create_peak_sets_table

.create_peak_set_requirements_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE peak_set_requirements (
                          peak_set_id VARCHAR,
                          peak_set_requirement_id VARCHAR,
                          peak_id VARCHAR,
                          peak_month INTEGER,
                          peak_season VARCHAR
  )")
}
create_table_functions$peak_set_requirements <- .create_peak_set_requirements_table

.create_peak_set_completions_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE peak_set_completions (
                          user_id UUID,
                          peak_set_id VARCHAR,
                          peak_set_requirement_id VARCHAR,
                          peak_id VARCHAR,
                          peak_month INTEGER,
                          peak_season VARCHAR,
                          requirement_completed BOOLEAN DEFAULT false,
                          track_peak_id VARCHAR

  )")
}
create_table_functions$peak_set_completions <- .create_peak_set_completions_table


.create_trails_table <- function(con) {
  # trail_id is hash of trail start coordinates rounded to 2 digits (~110m circle)
  # trail_depreciated to be set if trail update moves the start of the trail
  con |> DBI::dbExecute("CREATE TABLE trails (
                          trail_id VARCHAR,
                          point_id VARCHAR,
                          point_lat FLOAT,
                          point_lon FLOAT,
                          trail_version TIMESTAMP,
                          trail_depreciated TIMESTAMP,
                          trail_name VARCHAR
  )")
}
create_table_functions$trails <- .create_trails_table

.create_tracks_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE tracks (
                          track_id VARCHAR,
                          track_data VARCHAR
  )")
}
create_table_functions$tracks <- .create_tracks_table

.create_user_tracks_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE user_tracks (
                          user_id UUID,
                          track_id VARCHAR
  )")
}
create_table_functions$user_tracks <- .create_user_tracks_table


.create_track_peaks_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE track_peaks (
                          track_peak_id VARCHAR,
                          track_id VARCHAR,
                          peak_id VARCHAR,
                          peak_time TIMESTAMP,
                          record_time TIMESTAMP,
                          local_date DATE,
                          local_season VARCHAR,
                          local_month INTEGER
  )")
}
create_table_functions$track_peaks <- .create_track_peaks_table

.create_track_trails_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE track_trails (
                          track_id VARCHAR,
                          trail_id VARCHAR,
                          point_id VARCHAR,
                          point_time TIMESTAMP,
                          record_time TIMESTAMP,
                          local_date DATE,
                          local_season VARCHAR,
                          local_month INTEGER
  )")
}
create_table_functions$track_trails <- .create_track_trails_table


.create_users_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE users (
                          user_id UUID,
                          email VARCHAR,
                          last_seen TIMESTAMP
  )")
}
create_table_functions$users <- .create_users_table

#' Create databasing4patches tables
#'
#' @param con A DBI database connection
#' @param tables Which tables to create ("all" is default)
#'
#' @returns NULL
#' @export
#'
#' @examples
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d4p_create_tables(con)
#' DBI::dbDisconnect(con)
d4p_create_tables <- function(con, tables="all") {
  if (tables == "all") {
    for (table in names(create_table_functions)) {
      create_table_functions[[table]](con)
    }
  } else {
    for (table in tables) {
      if (table %in% names(create_table_functions)) {
        create_table_functions[[table]](con)
      } else {
        warning(glue::glue("Unknown table '{table}' - passed over without creating"))
      }
    }
  }
}
