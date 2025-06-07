
# NOTE: seasons are winter, spring, summer, fall

create_table_functions <- list()

.create_points_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE points (
                          point_id VARCHAR,
                          point_lat FLOAT,
                          point_lon FLOAT,
                          point_elev_m INTEGER,
                          point_elev_ft INTEGER,
                          point_version TIMESTAMP,
                          point_depreciated TIMESTAMP,
                          point_name VARCHAR
  )")
}
create_table_functions$points <- .create_points_table

.create_point_sets_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE point_sets (
                          point_set_id VARCHAR,
                          point_set_name VARCHAR,
                          point_set_type VARCHAR,
                          point_set_single BOOLEAN,
                          point_set_single_type VARCHAR,
                          point_set_version TIMESTAMP,
                          point_set_depreciated TIMESTAMP,
                          point_set_notes VARCHAR
  )")
}
create_table_functions$point_sets <- .create_point_sets_table

.create_point_set_requirements_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE point_set_requirements (
                          point_set_id VARCHAR,
                          point_set_requirement_id VARCHAR,
                          point_id VARCHAR,
                          point_month INTEGER,
                          point_season VARCHAR
  )")
}
create_table_functions$point_set_requirements <- .create_point_set_requirements_table

.create_point_set_completions_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE point_set_completions (
                          user_id UUID,
                          point_set_id VARCHAR,
                          point_set_requirement_id VARCHAR,
                          point_id VARCHAR,
                          point_month INTEGER,
                          point_season VARCHAR,
                          requirement_completed BOOLEAN DEFAULT false,
                          track_point_id VARCHAR

  )")
}
create_table_functions$point_set_completions <- .create_point_set_completions_table


.create_trails_table <- function(con) {
  # trail_id is a 6 character geohash of trail start coordinates
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


.create_track_points_table <- function(con) {
  con |> DBI::dbExecute("CREATE TABLE track_points (
                          track_point_id VARCHAR,
                          track_id VARCHAR,
                          point_id VARCHAR,
                          point_time TIMESTAMP,
                          record_time TIMESTAMP,
                          local_date DATE,
                          local_season VARCHAR,
                          local_month INTEGER
  )")
}
create_table_functions$track_points <- .create_track_points_table

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
