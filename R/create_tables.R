
.create_season_enum <- function(con) {
  con |> DBI::dbExecute("CREATE TYPE season AS ENUM ('winter', 'spring', 'summer', 'fall')")
}

create_table_functions <- list()

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
                          track_id VARCHAR,
                          peak_id VARCHAR,
                          peak_time TIMESTAMP,
                          record_time TIMESTAMP,
                          date_in_ny DATE,
                          season_in_ny season,
                          month_in_ny INTEGER
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
                          date_in_ny DATE,
                          season_in_ny season,
                          month_in_ny INTEGER
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

# .create_track_trail_times_view <- function(con) {
#   con |> DBI::dbExecute("CREATE VIEW track_trail_times 
#                          AS SELECT t.track_id, trail_id, point_id,
#                                    date_in_ny, season_in_ny, month_in_ny
#                          FROM tracks t, track_trails tt 
#                          WHERE t.track_id = tt.track_id")
# }
