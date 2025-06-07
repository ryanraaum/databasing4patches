

## DB Interface Object

#' @title DB4Patches Object
#'
#' @description
#' Interface for a hiking tracks, bagged peaks, and collected trails database.
#'
#' @export
DB4Patches <- R6::R6Class(classname = "db4patches_object",
   private = list(
     pool = NULL
   ),
   public = list(
     #' @description
     #' Create new DB4Patches object
     #'
     #' @param dbargs_list A list of parameters to create the database
     #' @param create_tables Create tables in the database (default: FALSE)
     initialize = function(dbargs_list, create_tables=FALSE) {
       # super$initialize()
       self$establish_connection(dbargs_list)
       if (create_tables) {
         self$create_tables()
       }
     },
     #' @description
     #' Create DB4Patches object and connect to a database
     #'
     #' @param dbargs_list A list of parameters to create the database
     establish_connection = function(dbargs_list) {
       if (is.null(private$pool) || !dbIsValid(private$pool)) {
         private$pool <- do.call(pool::"dbPool", dbargs_list)
       }
       invisible(self)
     },
     #' @description
     #' Disconnect from a database
     #'
     #' @returns Nothing
     disconnect = function() {
       if (!is.null(private$pool)) {
         pool::poolClose(private$pool)
         private$pool <- NULL
       }
     },
     #' @description
     #' Clean up after removal of DB4Patches object
     finalize = function() {
       self$disconnect()
     },
     #' @description
     #' Generate a data frame interface to a database table
     #'
     #' @param tbl_name A database table name
     create_tables = function() {
       current_tables <- DBI::dbListTables(self$con)
       proposed_tables <- names(create_table_functions)
       if (length(intersect(current_tables, proposed_tables)) > 0) {
         stop("Cannot create tables - database already has tables.")
       }
       for (table in proposed_tables) {
         create_table_functions[[table]](self$con)
       }
     },
     #' @description
     #' Generate a data frame interface to a database table
     #'
     #' @param tbl_name A database table name
     tbl = function(tbl_name) {
       return(dplyr::tbl(private$pool, tbl_name))
     },
     #' @description
     #' Check if a track is already in the database
     #'
     #' @param track_id The track hash
     track_exists = function(track_id) {
       response <- DBI::dbGetQuery(self$con, glue::glue("SELECT track_id FROM tracks WHERE track_id = '{track_id}'"))
       return(nrow(response) == 1)
     },
     #' @description
     #' Add a track to the database
     #'
     #' @param track A GPX-style track data frame
     add_track = function(track) {
       track_id <- hashtrack(track)
       if (!self$track_exists(track_id)) {
         track_data <- cerealize(track)
         response <- DBI::dbExecute(self$con, glue::glue("INSERT INTO tracks (track_id, track_data)
                                                VALUES ('{track_id}', '{track_data}')"))
         assertthat::are_equal(response, 1)
       }
       return(track_id)
     },
     #' @description
     #' Check if a peak is already in the database
     #'
     #' @param lat The peak latitude
     #' @param lon The peak longitude
     #' @param plusminus The plus/minus degree range difference allowed
     #' @param conn (Internal) Database pool connection
     peak_exists = function(lat, lon, plusminus=0.01, conn=self$con) {
       lat_low <- lat - plusminus
       lat_high <- lat + plusminus
       lon_low <- lon - plusminus
       lon_high <- lon + plusminus
       response <- DBI::dbGetQuery(conn, glue::glue("SELECT peak_id FROM peaks WHERE
                                                        peak_lat > {lat_low} AND
                                                        peak_lat < {lat_high} AND
                                                        peak_lon > {lon_low} AND
                                                        peak_lon < {lon_high}"))
       return(nrow(response) == 1)
     },
     #' @description
     #' Add a peak to the database
     #'
     #' @param lat Latitude in degrees in WGS84 CRS
     #' @param lon Longitude in degrees in WGS84 CRS
     #' @param name Peak name
     #' @param elev_m Elevation in meters
     #' @param elev_ft Elevation in feet
     #' @param conn (Internal) Database pool connection
     add_peak = function(lat, lon, name=NA, elev_m=NA, elev_ft=NA, conn=self$con) {
       if (!self$peak_exists(lat, lon, conn=conn)) {
         peak_id <- uuid::UUIDgenerate()
         peak_version <- lubridate::ymd("1962-01-01")
         if (!is.na(elev_m) & is.na(elev_ft)) {
           elev_ft <- round(elev_m * 3.281)
         } else if (is.na(elev_m) & !is.na(elev_ft)) {
           elev_m <- round(elev_ft / 3.281)
         }
         var_names <- c("peak_id", "peak_lat", "peak_lon", "peak_elev_m", "peak_elev_ft", "peak_version", "peak_name")
         var_targets <- c("'{peak_id}'", "'{lat}'", "'{lon}'", "'{elev_m}'", "'{elev_ft}'", "'{peak_version}'", "'{name}'")
         var_values <- c(peak_id, lat, lon, elev_m, elev_ft, peak_version, name)
         filled_values <- !is.na(var_values)
         names_for_sql <- paste(var_names[filled_values], collapse=", ")
         targets_for_sql <- paste(var_targets[filled_values], collapse=", ")
         response <- DBI::dbExecute(conn, glue::glue(glue::glue("INSERT INTO peaks ({names_for_sql})
                                                VALUES ({targets_for_sql})")))
         assertthat::are_equal(response, 1)
         return(peak_id)
       } else {
         return(NA)
       }
     },
     #' @description
     #' Add peak(s) in sf data frame to the database
     #'
     #' @param sfdf sf data frame
     #' @param name_col Name of name column
     #' @param elev_m_col Name of meters elevation column
     #' @param elev_ft_col Name of feet elevation column
     add_peak_sfdf = function(sfdf,
                              name_col=NA,
                              elev_m_col=NA,
                              elev_ft_col=NA) {
       pool::poolWithTransaction(self$con, function(this_conn) {
         added_peaks <- vector("character", nrow(sfdf))
         for (i in seq_len(nrow(sfdf))) {
           this_peak <- sfdf[i,]
           peak_name <- ifelse(is.na(name_col), NA, sfdf[i, name_col])
           peak_elev_m <- ifelse(is.na(elev_m_col), NA, sfdf[i, elev_m_col])
           peak_elev_ft <- ifelse(is.na(elev_ft_col), NA, sfdf[i, elev_ft_col])
           peak_coords <- sf::st_coordinates(sfdf[i, "geometry"])
           peak_lat <- peak_coords[1,1]
           peak_lon <- peak_coords[1,2]
           added_peaks[i] <- self$add_peak(peak_lat, peak_lon,
                                           name = peak_name,
                                           elev_m = peak_elev_m,
                                           elev_ft = peak_elev_ft,
                                           conn = this_conn)
           if (is.na(added_peaks[i])) {
             stop(glue::glue("Problem adding row {i} to the peaks table."))
           }
         }
         return(added_peaks)
       })
     }
   ),
   active = list(
     #' @field con
     #' DB4Patches. Read-only.
     con = function() {
       private$pool
     }
   )
)

##
# dbplyr *is* used, but indirectly;
# this just kills an annoying packaging warning

#' Nonsense packaging issue function
#'
#' The [dbplyr] package is used, but indirectly. This function exists
#' only to kill an annoying packaging `check` warning.
#'
#' @param con A database connection
#'
#' @returns An API version.
make_dbplyr_warning_go_away <- function(con) {
  dbplyr::dbplyr_edition(con)
}

# pool *is* used, but indirectly;
# this just kills an annoying packaging warning

#' Nonsense packaging issue function
#'
#' The [pool] package is used, but indirectly. This function exists
#' only to kill an annoying packaging `check` warning.
#'
#' @param con A database connection
#'
#' @returns An API version.
make_pool_warning_go_away <- function(con) {
  pool::dbGetInfo(con)
}
