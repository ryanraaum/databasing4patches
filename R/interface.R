

## DB Interface Object

#' @title DB4Patches Object
#'
#' @description
#' Interface for a hiking tracks, bagged points, and collected trails database.
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
     #' Check if a point is already in the database
     #'
     #' @param lat The point latitude
     #' @param lon The point longitude
     #' @param plusminus The plus/minus degree range difference allowed
     #' @param conn (Internal) Database pool connection
     point_exists = function(lat, lon, plusminus=0.01, conn=self$con) {
       lat_low <- lat - plusminus
       lat_high <- lat + plusminus
       lon_low <- lon - plusminus
       lon_high <- lon + plusminus
       response <- DBI::dbGetQuery(conn, glue::glue("SELECT point_id FROM points WHERE
                                                        point_lat > {lat_low} AND
                                                        point_lat < {lat_high} AND
                                                        point_lon > {lon_low} AND
                                                        point_lon < {lon_high}"))
       return(nrow(response) == 1)
     },
     #' @description
     #' Add a point to the database
     #'
     #' @param lat Latitude in degrees in WGS84 CRS
     #' @param lon Longitude in degrees in WGS84 CRS
     #' @param name point name
     #' @param elev_m Elevation in meters
     #' @param elev_ft Elevation in feet
     #' @param conn (Internal) Database pool connection
     add_point = function(lat, lon, name=NA, elev_m=NA, elev_ft=NA, conn=self$con) {
       if (!self$point_exists(lat, lon, conn=conn)) {
         point_id <- uuid::UUIDgenerate()
         point_version <- lubridate::ymd("1962-01-01")
         if (!is.na(elev_m) & is.na(elev_ft)) {
           elev_ft <- round(elev_m * 3.281)
         } else if (is.na(elev_m) & !is.na(elev_ft)) {
           elev_m <- round(elev_ft / 3.281)
         }
         var_names <- c("point_id", "point_lat", "point_lon", "point_elev_m", "point_elev_ft", "point_version", "point_name")
         var_targets <- c("'{point_id}'", "'{lat}'", "'{lon}'", "'{elev_m}'", "'{elev_ft}'", "'{point_version}'", "'{name}'")
         var_values <- c(point_id, lat, lon, elev_m, elev_ft, point_version, name)
         filled_values <- !is.na(var_values)
         names_for_sql <- paste(var_names[filled_values], collapse=", ")
         targets_for_sql <- paste(var_targets[filled_values], collapse=", ")
         response <- DBI::dbExecute(conn, glue::glue(glue::glue("INSERT INTO points ({names_for_sql})
                                                VALUES ({targets_for_sql})")))
         assertthat::are_equal(response, 1)
         return(point_id)
       } else {
         return(NA)
       }
     },
     #' @description
     #' Add point(s) in sf data frame to the database
     #'
     #' @param sfdf sf data frame
     #' @param name_col Name of name column
     #' @param elev_m_col Name of meters elevation column
     #' @param elev_ft_col Name of feet elevation column
     add_point_sfdf = function(sfdf,
                              name_col=NA,
                              elev_m_col=NA,
                              elev_ft_col=NA) {
       pool::poolWithTransaction(self$con, function(this_conn) {
         added_points <- vector("character", nrow(sfdf))
         for (i in seq_len(nrow(sfdf))) {
           this_point <- sfdf[i,]
           point_name <- ifelse(is.na(name_col), NA, sfdf[i, name_col])
           point_elev_m <- ifelse(is.na(elev_m_col), NA, sfdf[i, elev_m_col])
           point_elev_ft <- ifelse(is.na(elev_ft_col), NA, sfdf[i, elev_ft_col])
           point_coords <- sf::st_coordinates(sfdf[i, "geometry"])
           point_lat <- point_coords[1,1]
           point_lon <- point_coords[1,2]
           added_points[i] <- self$add_point(point_lat, point_lon,
                                           name = point_name,
                                           elev_m = point_elev_m,
                                           elev_ft = point_elev_ft,
                                           conn = this_conn)
           if (is.na(added_points[i])) {
             stop(glue::glue("Problem adding row {i} to the points table."))
           }
         }
         return(added_points)
       })
     },
     #' @description
     #' Add a point set to the database
     #'
     #' @param name Point set name
     #' @param points_type "peaks" | "fire towers"
     #' @param version Start date for this version of the point set
     #' @param single To be completed within a single year (or contiguous season)
     # @param single_type Time frame of month, season, or year
     #' @param depreciated Date (if) this version has been superseded/ended
     #' @param notes Notes
     add_point_set = function(name, points_type, version,
                             single=FALSE,
                             # single_type=NA,
                             depreciated=NA, notes=NA) {
       point_set_id <- uuid::UUIDgenerate()
       points_type <- tolower(points_type)
       assertthat::assert_that(points_type %in% c("peaks", "fire towers"))
       var_names <- c("point_set_id", "point_set_name", "point_set_type", "point_set_version", "point_set_single", "point_set_depreciated", "point_set_notes")
       var_targets <- c("'{point_set_id}'", "'{name}'", "'{points_type}'", "'{version}'", "'{single}'", "'{depreciated}'", "'{notes}'")
       var_values <- c(point_set_id, name, points_type, version, single, depreciated, notes)
       filled_values <- !is.na(var_values)
       names_for_sql <- paste(var_names[filled_values], collapse=", ")
       targets_for_sql <- paste(var_targets[filled_values], collapse=", ")
       response <- DBI::dbExecute(self$con,
          glue::glue(glue::glue("INSERT INTO point_sets ({names_for_sql})
                                 VALUES ({targets_for_sql})")))
       assertthat::are_equal(response, 1)
       return(point_set_id)
     },
     #' @description
     #' Add a point set requirement to the database
     #'
     #' @param point_set_id Point set id
     #' @param point_id Point id
     #' @param month If needs to be completed in a specific month, which?
     #' @param season If needs to be completed in a specific season, which?
     add_point_set_requirement = function(point_set_id, point_id,
                              month=NA, season=NA) {
       point_set_requirement_id <- uuid::UUIDgenerate()

       var_names <- c("point_set_requirement_id", "point_set_id", "point_id", "point_month", "point_season")
       var_targets <- c("'{point_set_requirement_id}'", "'{point_set_id}'", "'{point_id}'", "'{month}'", "'{season}'")
       var_values <- c(point_set_requirement_id, point_set_id, point_id, month, season)
       filled_values <- !is.na(var_values)
       names_for_sql <- paste(var_names[filled_values], collapse=", ")
       targets_for_sql <- paste(var_targets[filled_values], collapse=", ")
       response <- DBI::dbExecute(self$con,
                                  glue::glue(glue::glue("INSERT INTO point_set_requirements ({names_for_sql})
                                 VALUES ({targets_for_sql})")))
       assertthat::are_equal(response, 1)
       return(point_set_requirement_id)
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
