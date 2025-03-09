

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
