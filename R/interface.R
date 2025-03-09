

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

## Nonsense packaging issue function
# dbplyr *is* used, but indirectly;
# this just kills an annoying packaging warning

make_dbplyr_warning_go_away <- function(con) {
  dbplyr::dbplyr_edition(con)
}
