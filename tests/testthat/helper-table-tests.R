## test helpers

make_testcon <- function(dbtype="sqlite", env=parent.frame()) {
  if (dbtype == "sqlite") {
    testcon <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  } else if (dbtype == "duckdb") {
    testcon <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  } else {
    stop(glue::glue("not a known dbtype: '{dbtype}'"))
  }
  withr::defer(
    DBI::dbDisconnect(testcon),
    envir = env
  )
  testcon
}

make_testpool <- function(dbtype="sqlite", env=parent.frame()) {
  if (dbtype == "sqlite") {
    testcon <- pool::dbPool(RSQLite::SQLite(), dbdir=":memory:")
  } else if (dbtype == "duckdb") {
    testcon <- pool::dbPool(duckdb::duckdb(), dbdir=":memory:")
  } else {
    stop(glue::glue("not a known dbtype: '{dbtype}'"))
  }
  withr::defer(
    pool::poolClose(testcon),
    envir = env
  )
  testcon
}

make_testdbobj <- function(dbtype="sqlite", env=parent.frame(), create_tables=FALSE) {
  if (dbtype == "sqlite") {
    requireNamespace("RSQLite", quietly = TRUE)
    dbobj <- DB4Patches$new(list(drv=RSQLite::SQLite(), dbdir=":memory:"),
                            create_tables=create_tables)
  } else if (dbtype == "duckdb") {
    requireNamespace("duckdb", quietly = TRUE)
    dbobj <- DB4Patches$new(list(drv=duckdb::duckdb(), dbdir=":memory:"),
                            create_tables=create_tables)
  } else {
    stop(glue::glue("not a known dbtype: '{dbtype}'"))
  }
  # withr::defer(
  #   dbobj$disconnect(),
  #   envir = env
  # )
  dbobj
}

# table_columns <- function(con, table_name) {
#   DBI::dbListFields(con, table_name)
# }

supported_databases <- function(just_this_one=NULL) {
  if (is.null(just_this_one)) {return(c("duckdb", "sqlite"))}
  return(just_this_one)
}

# is_sqlite_connection <- function(conn) {
#   if (inherits(conn, "Pool")) {
#     return("SQLiteConnection" %in% conn$objClass)
#   }
#   return(isa(conn, "SQLiteConnection"))
# }
