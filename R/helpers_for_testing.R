
supported_databases <- function(just_this_one=NULL) {
  if (is.null(just_this_one)) {return(c("duckdb", "sqlite"))}
  return(just_this_one)
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

make_testdbobj <- function(dbtype="sqlite", env=parent.frame()) {
  if (dbtype == "sqlite") {
    dbobj <- DB4Patches$new(list(drv=RSQLite::SQLite(), dbdir=":memory:"))
  } else if (dbtype == "duckdb") {
    dbobj <- DB4Patches$new(list(drv=duckdb::duckdb(), dbdir=":memory:"))
  } else {
    stop(glue::glue("not a known dbtype: '{dbtype}'"))
  }
  # withr::defer(
  #   dbobj$disconnect(),
  #   envir = env
  # )
  dbobj
}
