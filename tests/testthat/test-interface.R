
test_that("database pool connections can be created", {
  for (db in supported_databases()) {
    this_db <- expect_no_condition(make_testpool(db))

  }
})

test_that("DB4Patches objects can be created", {
  for (db in supported_databases()) {
    this_db <- expect_no_condition(make_testdbobj(db))
  }
})

test_that("Tables can be created by DB4Patches object", {
  for (db in supported_databases()) {
    this_db <- expect_no_condition(make_testdbobj(db))
    expect_no_condition(this_db$create_tables())
    expected_tables <- names(create_table_functions)
    created_tables <- DBI::dbListTables(this_db$con)
    for (table in expected_tables) {
      expect_true(table %in% created_tables)
    }
    expect_error(this_db$create_tables()) # tables have already been created
  }
})

test_that("track_exists is FALSE when track_id is not there", {
  for (db in supported_databases()) {
    this_db <- expect_no_condition(make_testdbobj(db, create_tables=TRUE))
    expect_false(this_db$track_exists("FJDK90w2fskldjfds"))
  }
})

test_that("add_track works, and then track_exists finds it", {
  for (db in supported_databases()) {
    this_db <- expect_no_condition(make_testdbobj(db, create_tables=TRUE))
    a_track <- kaaterskill$tracks[[1]]
    track_id <- expect_no_error(this_db$add_track(a_track))
    expect_true(this_db$track_exists(track_id))
  }
})
