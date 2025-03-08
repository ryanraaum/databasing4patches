
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
