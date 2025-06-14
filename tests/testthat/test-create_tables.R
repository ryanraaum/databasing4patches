
test_that("create_tables creates sqlite tables", {
  testcon <- make_testcon("sqlite")
  expect_no_error(d4p_create_tables(testcon))
})

test_that("create_tables creates duckdb tables", {
  testcon <- make_testcon("duckdb")
  expect_no_error(d4p_create_tables(testcon))
})

test_that("create_tables creates individual tables", {
  testcon <- make_testcon("duckdb")
  for (table in names(create_table_functions)) {
    expect_no_error(d4p_create_tables(testcon, table))
  }
})

test_that("create_tables warns about unknown tables", {
  testcon <- make_testcon("duckdb")
  expect_warning(d4p_create_tables(testcon, "unknown"))
})

