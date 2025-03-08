
test_that("database pool connections can be created", {
  for (db in supported_databases()) {
    this_db <- expect_no_condition(make_testpool(db))

  }
})

test_that("DB4Patches can be created", {
  for (db in supported_databases()) {
    this_db <- expect_no_condition(make_testdbobj(db))
  }
})
