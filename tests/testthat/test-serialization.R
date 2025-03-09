test_that("cerealization and decerealization works", {
  expect_equal(decerealize(cerealize(mtcars)), mtcars)
})
