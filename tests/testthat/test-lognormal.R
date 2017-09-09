context("lognormal")

test_that("Check inputs", {
  expect_error(LognormalParams())
  expect_error(LognormalParams(), "No mean was specified")
  expect_error(LognormalParams(3), "No CV was specified")

  expect_silent(LognormalParams(5, 4))

})
