context("gamma")

test_that("Inputs are kosher", {
  expect_error(GammaParams())
  expect_error(GammaParams(), "No mean was specified")
  expect_error(GammaParams(3), "No CV was specified")

  expect_silent(GammaParams(5, 4))

})
