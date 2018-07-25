test_that("max_value_to_limit() works", {

  expect_error(
    kwb.monitoring:::max_value_to_limit()
    # argument "maxValue" is missing, with no default
  )

})

