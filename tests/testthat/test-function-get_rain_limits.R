test_that("get_rain_limits() works", {

  expect_error(
    kwb.monitoring:::get_rain_limits()
    # argument "values" is missing, with no default
  )

})

