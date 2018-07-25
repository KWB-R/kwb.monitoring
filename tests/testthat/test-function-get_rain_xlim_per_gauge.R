test_that("get_rain_xlim_per_gauge() works", {

  expect_error(
    kwb.monitoring:::get_rain_xlim_per_gauge()
    # argument "gauges" is missing, with no default
  )

})

