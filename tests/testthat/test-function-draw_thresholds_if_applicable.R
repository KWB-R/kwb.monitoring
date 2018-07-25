test_that("draw_thresholds_if_applicable() works", {

  expect_error(
    kwb.monitoring:::draw_thresholds_if_applicable()
    # argument "time.dependent.thresholds" is missing, with no default
  )

})

