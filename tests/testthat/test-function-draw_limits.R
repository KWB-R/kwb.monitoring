test_that("draw_limits() works", {

  expect_error(
    kwb.monitoring:::draw_limits()
    # argument "v" is missing, with no default
  )

})

