test_that("get_appropriate_ylim() works", {

  expect_error(
    kwb.monitoring:::get_appropriate_ylim()
    # argument "gaugeIndices" is missing, with no default
  )

})

