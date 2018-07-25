test_that("prepareDataForOverviewPlot() works", {

  expect_error(
    kwb.monitoring:::prepareDataForOverviewPlot()
    # argument "dataFrame" is missing, with no default
  )

})

