test_that("addFakeEntriesForDaysWithoutData() works", {

  expect_error(
    kwb.monitoring:::addFakeEntriesForDaysWithoutData()
    # argument "dataFrame" is missing, with no default
  )

})

