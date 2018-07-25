test_that("get_appropriate_xlim() works", {

  expect_error(
    kwb.monitoring:::get_appropriate_xlim()
    # argument "timestamps" is missing, with no default
  )

})

