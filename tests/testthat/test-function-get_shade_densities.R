test_that("get_shade_densities() works", {

  expect_error(
    kwb.monitoring:::get_shade_densities()
    # argument "bottleNumbers" is missing, with no default
  )

})

