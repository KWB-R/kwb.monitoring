#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("getHydraulicEvents() works", {

  expect_error(
    kwb.monitoring:::getHydraulicEvents()
    # argument "settings" is missing, with no default
  )

})

