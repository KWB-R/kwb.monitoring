#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("getStatisticsByEvent() works", {

  expect_error(
    kwb.monitoring:::getStatisticsByEvent()
    # argument "events" is missing, with no default
  )

})

