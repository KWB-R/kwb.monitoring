test_that("set_page_layout() works", {

  expect_error(
    kwb.monitoring:::set_page_layout()
    # argument "numberOfGauges" is missing, with no default
  )

})

