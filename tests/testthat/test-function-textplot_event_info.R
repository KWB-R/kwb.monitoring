test_that(".textplot_eventInfo() works", {

  expect_error(
    kwb.monitoring:::.textplot_eventInfo()
    # argument "settings" is missing, with no default
  )

})

