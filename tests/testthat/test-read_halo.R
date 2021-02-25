test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

data(Sleep_Sessions)

test_that("BedTime time zone is correct", {
  expect_equal(lubridate::with_tz(Sleep_Sessions$BedTime[1] -
                                    lubridate::hours(Sleep_Sessions$`Local Time Offset`[1]),
                                  tzone = "America/Los_Angeles"
                                  ),
               lubridate::as_datetime("2020-11-27 05:46:01", tz = "America/Los_Angeles"))
})
