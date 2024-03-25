test_that("success", {
  expect_identical(
    decimal_day_from_date(as.POSIXct("1970-01-01 12:00", tz = "UTC"), origin = as.POSIXct("1970-01-01 00:00", tz = "UTC"), tz = "UTC"),
    0.5
  )
})

test_that("success day 59 none leap year", {
  expect_identical(decimal_day_from_date(as.POSIXct("1970-03-01", tz = "UTC"), origin = as.POSIXct("1970-01-01 00:00", tz = "UTC"), tz = "UTC"),
                   59)
})

test_that("success day 59 leap year", {
  expect_identical(decimal_day_from_date(as.POSIXct("1970-03-01", tz = "UTC"), origin = as.POSIXct("1970-01-01 00:00", tz = "UTC"), tz = "UTC"),
                   59)
})
