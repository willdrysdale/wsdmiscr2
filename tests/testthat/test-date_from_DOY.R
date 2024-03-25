test_that("success", {
  expect_identical(
    date_from_DOY(1.5, origin = "1970-01-01", tz = "UTC"),
    as.POSIXct("1970-01-01 12:00", tz = "UTC", format = "%Y-%m-%d %H:%M")
  )
})

test_that("success day 60 none leap year", {
  expect_identical(date_from_DOY(60, origin = "1970-01-01", tz = "UTC"),
                   as.POSIXct("1970-03-01", tz = "UTC", format = "%Y-%m-%d"))
})

test_that("success day 59 leap year", {
  expect_identical(date_from_DOY(60, origin = "1972-01-01", tz = "UTC"),
                   as.POSIXct("1972-02-29", tz = "UTC", format = "%Y-%m-%d"))
})
