test_that("success", {
  expect_identical(
    DOY_from_date(as.POSIXct("1970-01-01 12:00", tz = "UTC")),
    1.5
  )
})

test_that("success day 60 none leap year", {
  expect_identical(DOY_from_date(as.POSIXct("1970-03-01", tz = "UTC")),
                   60)
})

test_that("success day 60 leap year", {
  expect_identical(DOY_from_date(as.POSIXct("1970-03-01", tz = "UTC")),
                   60)
})
