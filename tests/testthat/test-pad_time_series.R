starting_data = data.frame(date = c(
  as.POSIXct("1970-01-01 00:00", format = "%Y-%m-%d %H:%M"),
  as.POSIXct("1970-01-01 23:00", format = "%Y-%m-%d %H:%M")
))

successful_pad = data.frame(date = seq(
  from = as.POSIXct("1970-01-01 00:00", format = "%Y-%m-%d %H:%M"),
  to = as.POSIXct("1970-01-01 23:00", format = "%Y-%m-%d %H:%M"),
  by = 3600
))

test_that("success input is data.frame", {
  expect_identical(pad_time_series(.data = starting_data,
                                   period = 3600),
                   successful_pad)
})

test_that("success input is tibble", {
  expect_identical(pad_time_series(.data = tibble(starting_data),
                                   period = 3600),
                   tibble(successful_pad))
})

test_that("error input has no date column", {
  no_date = starting_data
  names(no_date) = "timestamp"

  expect_error(pad_time_series(.data = no_date,
                               period = 3600))
})

test_that("error input date not POSIXct", {
  not_posixct = starting_data
  not_posixct$date = as.numeric(not_posixct$date)

  expect_error(pad_time_series(.data = not_posixct,
                               period = 3600))
})
