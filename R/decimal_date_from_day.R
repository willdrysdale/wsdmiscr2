#' Decimal day from date
#'
#' convert a POSIXct in to a number representing decimal date, where Jan 1st 12:00 == 0.5, if the origin is YYYY-01-01 00:00:00.
#' Specification of origin allows for negative values e.g if origin is 2020-01-01 00:00, but the first data point is at 2019-12-31 12:00
#' the deciaml day returned will be -0.5.
#'
#' @param date a POSIXct date
#' @param origin specify the origin date as a POSIXct. Usually YYYY-01-01 00:00:00 for the most relevant year to the data.#
#' @param tz time zone string, defaults to UTC.
#'
#' @author W. S. Drysdale
#'
#' @export

decimal_day_from_date = function(date, origin, tz = "UTC"){

  diff = as.numeric(date)-as.numeric(origin)

  total_days = diff %/% 86400
  remaining = diff %% 86400
  fractional_days = remaining/(86400)

  total_days+fractional_days

}
