#' date_from_decimal_day
#'
#' converts DOY.decimalday into a POSIXct object.
#'
#' @param decimal_day date stored as DOY.decimalday
#' @param date_origin origin of decimal_day as YYYY/MM/DD
#' @param tz timezone
#'
#' @return decimal_day as POSIXct
#'
#' @export

date_from_decimal_day = function(decimal_day, date_origin, tz = "UTC") {
  doy = floor(decimal_day)
  decimal = decimal_day %% 1

  seconds = 86400 * decimal

  day = as.Date(doy-1, origin = date_origin)  |>
    as.character() |>
    as.POSIXct(tz = tz, format = "%Y-%m-%d")

  #
  day + seconds
}
