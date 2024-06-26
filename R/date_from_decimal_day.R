#' date from DOY
#'
#' converts days.decimalday into a POSIXct object. - note as this is decimal day, Jan 1st 1200 is 0.5
#'
#' @param decimal_day date stored as date.decimalday
#' @param origin origin of decimal_day as YYYY/MM/DD
#' @param tz timezone
#'
#' @return decimal_day as POSIXct
#'
#' @export

date_from_decimal_day = function(decimal_day, origin, tz = "UTC") {
  doy = floor(decimal_day)
  decimal = decimal_day %% 1

  seconds = 86400 * decimal

  day = as.Date(doy, origin = origin)  |>
    as.character() |>
    as.POSIXct(tz = tz, format = "%Y-%m-%d")

  #
  day + seconds
}
