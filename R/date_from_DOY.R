#' date from DOY
#'
#' converts DOY.decimalday into a POSIXct object. - note as this is DOY, Jan 1st 1200 is 1.5
#'
#' @param decimal_day date stored as DOY.decimalday
#' @param origin origin of decimal_day as YYYY/MM/DD
#' @param tz timezone
#'
#' @return decimal_day as POSIXct
#'
#' @export

date_from_DOY = function(decimal_day, origin, tz = "UTC") {
  doy = floor(decimal_day)
  decimal = decimal_day %% 1

  seconds = 86400 * decimal

  day = as.Date(doy-1, origin = origin)  |>
    as.character() |>
    as.POSIXct(tz = tz, format = "%Y-%m-%d")

  #
  day + seconds
}
