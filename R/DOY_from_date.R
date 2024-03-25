#' DOY from date
#'
#' convert a POSIXct in to a number representing DOY, where Jan 1st 12:00 == 1.5
#'
#' @param date a POSIXct date
#'
#' @author W. S. Drysdale
#'
#' @export

DOY_from_date = function(date){

  DOY = as.numeric(lubridate::yday(date))
  h = as.numeric(lubridate::hour(date) * 3600)
  m = as.numeric(lubridate::minute(date) * 60)
  s = as.numeric(lubridate::second(date))
  all_seconds = h + m + s
  decimal_day = all_seconds/86400
  DOY_dd = DOY + decimal_day

  DOY_dd

}
