% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date_from_decimal_day.R
\name{date_from_decimal_day}
\alias{date_from_decimal_day}
\title{date from DOY}
\usage{
date_from_decimal_day(decimal_day, origin, tz = "UTC")
}
\arguments{
\item{decimal_day}{date stored as date.decimalday}

\item{origin}{origin of decimal_day as YYYY/MM/DD}

\item{tz}{timezone}
}
\value{
decimal_day as POSIXct
}
\description{
converts days.decimalday into a POSIXct object. - note as this is decimal day, Jan 1st 1200 is 0.5
}
