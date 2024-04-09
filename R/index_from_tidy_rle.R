#' Index from tidy_rle
#'
#' Takes the output from `tidy_rle()` and produces a data.frame
#' containing index and an incrementing id. Useful in cases where you have a
#' column describing a particular state e.g. an instrument zero as either 0 or 1
#' that occurs hourly.
#' In this case you may wish to group the zeros to take an average, but to group
#' **each zero separatly**, so they can subsequently be interpolated.
#'
#' @param tidyRle the output of `tidy_rle()`
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' # some imaginary data where the zero_valve column takes a 0 or 1 state.
#' valveIdx = mydata$zero_valve |>
#'   rle() |>
#'   tidy_rle() |>
#'   filter(values == 1) |> just get the times where the valve is acuated
#'   index_from_tidy_rle()
#'
#' # each valve actuation now has a unique id. where zero_value == 0, id == NA
#' mydata = mydata |>
#'   mutate(idx = row_number()) |>
#'   left_join(valveIdx, by = idx)
#'}
#'
#' @author W. S. Drysdale
#'
#' @export
#'

index_from_tidy_rle = function(tidyRle){

  tidyRle |>
    dplyr::mutate(id = dplyr::row_number()) |>
    purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5))

}
