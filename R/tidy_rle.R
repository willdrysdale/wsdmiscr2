#' tidy rle
#'
#' Take the output of \code{rle()}, convert to a tibble and calculate start and end indices.
#'
#' @param rle_obj output of \code{rle()}
#'
#' @author W. S. Drysdale
#'
#' @export

tidy_rle = function(rle_obj) {
  rle_df = dplyr::tibble(lengths = rle_obj$lengths,
                         values = rle_obj$values)  |>
    dplyr::mutate(idx_start = cumsum(c(1, lengths))[1:(nrow(.))],
                  idx_end = cumsum(lengths))

  #
  rle_df
}
