#' Read NASA Ames
#'
#' Reads a NASA Ames file by removing the header and parsing the rest of the data using read.table()
#'
#' @param file path to the file
#' @param NComm_header Logical default false. If the final line of normal comments contains the data header, set to true
#' @param header_length by default the header length is obtained from the first line of the file. Supply a value here to override this.
#' @param ... additional arguments passed to read.table
#'
#' @author W. S. Drysdale
#'
#' @export

read_nasa_ames = function(file,
                          NComm_header = FALSE,
                          header_length = NULL,
                          ...){

  if(is.null(header_length)){
    # Get header length from file
    header_length = readLines(file,n = 1)  |>
      stringr::word(1, sep = "\\s+") |>
      as.numeric()
  }

  # If the final line of the special comments contains the column headers, use these
  if(NComm_header){
    dat = read.table(file, skip = (header_length-1), header = T, ...)
  }else{
    dat = read.table(file, skip = header_length, ...)
  }

  #Return
  dplyr::tibble(dat)
}
