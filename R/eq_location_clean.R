#' eq_location_clean
#' Cleans data
#'
#' @param data A dataset
#' @return cleaned dataset
#' @importFrom stringr str_to_title
#' @examples
#'\dontrun{
#' readr::read_delim(data,
#'                   delim = "\t") %>%
#'                       eq_location_clean()}
#'@export
eq_location_clean <- function(data){
  data$LOCATION_NAME <- gsub(".*:  ","",data$LOCATION_NAME)
  data$LOCATION_NAME <- str_to_title(data$LOCATION_NAME)
  return(data)
}
