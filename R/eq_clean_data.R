utils::globalVariables(c("DEATHS","YEAR", "MONTH", "DAY", "EQ_PRIMARY", 
                         "LATITUDE", "LONGITUDE"))

#' eq_clean_data
#' Curate data
#'
#' @param data A dataset
#' @return curated dataset
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom tidyr replace_na
#' @importFrom lubridate ymd
#' @examples
#'\dontrun{
#' readr::read_delim(raw_data,
#'                   delim = "\t") %>%
#'                       eq_location_clean()}
#' @export
eq_clean_data <- function(data){
  data <- data %>%
    mutate(DEATHS = as.numeric(DEATHS)) %>%
    mutate(EQ_PRIMARY = as.numeric(EQ_PRIMARY)) %>%
    mutate(DEATHS = tidyr::replace_na(DEATHS,0)) %>%
    mutate(YEAR = tidyr::replace_na(YEAR,1)) %>%
    mutate(BC_YEAR = ifelse(YEAR<0, YEAR,0)) %>%
    mutate(YEAR = ifelse(YEAR<0, "",YEAR)) %>%
    mutate(YEAR = ifelse(nchar(YEAR)<2, paste0("000",YEAR),YEAR)) %>%
    mutate(YEAR = ifelse(nchar(YEAR)<3, paste0("00",YEAR),YEAR)) %>%
    mutate(YEAR = ifelse(nchar(YEAR)<4, paste0("0",YEAR),YEAR)) %>%
    mutate(MONTH = tidyr::replace_na(MONTH,1)) %>%
    mutate(DAY = tidyr::replace_na(DAY,1)) %>%
    mutate(DATE = lubridate::ymd(paste(YEAR,MONTH,DAY, sep = "/"))+ lubridate::years(BC_YEAR)) %>%
    mutate(DATE = as.Date(DATE)) %>%
    mutate(YEAR = as.numeric(YEAR) + as.numeric(BC_YEAR)) %>%
    mutate(LATITUDE = as.numeric(LATITUDE)) %>%
    mutate(LONGITUDE = as.numeric(LONGITUDE)) %>%
    eq_location_clean(.)
  return(data)

 }
