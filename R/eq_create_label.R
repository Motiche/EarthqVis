#' eq_create_label
#' make label for popup on the map
#'
#' @param data A dataset
#' @return a list of labels consisting of Location, Number of Deaths and Magnitude for eache earthquake
#' @importFrom stringr str_glue
#' @examples
#'\dontrun{
#' readr::read_delim(data,
#'                   delim = "\t") %>%
#'        eq_location_clean()%>%
#'        dplyr::mutate(popup_text = eq_create_label(.))
#'                       }
#'@export
eq_create_label <- function(data){
  Loc <- ifelse(nchar(data$LOCATION_NAME)>0,
                stringr::str_glue('<b>Location: </b>{data$LOCATION_NAME}<br/>'),
                "")
  Death <- ifelse(!is.na(data$TOTAL_DEATHS),
                  stringr::str_glue('<b>Total deaths: </b>{as.numeric(data$TOTAL_DEATHS)}<br/>'),
                  "")
  Mag <- ifelse(!is.na(data$EQ_PRIMARY),
                  stringr::str_glue('<b>Magnitude: </b>{data$EQ_PRIMARY}'),
                  "")
  return(paste0(Loc,Death,Mag))

}

