#' eq_create_label
#' Cleans data
#'
#' @param data A dataset
#' @param annot_col selected column for showing in pop-up
#' @return show earthquake points on maps
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircles
#' @references https://rstudio.github.io/leaflet/markers.html
#' @examples
#'\dontrun{
#' readr::read_delim(data,
#'                   delim = "\t") %>%
#'        eq_clean_data() %>%
#'     dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'     eq_map(annot_col = "DATE")
#'                       }
#'@export
eq_map <- function(data, annot_col){
  m <- leaflet::leaflet(data) %>%
    leaflet::addTiles() %>%
    leaflet::addCircles(~LONGITUDE, ~LATITUDE,
               popup= ~data[[annot_col]],
               radius =  ~EQ_PRIMARY*10000,
               #color = ~pal(type),
               fillOpacity = 0.2,
               weight =1,
               stroke = 0.1)
  return(m)
}


