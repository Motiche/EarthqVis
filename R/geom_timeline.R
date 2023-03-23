#' ggproto_timeline
#' build ggproto object for geom_timeline
#' @import ggplot2
#' @import grid
#' @importFrom ggplot2 ggproto aes draw_key_point Geom
#' @importFrom grid circleGrob gpar
#' @export
ggproto_timeline <- ggplot2::ggproto("ggproto_timeline", ggplot2::Geom,
                         required_aes = c('x'),
                         default_aes = ggplot2::aes(shape = 1,
                                           alpha = 0.1,
                                           size = 5,
                                           colour = "blue"),
                         optional_aes = ('y'),
                         draw_key = ggplot2::draw_key_point,
                         draw_panel = function(data, panel_params, coord)  {
                           ## Transform the data
                           coords <- coord$transform(data, panel_params)
                           coords$size = coords$size/100
                           ## Construct a grid grob
                           grid::circleGrob(
                             x = coords$x,
                             y = coords$y,
                             r = coords$size,
                             gp = grid::gpar(alpha = coords$alpha,
                                             fill = coords$colour,
                                             col = "blue",
                                             lwd = 1)
                           )

                         }
                         )
#' geom_timeline
#' make a timeline of earthquakes for countries
#'
#' @importFrom ggplot2 layer
#' @examples
#'\dontrun{
#' ex_data <- readr::read_delim(raw_data,
#'                   delim = "\t") %>%
#'                       eq_clean_data() %>%
#'                       filter(YEAR>2010) %>%
#'filter(COUNTRY %in% c("IRAN","CHINA"))
#'ggplot2::ggplot(data = ex_data, aes(x = ex_data$DATE ,
#'                           y = ex_data$COUNTRY,
#'                           size = ex_data$EQ_PRIMARY,
#'                           alpha = ex_data$DEATHS)) +
#'  geom_timeline()+
#'  theme_light()+
#'  labs(size = "Strength (Richter)",
#'       alpha = "Number of deaths")+
#'  xlab("Time") + ylab("")
#'}
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE,
                         show.legend = T, inherit.aes = TRUE, ...) {
    ggplot2::layer(
    geom = ggproto_timeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = T, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...))

}



