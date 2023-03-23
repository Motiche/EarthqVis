#' ggproto_timeline_label
#' build ggproto object for geom_timeline_label
#'
#' @importFrom ggplot2 ggproto aes draw_key_point Geom
#' @import grid
#'
#' @export
ggproto_timeline_label <- ggplot2::ggproto("ggproto_timeline_label", ggplot2::Geom,
                                  required_aes = c('x',
                                                   'y',
                                                   'label',
                                                   'size'),
                                  #segmentsGrob grid.text gpar gtree gList
                                  default_aes = ggplot2::aes(
                                    size = 5,
                                    colour = "blue"),
                                  optional_aes = ('n_max'),
                                  draw_key = ggplot2::draw_key_point,
                                  draw_panel = function(data, panel_params, coord)  {
                                    ## Transform the data first
                                    data = data %>%
                                      dplyr::arrange(dplyr::desc(.data$size)) %>%
                                      head(data$n_max[1])

                                    coords <- coord$transform(data, panel_params)


                                    lines <- grid::segmentsGrob(x0 = coords$x,
                                                                y0 = coords$y,
                                                                x1 = coords$x,
                                                                y1 = coords$y + 0.09,
                                                                gp = grid::gpar(col ="grey",
                                                                                alpha = 0.5,
                                                                                fontsize = 1,
                                                                                lwd =     1))

                                    ## Construct a grid grob
                                    texts <- grid::grid.text(
                                      coords$label,
                                      x = coords$x,
                                      y = coords$y + 0.1,
                                      just = 'left',
                                      rot = 50,
                                      check.overlap = TRUE,
                                      gp = grid::gpar(col="grey", fontsize=8)
                                    )


                                    gTree(children = grid::gList(lines, texts))
                                  }
)


#' geom_timeline_label
#' make labels for a timeline of earthquakes for countries
#'
#' @importFrom ggplot2 layer
#' @examples
#'\dontrun{
#' ex_data <- readr::read_delim(raw_data,
#'                   delim = "\t") %>%
#'                       eq_location_clean() %>%
#'                       filter(YEAR>1500) %>%
#'filter(COUNTRY %in% c("IRAN","CHINA","USA","UK"))
#'ggplot2::ggplot(data = ex_data, aes(x = ex_data$DATE ,
#'                           y = ex_data$COUNTRY,
#'                           size = ex_data$EQ_PRIMARY,
#'                           alpha = ex_data$DEATHS)) +
#'  geom_timeline_label(n_max = 20)+
#'  geom_timeline()+
#'  theme_light()+
#'  labs(size = "Strength (Richter)",
#'       alpha = "Number of deaths")+
#'  xlab("Time") + ylab("")
#'}
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = F, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = ggproto_timeline_label, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = F, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...))

}
