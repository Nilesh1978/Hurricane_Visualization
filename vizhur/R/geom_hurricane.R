#' Create Geom to add the hurricane wind radii chart for a single storm observation to a map
#' This function builds a custom geom for ggplot2 that can be used to add the hurricane wind radii chart for a single storm observation to a map
#' @param data storm data specifying maximum wind speeds reached at specified wind intensities in knots.  These data should be in long format (one row for each wind speed intensity. The maximum wind speeds in each of the four quadrants (ne, se, nw, sw) should be in separate columns for each of the wind speed intensities.
#' @param x longitude of the location
#' @param y latitude of the location
#' @param r_ne max wind speed reached in the north east quadrant of the location
#' @param r_se max wind speed reached in the south east quadrant of the location
#' @param r_sw max wind speed reached in the south west quadrant of the location
#' @param r_nw max wind speed reached in the north west quadrant of the location
#' @param scale_radii wind radii give the maximum radial extent of winds of a certain direction in each quadrant. In some circumstances, you may want to plot a certain percentage of the full wind radii, to give a better idea of average exposures in each quadrant
#' @return plots circular polygons for each of the three wind speed intensities based on the maximum wind speeds and in each quadrants.  scale_radii input will scale the polygon based on the input (1 will be the default value)
#' @import ggplot2
#' @import ggmap
#' @importFrom grid polygonGrob
#' @importFrom dplyr select filter mutate arrange
#' @importFrom tidyr unite separate gather spread
#' @export
GeomHurricane <- ggplot2::ggproto("GeomHurricane", ggplot2::Geom,
required_aes = c("x", "y", "r_ne", "r_se", "r_sw", "r_nw"),
default_aes = ggtern::aes(colour = "NA", fill = "grey20", size = 0.5, linetype = 1, alpha = 0.8, scale_radii = 1),
draw_key = ggplot2::draw_key_polygon,
draw_group = function(data, panel_scales, coord){

  # get the lat & lon data for the specified date and storm
  point_obs = c(data[1,]$x, data[1,]$y)
  color <- data[1,]$colour
  fill <- data[1,]$fill
  alpha <- data[1,]$alpha
  scale_radii = data[1,]$scale_radii

  # get lat & lon points in the north east quadrant
  points_polygon = geosphere::destPoint(p = point_obs, b=1:90, d = data[1,]$r_ne * 1852 * scale_radii)
  data_ne <- data.frame(x = c(points_polygon[,"lon"], point_obs[1]),
                        y = c(points_polygon[,"lat"], point_obs[2])
  )

  # get lat & lon points in the south east quadrant
  points_polygon = geosphere::destPoint(p = point_obs, b=90:180, d = data[1,]$r_se * 1852 * scale_radii)
  data_se <- data.frame(x = c(points_polygon[,"lon"], point_obs[1]),
                        y = c(points_polygon[,"lat"], point_obs[2])
  )

  # get lat & lon points in the south west quadrant
    points_polygon = geosphere::destPoint(p = point_obs, b=180:270, d = data[1,]$r_sw * 1852 * scale_radii)
  data_sw <- data.frame(x = c(points_polygon[,"lon"], point_obs[1]),
                        y = c(points_polygon[,"lat"], point_obs[2])
  )
  # get lat & lon points in the north west quadrant
  points_polygon = geosphere::destPoint(p = point_obs, b=270:360, d = data[1,]$r_nw * 1852 * scale_radii)
  data_nw <- data.frame(x = c(points_polygon[,"lon"], point_obs[1]),
                        y = c(points_polygon[,"lat"], point_obs[2])
  )

  # bind the data for all four quadrants
  data_all <- rbind(data_ne, data_se, data_nw, data_sw)

  # transform
  coords <- coord$transform(data_all, panel_scales)

  # use polygonGrob for plotting
  grid::polygonGrob(x = coords$x,
                    y = coords$y,
                    gp = grid::gpar(col = color, fill = fill, alpha = alpha))
}
)

#' Add new geom to the ggmap to plot hurricane wind radii chart for a single storm observation to a map.
#' This function builds a custom geom for ggplot2 that can be used to add the hurricane wind radii chart for a single storm observation to a map
#' @param data storm data specifying maximum wind speeds reached at specified wind intensities in knots.  These data should be in long format (one row for each wind speed intensity. The maximum wind speeds
#' @param x longitude of the location
#' @param y latitude of the location
#' @param r_ne max wind speed reached in the north east quadrant of the location
#' @param r_se max wind speed reached in the south east quadrant of the location
#' @param r_sw max wind speed reached in the south west quadrant of the location
#' @param r_nw max wind speed reached in the north west quadrant of the location
#' @param scale_radii wind radii give the maximum radial extent of winds of a certain direction in each quadrant. In some circumstances, you may want to plot a certain percentage of the full wind radii, to give a better idea of average exposures in each quadrant
#' @return plots circular polygons for each of the three wind speed intensities based on the maximum wind speeds and in each quadrants.  scale_radii input will scale the polygon based on the input (1 will be the default value)
#' @import ggplot2
#' @importFrom grid polygonGrob
#' @importFrom dplyr select filter mutate arrange
#' @importFrom tidyr unite separate gather spread in each of the four quadrants (ne, se, nw, sw) should be in separate columns for each of the wind speed intensities.
#' @return plots circular polygons for each of the three wind speed intensities based on the maximum wind speeds and in each quadrants.  scale_radii input will scale the polygon based on the input (1 will be the default value)
#' @import ggplot2
#' @import ggmap
#' @inheritParams ggplot2::geom_polygon
#' @examples
#'map_plot <- ggmap::get_map("Lousiana", zoom = 6, maptype = "toner-background")

#'map_plot %>%
#'  ggmap(extent = "device") +
#'  geom_hurricane(data = storm_observation,
#'                 aes(x = longitude, y = latitude,
#'                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#'                     color = wind_speed, fill = wind_speed, scale_radii=0.8)) +
#'  scale_color_manual(name = "Wind speed (kts)",
#'                     values = c("red", "orange", "yellow")) +
#'  scale_fill_manual(name = "Wind speed (kts)",
#'                    values = c("red", "orange", "yellow"))
#' @export
#' @export
geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
position = "identity", na.rm = FALSE,
show.legend = NA, inherit.aes = TRUE, ...){
  layer(
    geom = GeomHurricane, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,...)
  )
}
