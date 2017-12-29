# load packages
library(tidyr)
library(dplyr)
library('lubridate')
library(ggmap)
library(geosphere)
library(readr)
source("./R source code/GeomHurricane.R")

# pre-format data columns
ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

# read data
ext_tracks <- read_fwf("./data/ebtrk_atlc_1988_2015.txt", 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

# reorganize data for use
dat<- ext_tracks %>%
  dplyr::select(-storm_id) %>%
  dplyr::mutate(storm_id = paste(storm_name, year, sep = "_")) %>%
  tidyr::unite(date, year, month, day, hour) %>%
  dplyr::select(storm_id, date, everything()) %>%
  dplyr::select(-storm_name, -final, -distance_to_land, -pressure_1, -pressure_2, -eye_diameter, -rad_max_wind,-max_wind,-min_pressure,-storm_type) %>%
  dplyr::mutate(date = lubridate::ymd_h(date)) %>%
  tidyr::gather(key = key, value = value,-storm_id, -date,-latitude, -longitude) %>%
  tidyr::separate(key, c("radius","wind_speed","direction")) %>%
  dplyr::select(-radius) %>%
  tidyr::spread(key = direction, value = value) %>%
  dplyr::mutate(longitude = as.numeric(paste0("-",longitude))) %>%
  dplyr::arrange(date)
#str(dat)
#head(dat)

# apply geom_hurricane on ike_2008 data set
storm_observation <- dat[dat$storm_id == "IKE_2008" & 
                           dat$date == ymd_hms("2008-09-12 18:00:00"),]

storm_observation$wind_speed <- as.factor(storm_observation$wind_speed)
str(storm_observation)

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

map_plot <- get_map("Lousiana", zoom = 6, maptype = "toner-background") 

map_plot %>%
  ggmap(extent = "device") +
  geom_hurricane(data = storm_observation,
                 aes(x = longitude, y = latitude, 
                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     color = wind_speed, fill = wind_speed, scale_radii=0.8)) + 
  scale_color_manual(name = "Wind speed (kts)", 
                     values = c("red", "orange", "yellow")) + 
  scale_fill_manual(name = "Wind speed (kts)", 
                    values = c("red", "orange", "yellow")) +
  ggtitle("Ike_2008")
#ggplot2::ggsave(path = "./plots", filename = "ike_2008.pdf")



