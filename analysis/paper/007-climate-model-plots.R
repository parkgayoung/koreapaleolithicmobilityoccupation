
library(ncdf4)
library(lattice)
library(ggplot2)
library(cowplot)

# you'll need to get this file from
# https://figshare.com/articles/LateQuaternary_Environment_nc/12293345/3
# and move to our raw_data folder
file <- here::here("analysis/data/raw_data/LateQuaternary_Environment.nc")

env_nc      <- ncdf4::nc_open(file)
longitude   <- ncdf4::ncvar_get(env_nc, "longitude")
latitude    <- ncdf4::ncvar_get(env_nc, "latitude")
years       <- ncdf4::ncvar_get(env_nc, "time")
months      <- ncdf4::ncvar_get(env_nc, "month")
temperature <- ncdf4::ncvar_get(env_nc, "temperature")
biome       <- ncdf4::ncvar_get(env_nc, "biome")
ncdf4::nc_close(env_nc)

# this takes a few minutes
mean_annual_temperature <- apply(temperature, c(1, 2, 4), mean)

# korean archaeological sites
library(tidyverse)

site_locations_tbl_raw <- read_csv (here::here("analysis/data/raw_data/korean_palaeolithic_site_locations.csv"))
site_locations_tbl <- site_locations_tbl_raw

# some problem finding a MAT value for the location
# Gigok

site_locations_tbl$lat_dd <-
  with(site_locations_tbl,
  ifelse(site_name == "Gigok",
         round(lat_dd, 0),
         lat_dd)
  )

site_locations_tbl$long_dd <-
  with(site_locations_tbl,
       ifelse(site_name == "Gigok",
              round(long_dd, 0),
              long_dd)
  )


site_locations_tbl_temps <-
site_locations_tbl %>%
  dplyr::select(site_name,
         lat_dd,
         long_dd,
         elevation) %>%
  rowwise()  %>%
  mutate(lonID = which.min(abs(longitude - long_dd)),
         latID = which.min(abs(latitude - lat_dd))) %>%
  mutate(mean_annual_temperature = list(tibble(year = years,
                                               mean_annual_temperature = mean_annual_temperature[lonID, latID, ]))) %>%
  unnest(mean_annual_temperature) %>%
  filter(between(year, -50000, -10000))

#----------------------------------------------------------------------
base_size = 6

# this is the overall time series plot
# we see the mean for all locations, and individual
# locations in grey in the background
mat_time_series_plot_annotated <-
ggplot() +
  # add annotations for climate events
  annotate ("rect",
            xmin = 14000, xmax = 29000,
            ymin = -Inf, ymax = Inf, fill = "lightgrey",
            alpha = .4) +
  annotate("rect",
           xmin = 19000, xmax = 26500,
           ymin = -Inf, ymax = Inf, fill = "grey",
           alpha = .4)   +
  # add lines for each location
  geom_line(data = site_locations_tbl_temps,
            aes(-year,
                mean_annual_temperature,
                group = site_name),
            colour = "grey90") +
  # add mean line for all locations
  geom_line(data = site_locations_tbl_temps %>%
              group_by(year) %>%
              summarise(mean_mean_annual_temperature = mean(mean_annual_temperature,
                                                            na.rm = TRUE)),
            aes(-year,
                mean_mean_annual_temperature),
            size = 1) +
  annotate("text",
           x = 11100,
           y = 11,
           label = "MIS\n1",
           size = 3) +
  annotate("text",
           x = 22000,
           y = 11,
           label = "MIS\n2",
           size = 3) +
  annotate("text",
           x = 38000,
           y = 11,
           label = "MIS\n3",
           size = 3) +
  annotate("text",
           x = 22500,
           y = 9.7,
           label = "LGM",
           size = 2) +
  scale_x_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(1, 12)) +
  theme_minimal(base_size = base_size ) +
  labs(y = "Mean annual temperature (MAT, °C)",
       x = "Year (BP)")

# now we prepare the data to show each site
# in a boxplot

korean_archaeological_sites <-
  readxl::read_excel(here::here("analysis/data/raw_data/korean-archaeologica-sites.xlsx"))

# get site age and location in neat and tidy format
site_locations_tbl_temps_periods <-
site_locations_tbl_temps %>%
  left_join(korean_archaeological_sites) %>%
  mutate(years_ka = str_sub(`C14(BP)`, 1, 5)) %>%
  mutate(years_ka = -as.numeric(str_replace(years_ka, ",", ".")) * 1000)

# filter climate data so we only get the temps during the time the site was occupied
site_locations_tbl_temps_periods_filtered <-
site_locations_tbl_temps_periods %>%
  # we filter 2.5 ka years either side of the radiocarbon age
  filter(between(year,
                 (years_ka - 2500),
                 (years_ka + 2500)))
#----------------------------------------------------------------------

# here is the plot that shows the temp range during the period of occupation
mat_per_site_plot <-
ggplot(site_locations_tbl_temps_periods_filtered) +
    aes(y = mean_annual_temperature,
        x = reorder(site_name,
                    mean_annual_temperature)) +
    geom_boxplot(lwd = 0.1) +
    coord_flip() +
  xlab("") +
  ylab("Mean annual temperature (MAT, °C)") +
  theme_minimal(base_size = base_size)

#----------------------------------------------------------------------

# can we have a map also?

library(ggmap)

# bounding box
# 39.534214, 124.159154 ... 39.657415, 129.138604
# 33.694662, 123.491142 ... 34.338454, 130.569658

# download background tiles for the map
map <-
  get_stamenmap(bbox = c(left = 125.5,
                         bottom = 34,
                         right = 	130,
                         top = 38.5),
                zoom = 9)

library(ggrepel)
site_locations_tbl_temps_periods_filtered_means <-
site_locations_tbl_temps_periods_filtered %>%
  group_by(site_name) %>%
  summarise(av_mat = mean(mean_annual_temperature),
            long_dd = mean(long_dd),
            lat_dd = mean(lat_dd)
            )
#----------------------------------------------------------------------

# map showing temp differences

mat_site_map_plot <-
ggmap(map)  +
  geom_point(data = site_locations_tbl_temps_periods_filtered_means %>%
               dplyr::select(-long_dd,
                      -lat_dd) %>%
               left_join(site_locations_tbl_raw) ,
             aes(long_dd ,
                 lat_dd,
             colour = av_mat),
             size = 2) +
  geom_text_repel(data = site_locations_tbl_temps_periods_filtered_means %>%
                    dplyr::select(-long_dd,
                           -lat_dd) %>%
                    left_join(site_locations_tbl_raw),
                   aes(long_dd ,
                       lat_dd,
                       label = site_name),
                  size = 1.5,
                  bg.color = "white",
                  bg.r = 0.1) +
  scale_colour_viridis_c(name = "MAT") +
  theme_bw(base_size = base_size) +
  theme(legend.position = c(0.9, 0.25),
        axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              plot.margin = unit(c(0, 0, -0.5, -0.5), 'lines'),
        legend.key.size = unit(0.3, "cm")) +
          xlab('') +
          ylab('')

#----------------------------------------------------------------------

library(ggpubr)


mat_elev_cor_plot <-
  site_locations_tbl_temps %>%
    group_by(site_name) %>%
    drop_na() %>%
    summarise(mat = mean(mean_annual_temperature),
              elev = mean(elevation)) %>%
  ggplot() +
    aes(y = elev,
        x = mat) +
    stat_smooth(method = "lm") +
    geom_point() +
    geom_text_repel(aes(label = site_name),
                    size = 2,
                    bg.color = "white",
                    bg.r = 0.1) +
    stat_cor(label.x = 6,
             label.y = 300,
             size = 2)  +
   ylab("Elevation above sea level (m)") +
   xlab("Mean annual temperature (MAT, °C)") +
    theme_minimal(base_size = base_size)

#----------------------------------------------------------------------

# combine plots

top_row <-
  plot_grid(mat_site_map_plot,
            mat_per_site_plot,
            nrow = 1,
            labels = c('A', 'B'),
            label_size = 6,
            rel_widths = c(0.7, 1))

bottom_row <-
  plot_grid(mat_time_series_plot_annotated,
            mat_elev_cor_plot,
            labels = c('C', 'D'),
            label_size = 6,
            nrow = 1)

# combine rows
plot_grid(top_row,
          bottom_row,
          ncol = 1)

ggsave(here::here("analysis/figures/007-climate-model-sites-panel-plot.png"),
       scale = 1,
       width = 4.45,
       height = 5,
       units = "in")











