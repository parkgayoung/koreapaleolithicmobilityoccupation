
require("ncdf4")
require("lattice")
require("ggplot2")

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

mean_annual_temperature <- apply(temperature, c(1, 2, 4), mean)

# korean archaeological sites
library(tidyverse)

gigok_others <-
  readxl::read_excel(here("analysis/data/raw_data/gigok_trial.xlsx"))

site_locations_tbl_temps <-
  gigok_others %>%
  select(site_name,
         lat_dd,
         long_dd) %>%
  rowwise()  %>%
  mutate(lonID = which.min(abs(longitude - long_dd)),
         latID = which.min(abs(latitude - lat_dd))) %>%
  mutate(mean_annual_temperature = list(tibble(year = years,
                                               mean_annual_temperature = mean_annual_temperature[lonID, latID, ]))) %>%
  unnest(mean_annual_temperature) %>%
  filter(between(year, -50000, -10000))


mat_time_series_plot <-
  ggplot() +
  geom_line(data = site_locations_tbl_temps,
            aes(-year,
                mean_annual_temperature,
                group = site_name),
            colour = "grey90") +
  geom_line(data = site_locations_tbl_temps %>%
              group_by(year) %>%
              summarise(mean_mean_annual_temperature = mean(mean_annual_temperature,
                                                            na.rm = TRUE)),
            aes(-year,
                mean_mean_annual_temperature),
            size = 2) +
  scale_x_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(1, 12)) +
  theme_minimal() +
  labs(y = "Mean annual temperature (MAT, °C)",
       x = "Year (BP)")
