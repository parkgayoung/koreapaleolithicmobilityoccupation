
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

my_year      <- -10000;   # 10,0000 BP
my_month     <- 6;        # June
my_longitude <- 0.1218;
my_latitude  <- 52.2053;  # Cambridge (UK)

my_longitude1 <- 127.108155
my_latitude1 <-  35.823923  # Bonggok


p1 <-
  print(lattice::levelplot(biome[, , years == my_year], main = "Biome distribution, 10000 BP"))

p2 <-
  print(lattice::levelplot(temperature[, , months == my_month, years == my_year], main = "Mean June temperature, 10000 BP"))

lonID <- which.min(abs(longitude - my_longitude))
# check
longitude[lonID]

latID <- which.min(abs(latitude - my_latitude))
# check
latitude[latID]

yearID <- which.min(abs(years - my_year))


p3 <-
  ggplot2::qplot(
    months,
    temperature[lonID, latID, , yearID],
    xlab = "Month",
    ylab = "Mean temperature",
    geom = c("point", "line")
  )

mean_annual_temperature <- apply(temperature, c(1, 2, 4), mean)

p4 <- ggplot2::qplot(
  years,
  mean_annual_temperature[lonID, latID, ],
  xlab = "Year",
  ylab = "Temperature",
  main = "Mean annual temperature time series",
  geom = c("point", "line")
)

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)


# korean archaeological sites
library(tidyverse)

site_locations_tbl <- read_csv(here::here("analysis/data/raw_data/korean_palaeolithic_site_locations.csv"))

# site_locations_tbl_temps <-

site_locations_tbl_temps <-
site_locations_tbl %>%
  select(site_name,
         lat_dd,
         long_dd,
         elevation) %>%
  rowwise()  %>%
  mutate(lonID = which.min(abs(longitude - long_dd)),
         latID = which.min(abs(latitude - lat_dd))) %>%
  mutate(mean_annual_temperature = list(tibble(year = years,
                                               mean_annual_temperature = mean_annual_temperature[lonID, latID, ]))) %>%
  unnest(mean_annual_temperature)



  ggplot(site_locations_tbl_temps) +
  aes(year,
      mean_annual_temperature,
      colour = site_name) +
  geom_line() +
  xlim(-50000,
       -10000) +
  theme_minimal()

  ggplot(site_locations_tbl_temps) +
    aes(y = mean_annual_temperature,
        x = reorder(site_name,
                    mean_annual_temperature)) +
    geom_boxplot() +
    coord_flip() +
    theme_minimal()

library(ggpubr)

  site_locations_tbl_temps %>%
    group_by(site_name) %>%
    drop_na() %>%
    summarise(mat = mean(mean_annual_temperature),
              elev = mean(elevation)) %>%
  ggplot() +
    aes(y = elev,
        x = mat) +
    geom_text(aes(label = site_name)) +
    stat_smooth(method = "lm") +
    stat_cor(label.x = 9,
             label.y = 300) +
    stat_regline_equation(label.x = 9,
                          label.y = 270) +
    theme_minimal()










