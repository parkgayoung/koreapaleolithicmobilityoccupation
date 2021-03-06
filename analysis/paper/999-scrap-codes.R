#----------------------------------------------------------------
# older map data

korean_archaeological_sites <-
  readxl::read_excel(here("analysis/data/raw_data/korean-archaeologica-sites.xlsx"))

coords <-
  korean_archaeological_sites %>%
  select(site_name,
         east_longitude,
         north_latitude) %>%
  separate(east_longitude,
           into = c("e_deg", "e_min", "e_sec"),
           sep =  "°|\\′") %>%
  mutate(e_min = str_squish(e_min),
         e_sec = str_squish(e_sec)) %>%
  separate(e_min,
           into = c("e_min", "e_sec1"),
           sep = "'") %>%
  mutate_at(vars(e_deg, e_min, e_sec, e_sec1), parse_number) %>%
  mutate(e_sec = ifelse(is.na(e_sec), e_sec1, e_sec)) %>%
  select(-e_sec1) %>%
  separate(north_latitude,
           into = c("n_deg", "n_min", "n_sec"),
           sep =  "°|\\′") %>%
  mutate(n_min = str_squish(n_min),
         n_sec = str_squish(n_sec)) %>%
  separate(n_min,
           into = c("n_min", "n_sec1"),
           sep = "'") %>%
  mutate_at(vars(n_deg, n_min, n_sec, n_sec1), parse_number) %>%
  mutate_if(is.numeric, list(~ifelse(is.na(.), 0, .))) %>%
  mutate(n_sec = ifelse(is.na(n_sec), n_sec1, n_sec)) %>%
  select(-n_sec1) %>%
  mutate(lat_dms =  str_glue('{n_deg} {n_min} {n_sec}'),
         long_dms = str_glue('{e_deg} {e_min} {e_sec}')) %>%
  # convert from decimal minutes to decimal degrees
  mutate(lat_dd = measurements::conv_unit(lat_dms,
                                          from = 'deg_min_sec',
                                          to = 'dec_deg')) %>%
  mutate(long_dd = measurements::conv_unit(long_dms,
                                           from = 'deg_min_sec',
                                           to = 'dec_deg')) %>%
  mutate_at(vars(lat_dd, long_dd), as.numeric)

# write_csv(coords, here::here("analysis/data/raw_data/korean_palaeolithic_site_locations.csv"))

# quick map

library(ggmap)

# bounding box
# 39.534214, 124.159154 ... 39.657415, 129.138604
# 33.694662, 123.491142 ... 34.338454, 130.569658

map <-
  get_stamenmap(bbox = c(left = 123,
                         bottom = 33,
                         right = 	131,
                         top = 39),
                zoom = 10)

library(ggrepel)
ggmap(map)  + geom_point(data = coords,
                         aes(long_dd ,
                             lat_dd),
                         colour = "red",
                         size = 2) +
  geom_label_repel(data = coords,
                   aes(long_dd ,
                       lat_dd,
                       label = site_name),
                   size = 2)

