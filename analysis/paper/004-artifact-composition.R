suppressPackageStartupMessages(library(tidyverse))
library(glue)
library(ggrepel)
library(here)

# data from PhD data sheet, not KAS sheet.
mydata <- read.csv(here("analysis/data/raw_data/General_info.csv"))

# assemblage composition data from KAS data sheet.
kasa <- read.csv(here("analysis/data/raw_data/Assemblage_info.csv"))

mydata_ages <-
  mydata %>%
  separate(C14.BP., into = c('age', 'error'),
           sep = "±") %>%
  mutate(age_ka = parse_number(age) / 1000,
         error = parse_number(error)) %>%
  mutate(has_sp = ifelse(!is.na(SP.), "yes", "no"))

# join artefact type freqs with site data

kasa %>%
  pivot_longer(-X,
               names_to = "site_name",
               values_to = "count") %>%
  pivot_wider(names_from = "X",
              values_from = "count") %>%
  left_join(mydata)

kasa_long1 <-
  kasa %>%
  gather(site,
         count,
         -X) %>%
  filter(!is.na(count)) %>%
  group_by(site) %>%
  mutate(percentage = count / sum(count, na.rm = TRUE) * 100,
         total = sum(count, na.rm = TRUE)) %>%
  filter(!is.na(percentage)) %>%
  filter(!X %in% c('unknown',
                   'unkown',
                   'unfinished',
                   'plane',
                   'point',
                   'chopper',
                  'end_scraper',
                   'pebble',
                   'hammer',
                  'flake',
                   'debris',
                   'beak_shaped')
                   ) %>%
  # if percentage is <10%, call it 'other'
   mutate(artefact_type =   X #ifelse(as.character(X) == 'stemmed_point',
  #                              'stemmed_point',
  #                              ifelse(percentage >= 10 | X == "unkown",
  #                                     as.character(X), "other_tools"))
) %>%
  mutate(axis_label = glue('{site}\n(n = {total})'))  %>%
  select(-X)

# join to get ages of the sites
kasa_long2 <-
  kasa_long1 %>%
  group_by(site,
           artefact_type) %>%
  summarise(percentage = sum(percentage)) %>%
  left_join(mydata_ages,
            by = c('site' = 'site_name')) %>%
  arrange(age_ka) %>%
  distinct(.keep_all = TRUE) %>%
  filter(!is.na(age_ka)) %>%
  mutate(axis_label = glue('{site_new_name} ({round(age_ka,1)} ka)'))

types_in_many_sites <-
kasa_long2 %>%
  group_by(site_new_name,
           artefact_type) %>%
  tally() %>%
  ungroup() %>%
  group_by(artefact_type) %>%
  tally() %>%
  filter(n >= 5)

# filter out rare types
kasa_long3 <-
kasa_long2 %>%
  filter(artefact_type %in% types_in_many_sites$artefact_type)

kasa_long3_fill_plot <-
ggplot(kasa_long3,
       aes(
         reorder(axis_label, -age_ka),
         percentage,
         fill = artefact_type)) +
  geom_col(position = "fill") +
  ylab("Proportion") +
  xlab("Assemblage (youngest at the top)") +
  theme_minimal(base_size = 16)  +
  scale_fill_viridis_d(name = "Artefact type",
                       option = "D") +
  coord_flip() +
  theme_minimal(base_size = 12)

ggsave(here::here("analysis/figures/004-artefact-types.png"))

# interactive
plotly::ggplotly(kasa_long3_fill_plot)



ggplot(kasa_long3,
       aes(
         reorder(axis_label, -age_ka),
         percentage)) +
  geom_col() +
  ylab("Proportion") +
  xlab("Assemblage (youngest at the top)") +
  theme_minimal(base_size = 16)  +
  scale_fill_viridis_d(name = "Artefact type",
                       option = "D") +
  facet_wrap( ~ artefact_type,
              ncol = 1,
              scales = "free_y") +
  theme_minimal(base_size = 6)




