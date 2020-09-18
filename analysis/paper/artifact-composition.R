suppressPackageStartupMessages(library(tidyverse))
library(glue)
library(ggrepel)
library(here)


# data from PhD data sheet, not KAS sheet.
mydata <- read.csv(here("analysis/data/raw_data/General_info.csv"))


# assemblage composition data from KAS data sheet.
kasa <- read.csv(here("analysis/data/raw_data/Assemblage_info.csv"))


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
  # if percentage is <10%, call it 'other'
  mutate(artefact_type = ifelse(as.character(X) == 'stemmed_point',
                                'stemmed_point',
                                ifelse(percentage >= 10 | X == "unkown",
                                       as.character(X), "other_tools"))) %>%
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
  mutate(axis_label = glue('{site.y} ({round(age_ka,1)} ka)'))

ggplot(kasa_long2,
       aes(
         reorder(axis_label, -age_ka),
         percentage,
         fill = artefact_type)) +
  geom_col(position = "fill") +
  ylab("Percentage") +
  xlab("Assemblage (youngest at the top)") +
  theme_minimal(base_size = 16)  +
  scale_fill_viridis_d(name = "Artefact type",
                       option = "D") +
  coord_flip() +
  theme_minimal(base_size = 12)


ggsave(here::here("analysis/figures/004-artefact-types.png"))

