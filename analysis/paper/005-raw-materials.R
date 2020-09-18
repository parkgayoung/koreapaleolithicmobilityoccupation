suppressPackageStartupMessages(library(tidyverse))
library(glue)
library(ggrepel)
library(here)


# data from PhD data sheet, not KAS sheet.
mydata <- read.csv(here("analysis/data/raw_data/General_info.csv"))

# raw material data from KAS data sheet.
kasr <- read.csv(here("analysis/data/raw_data/Rawmaterial_info.csv"))

# join artefact type freqs with site data



 #raw material information
  kasr_long1 <-
  kasr %>%
  gather(site,
         count,
         -X) %>%
  group_by(site) %>%
  mutate(percentage = count / sum(count, na.rm = TRUE) * 100,
         total = sum(count, na.rm = TRUE)) %>%
  filter(!is.na(percentage)) %>%
  # if percentage is <10%, call it 'other'
  mutate(raw_material = ifelse(percentage >= 10, as.character(X), "other")) %>%
  mutate(raw_material = ifelse(raw_material == "etc.", "other", raw_material)) %>%
  select(-X)


# if percentage is <10%, call it 'other'
kasr_long2 <-
  kasr_long1 %>%
  group_by(site, raw_material) %>%
  summarise(percentage = sum(percentage)) %>%
  ungroup() %>%
  left_join(kasr_long1 %>%
              select(site, raw_material, total),
            by = c('site', "raw_material")) %>%
  distinct(.keep_all = TRUE) %>%
  # join to get ages of the sites
  left_join(mydata_ages,
            by = c('site' = 'site_name')) %>%
  # mutate(age_ka = jitter(age_ka),
  #        age_bin  = ntile(age_ka, 5)) %>%
  arrange(age_ka) %>%
  filter(!is.na(age_ka)) %>%
  mutate(axis_label = glue('{site.y} ({round(age_ka,1)} ka, n = {total})'))


ggplot(kasr_long2,
       aes(
         reorder(axis_label,
                 -age_ka),
         percentage,
         fill = raw_material)) +
  geom_col(position = "fill") +
  xlab("Assemblage (youngest at the top)") +
  ylab("Percentage") +
  scale_fill_viridis_d(name = "Raw material type",
                       option = "C") +
  coord_flip() +
  theme_minimal(base_size = 12)

ggsave(here::here("analysis/figures/005-raw-materials.png"))

