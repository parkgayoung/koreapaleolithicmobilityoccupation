suppressPackageStartupMessages(library(tidyverse))
library(glue)
library(ggrepel)
library(here)


# data from PhD data sheet, not KAS sheet.
mydata <- read.csv(here("analysis/data/raw_data/General_info.csv"))

# raw material data from KAS data sheet.
kasr <- read.csv(here("analysis/data/raw_data/Rawmaterial_info.csv"))

# join artefact type freqs with site data

mydata_ages <-
  mydata %>%
  separate(C14.BP., into = c('age', 'error'),
           sep = "±") %>%
  mutate(age_ka = parse_number(age) / 1000,
         error = parse_number(error)) %>%
  mutate(has_sp = ifelse(!is.na(SP.), "yes", "no"))

 #raw material information
  kasr_long1 <-
  kasr %>%
  gather(site,
         count,
         -X) %>%
  filter(!is.na(count)) %>%
  group_by(site) %>%
  mutate(percentage = count / sum(count, na.rm = TRUE) * 100,
         total = sum(count, na.rm = TRUE)) %>%
  filter(!is.na(percentage)) %>%
    filter(!X %in% c(
      #'gneiss', exclude raw materials cluded in less then 4 assemblages
      'crystal',
      'basalt',
      'etc.',
      'iron_ore',
      'slate',
      'limestone',
      'granite',
      'gneiss',
      'tuff',
      'amphibolite',
      'andesite',
      'grit',
      'metamorphic-rock',
      'quartzitic-gneiss',
      'siliceous-schist')) %>%
  # if percentage is <10%, call it 'other'
  mutate(raw_material = X) %>%  #ifelse(percentage >= 10, as.character(X), "other")) %>%
  #mutate(raw_material = ifelse(raw_material == "etc.", "other", raw_material)) %>%
  dplyr::select(-X)


  # join to get ages of the sites
kasr_long2 <-
  kasr_long1 %>%
  group_by(site, raw_material) %>%
  summarise(percentage = sum(percentage)) %>%
  ungroup() %>%
  left_join(kasr_long1 %>%
              dplyr::select(site, raw_material, total),
            by = c('site', "raw_material")) %>%
  distinct(.keep_all = TRUE) %>%
  # join to get ages of the sites
  left_join(mydata_ages,
            by = c('site' = 'site_name')) %>%
  # mutate(age_ka = jitter(age_ka),
  #        age_bin  = ntile(age_ka, 5)) %>%
  arrange(age_ka) %>%
  filter(!is.na(age_ka)) %>%
  mutate(axis_label = glue('{site_new_name} ({round(age_ka,1)} ka, n = {total})')) %>%
  # replace _ with space for pretty text in legend
  mutate(raw_material = str_replace_all(raw_material, "_", " "))


kasr_long2_pill_plot <-
ggplot(kasr_long2,
       aes(
         reorder(axis_label,
                 -age_ka),
         percentage,
         fill = raw_material)) +
  geom_col(position = "fill") +
  xlab("") +
  ylab("Assemblage proportion") +
  scale_fill_viridis_d(name = "Raw material",
                       option = "C") +
  coord_flip() +
  theme_minimal(base_size = 8) +
  theme(panel.background = element_rect(fill='white', colour="white"),
                                    plot.background = element_rect(fill='white', colour="white"))

ggsave(here::here("analysis/figures/005-raw-materials.png"),
       width = 4.45,
       height = 5,
       units = "in")

# interactive
# plotly::ggplotly(kasr_long2_pill_plot)




























