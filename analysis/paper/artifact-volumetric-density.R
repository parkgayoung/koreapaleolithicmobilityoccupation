suppressPackageStartupMessages(library(tidyverse))
library(glue)
library(ggrepel)
library(here)

korean_archaeological_sites <- readxl::read_excel(here("analysis/data/raw_data/korean-archaeologica-sites.xlsx"))

# data from PhD data sheet, not KAS sheet.
mydata <- read.csv(here("analysis/data/raw_data/General_info.csv"))

# raw material data from KAS data sheet.
kasr <- read.csv(here("analysis/data/raw_data/Rawmaterial_info.csv"))

# assemblage composition data from KAS data sheet.
kasa <- read.csv(here("analysis/data/raw_data/Assemblage_info.csv"))

#volume of the cultural layer from KAS data sheet.
kasv <- read.csv(here("analysis/data/raw_data/Dating_info.csv"))

dates <- read.csv(here("analysis/data/raw_data/Radiocarbon dates.csv"))

# join artefact type freqs with site data

kasa %>%
  pivot_longer(-X,
               names_to = "site_name",
               values_to = "count") %>%
  pivot_wider(names_from = "X",
              values_from = "count") %>%
  left_join(mydata)



kasv_tidy <-
  kasv %>%
  t %>%
  as_tibble() %>%
  setNames(as.character(.[1,])) %>%
  .[-1,] %>%
  mutate_all(parse_number) %>%
  mutate(artefact_density = total_artifacts / volume,
         sites = names(kasv)[-1]) %>%
  left_join(mydata, by = c('sites' = 'site_name' )) %>%
  mutate(has_sp = ifelse(is.na(SP.), "no", "yes"))


mydata_ages <-
  mydata %>%
  separate(C14.BP., into = c('age', 'error'),
           sep = "±") %>%
  mutate(age_ka = parse_number(age) / 1000,
         error = parse_number(error)) %>%
  mutate(has_sp = ifelse(!is.na(SP.), "yes", "no"))



#Volume and artefact counts to get density over time.

density_sp_sub_plot <-
  ggplot(kasv_tidy,
         aes(has_sp,
             artefact_density)) +
  geom_boxplot() +
  theme_bw(base_size = 8)  +
  labs(x = "Stemmed points present",
       y = "Artefact density")

density_sp_main_plot <-
  ggplot(kasv_tidy,
         aes(date_age / 1000,
             artefact_density)) +
  geom_point(aes(size = total_artifacts,
                 colour = has_sp)) +
  ylab(bquote('Artefact density'~(n/m^3))) +
  xlab("Age of assemblage (ka)") +
  #geom_smooth(method = "lm") +
  scale_size_continuous(name = "Total number\nof artifacts")  +
  scale_color_viridis_d(name = "Contains\nstemmed\npoints?") +
  theme_minimal(base_size = 16)  #+
#geom_text_repel(aes(label = sites),
#                nudge_x = 0.25,
#                nudge_y = 0.25)

# https://wilkelab.org/cowplot/articles/drawing_with_on_plots.html
library(cowplot)
ggdraw(density_sp_main_plot) +
  draw_plot(density_sp_sub_plot,
            .375, .67,
            .25, .25)

ggsave("figures/age-by-density.png")
