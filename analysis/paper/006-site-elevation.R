suppressPackageStartupMessages(library(tidyverse))
library(glue)
library(ggrepel)
library(here)


# data from PhD data sheet, not KAS sheet.
mydata <- read.csv(here("analysis/data/raw_data/General_info.csv"))


#volume of the cultural layer from KAS data sheet.
kasv <- read.csv(here("analysis/data/raw_data/Dating_info.csv"))


# site elevation
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

# compute t-test
elev_sp_ttest <-
  t.test(altitude.m._of_main_layer ~ has_sp, data = kasv_tidy)

# extract elements from the t-test output
elev_sp_ttest_t <- round(unname(elev_sp_ttest$statistic), 3)
elev_sp_ttest_p <- round(unname(elev_sp_ttest$p.value ), 3)
elev_sp_ttest_df <- round(unname(elev_sp_ttest$parameter ), 3)

# t(degress of freedom) = the t statistic, p = p value.
elev_sp_ttest_str <-
  paste0("t(", elev_sp_ttest_df, ") = ", elev_sp_ttest_t, ", p = ", elev_sp_ttest_p)


elevation_sp_sub_plot <-
  ggplot(kasv_tidy,
         aes(has_sp,
             altitude.m._of_main_layer)) +
  geom_boxplot() +
  annotate("text",
           x = 1.5,
           y = 250,
           label = elev_sp_ttest_str,
           size = 3) +
  theme_bw(base_size = 8)  +
  labs(x = "Stemmed points present",
       y = "Elevation above sea level (m)")

elevation_sp_main_plot <-
  ggplot(mydata_ages,
         aes(x = age_ka,
             y = altitude.m._of_main_layer
             )) +
  geom_point(size = 3,
             aes(color = as.factor(has_sp))) +
  xlab("Age of occupation (ka)") +
  ylab("Elevation above sea level (m)") +
  geom_smooth(se = FALSE,
              aes(color = as.factor(has_sp))) +
 # geom_smooth(se = FALSE)
  scale_colour_discrete(name = "Contains\nstemmed\npoints?") +
  theme_minimal(base_size = 16)  +
  theme(legend.position = c(0.1, 0.9))


library(cowplot)
ggdraw(elevation_sp_main_plot) +
  draw_plot(elevation_sp_sub_plot,
            .7, .60,
            .25, .32)

ggsave(here::here("analysis/figures/006-site-elevation.png"))
