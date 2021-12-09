suppressPackageStartupMessages(library(tidyverse))
library(glue)
library(ggrepel)
library(here)
library(cowplot)

# data from PhD data sheet, not KAS sheet.
mydata <- read.csv(here("analysis/data/raw_data/General_info.csv"))

#volume of the cultural layer from KAS data sheet.
kasv <- read.csv(here("analysis/data/raw_data/Dating_info.csv"))

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

# retouch, density, age
Assemblage_info <- read_csv(here::here("analysis/data/raw_data/Assemblage_info.csv"))

Assemblage_info_retouch_density_ages <-
  Assemblage_info %>%
  pivot_longer(-X1,
               names_to = "sites") %>%
  left_join(kasv_tidy)

Assemblage_info_retouch_density_ages_prop <-
  Assemblage_info_retouch_density_ages %>%
  filter(!X1 %in% c("debris",
                    "unkown",
                    "pebble",
                    "metate",
                    "flake",
                    "unfinished",
                    "ground",
                    "blank"
  )) %>%
  group_by(sites) %>%
  mutate(prop_retouched = sum(value, na.rm = TRUE) / total_artifacts) %>%
  arrange(sites) %>%
  dplyr::select(-X1, -value) %>%
  distinct_all()

# retouch over time
retouch_over_time_subplot <-
  ggplot(Assemblage_info_retouch_density_ages_prop,
         aes(date_age / 1000,
             prop_retouched)) +
  geom_point(size = 2,
             colour = "grey80") +
  labs(x = "Age of assemblage (ka)",
       y = "Proportion retouched") +
  theme_bw(base_size = 4) +
  theme(axis.title = element_text(size = 6.5))


# compute t-test for retouch pieces and SP existence
retouch_sp_ttest <-
  t.test(prop_retouched ~ has_sp, data = Assemblage_info_retouch_density_ages_prop)

# extract elements from the t-test output
retouch_sp_ttest_t <- round(unname(retouch_sp_ttest$statistic), 3)
retouch_sp_ttest_p <- round(unname(retouch_sp_ttest$p.value ), 3)
retouch_sp_ttest_df <- round(unname(retouch_sp_ttest$parameter ), 3)

# t(degress of freedom) = the t statistic, p = p value.
retouch_sp_ttest_str <-
  paste0("t(", retouch_sp_ttest_df, ") = ", retouch_sp_ttest_t, ", p = ", retouch_sp_ttest_p)

# box plot for # retouch pieces by stemmed point

retouch_sp_sub_plot <-
  ggplot(Assemblage_info_retouch_density_ages_prop,
         aes(has_sp,
             prop_retouched)) +
  geom_boxplot(lwd = 0.1) +
  annotate("text",
           x = 1.5,
           y = 1.25,
           label = retouch_sp_ttest_str,
           size = 1.5) +
  theme_bw(base_size = 4)  +
  labs(x = "Contains stemmed points?",
       y = "Proportion retouched",
       size = 2) +
  theme(axis.title = element_text(size = 6.5))

# compute correlation
kas_sites_retouch_density_corr <-
  cor.test(Assemblage_info_retouch_density_ages_prop$prop_retouched,
           Assemblage_info_retouch_density_ages_prop$artefact_density)

r_value <- round(unname(kas_sites_retouch_density_corr$estimate), 3)
p_value <- round(unname(kas_sites_retouch_density_corr$p.value ), 3)
t_value <- round(unname(kas_sites_retouch_density_corr$statistic ), 3)
df_value <- round(unname(kas_sites_retouch_density_corr$parameter ), 3)


# t(degress of freedom) = the t statistic, p = p value.
combined_retouch_density_ttest_str <-
  paste0("t(", df_value, ") = ", t_value, ", p = ", p_value)

size <-  6
Assemblage_info_retouch_density_ages_prop_main_plot <-
  ggplot(Assemblage_info_retouch_density_ages_prop,
         aes(artefact_density,
             prop_retouched
         )) +
  geom_point(aes(colour = date_age / 1000,
                 size = total_artifacts,
                 shape = has_sp)) +
  # geom_text_repel(aes(label = sites),
  #                 nudge_x = 0.075,
  #                 nudge_y = -0.075,
  #                 size = 5) +
  geom_smooth(alpha = 0.2,
              method = "lm") +
  scale_y_log10(limits = c(0.001, 1),
                labels = scales::comma_format(accuracy = 0.001)) +
  scale_x_log10(limits = c(0.001, 10),
                labels = scales::comma_format(accuracy = 0.001)) +
  theme_minimal(base_size = 6) +
  scale_shape_discrete("Contains\nstemmed\npoints?") +
  scale_size("Total number\nof artifacts") +
  scale_color_viridis_c(name = "Age of\nassemblage (ka)") +
  xlab("Artifact volumetric density") +
  ylab("Proportion of retouched pieces") +
  annotate("text",
           x = 0.005,
           y = 0.7,
           label = "Curated",
           size = size - 2,
           colour = "grey50") +
  annotate("text",
           x = 5,
           y = 0.06,
           label = "Expedient",
           size = size - 2,
           colour = "grey50") +
  annotate("text",
           x = 0.05,
           y = 0.03,
           label = glue('r = {round(r_value, 3)}, t({df_value}) =  {round(t_value, 3)}, p = {round(p_value, 3)}'),
           size = size - 3,
           colour = "grey50") +
  theme(axis.title = element_text(size = 11))


ggdraw(Assemblage_info_retouch_density_ages_prop_main_plot) +
  draw_plot(retouch_over_time_subplot,
            .1, .1,
            .35, .28) +
  draw_plot(retouch_sp_sub_plot,
            .46, .1,
            .23, .28) +
  theme(panel.background = element_rect(fill='white', colour="white"),
                               plot.background = element_rect(fill='white', colour="white"))



ggsave(here::here("analysis/figures/003-retouch-by-density.png"),
       width = 6,
       height = 6,
       units = "in")

