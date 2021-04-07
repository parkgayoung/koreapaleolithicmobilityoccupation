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


# compute t-test
den_sp_ttest <-
  t.test(artefact_density ~ has_sp, data = kasv_tidy)

# extract elements from the t-test output
den_sp_ttest_t <- round(unname(den_sp_ttest$statistic), 3)
den_sp_ttest_p <- round(unname(den_sp_ttest$p.value ), 3)
den_sp_ttest_df <- round(unname(den_sp_ttest$parameter ), 3)

# t(degress of freedom) = the t statistic, p = p value.
den_sp_ttest_str <-
  paste0("t(", den_sp_ttest_df, ") = ", den_sp_ttest_t, ", p = ", den_sp_ttest_p)

#Volume and artefact counts to get density over time.

density_sp_sub_plot <-
  ggplot(kasv_tidy,
         aes(has_sp,
             artefact_density)) +
  geom_boxplot(lwd = 0.1) +
  annotate("text",
           x = 1.5,
           y = 9,
           label = den_sp_ttest_str,
           size = 1.5) +
  theme_bw(base_size = 6)  +
  labs(x = "Stemmed points present?",
       y = "Artifact density")


density_sp_main_plot <-
  ggplot(kasv_tidy,
         aes(date_age / 1000,
             artefact_density)) +
  geom_point(aes(size = total_artifacts,
                 colour = has_sp)) +
  ylab(bquote('Artifact density'~(n/m^3))) +
  xlab("Age of assemblage (ka)") +
  scale_size_continuous(name = "Total number\nof artifacts")  +
  scale_color_viridis_d(name = "Stemmed\npoints\npresent?") +
  theme_minimal(base_size = 8)

# https://wilkelab.org/cowplot/articles/drawing_with_on_plots.html
library(cowplot)
ggdraw(density_sp_main_plot) +
  draw_plot(density_sp_sub_plot,
            .37, .62,
            .32, .33)

ggsave(here::here("analysis/figures/002-age-by-density.png"),
       width = 4.45,
       height = 4.45,
       units = "in")

