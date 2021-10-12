suppressPackageStartupMessages(library(tidyverse))
library(glue)
library(ggrepel)
library(here)

dates <-
  read.csv(here("analysis/data/raw_data/Radiocarbon dates_revision.csv"))


dates_clean <-
  dates %>%
  mutate(age = parse_number(as.character(age)),
         error = parse_number(as.character(error))) %>%
  # filter age range here
  filter(age < 50500,
         age >= 10000,
         method == 'AMS') %>%   #exclude OSL dates
  filter(!site_name_original %in% c('Nobong',
                                    'Geoduri',
                                    'Geumneungdong',
                                    'Janggi',
                                    'Sanggari',
                                    'Sinheug',
                                    'Oeryang',
                                    'Eosanri',
                                    'Jeommal',
                                    'Pyeongneungdong'))
