##############################################################################
#               Script 00 - read databases and exclude parties
#                           Irena Zimovska 
#                             16.07.2024
#############################################################################
# 0. Set Up Session Settings 
# ------------------------------------------------------------------------------
if (!require("here")) install.packages("here")
library(here)

# Set working directory
setwd(here())
# Set environment language to English
Sys.setlocale("LC_ALL", "English")
# Avoid scientific notation
options(scipen = 999)
# ------------------------------------------------------------------------------

# 1. Import Libraries & Load Data 
# ------------------------------------------------------------------------------
if (!require('pacman')) install.packages('pacman')
pacman::p_load(here, DescTools, dplyr, forcats, tidyverse, scorecard, 
               ggplot2, Hmisc, stargazer, sandwich, bucky)
# ------------------------------------------------------------------------------
# ess9 <- read.csv(here('data/final_database.csv'), sep = ';')
# ess9 <- read.csv(here('data/final_database_zeroes.csv'), sep = ',')

scales_dict <- read.csv(here('data/party_classification_dict.csv'), sep = ';')
ess9 <- read.csv(here('data/final_database_dummies.csv'), sep = ',')

# Read and transform RFPOPI classifications
scales_dict$ESS9_2 <- na_if(scales_dict$ESS9_2, 'N/A')
# Filtering the data frame
filtered_data <- scales_dict[!is.na(scales_dict$ESS9_2) &         # Exclude rows where ESS9_2 is NA
                               !is.na(scales_dict$populism_ide) & # Exclude rows where populism_ide is NA
                               scales_dict$ESS9_2 != 'Other' &    # Exclude rows where ESS9_2 is 'Other'
                               scales_dict$ESS9_2 != 'Blank paper' &
                               scales_dict$ESS9_2 != 'Obedineni patrioti - NFSB, Ataka i VMRO' &
                               scales_dict$ESS9_2 != "Koalicija HDSSB-HKS",
]  # Exclude rows where ESS9_2 is 'Koalicija HDSSB-HK' & 'Obedineni patrioti...'

ess9 <- ess9 %>% left_join(filtered_data[, c("ESS9_2", "populism_ide", "populism_rhet", 'full_country_name')],
                           by = c('ESS9_party_name' = 'ESS9_2',
                                  'cntry' = 'full_country_name'
                                  
                           ))

# -----------------------------------------------------------------------------
# originally in rfpopi there are 158 parties 
# in the dictionary we have 153 (ok)

ess9$populism_ide <- gsub(",", ".", ess9$populism_ide)
ess9$populism_ide <- as.numeric(as.character(ess9$populism_ide))

ess9$populism_rhet <- gsub(",", ".", ess9$populism_rhet)
ess9$populism_rhet <- as.numeric(as.character(ess9$populism_rhet))

rfpopi <- ess9 %>% 
  filter(!is.na(populism_ide) & populism_ide != '') %>% 
  filter(!is.na(populism_rhet) & populism_rhet != '') %>%
  filter(ESS9_party_name != '')

rfpopi <- rfpopi %>% 
  dplyr::select(cntry, ESS9_party_name, populism_ide, populism_rhet) %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(ESS9_party_name != '') %>% 
  filter(ESS9_party_name != 'Obedineni patrioti - NFSB, Ataka i VMRO')



# Wyznaczenie zmiennych binarnych dla RFPOPI - klasyfikacja partii 
# -------------------------------------------------------------------------
cutoff <- 5 

rfpopi$rfpopi_ide <- ifelse(rfpopi$populism_ide >= cutoff, 1, 0)
rfpopi$rfpopi_rhet <- ifelse(rfpopi$populism_rhet >= cutoff, 1, 0)

ess9$rfpopi_ide <- ifelse(ess9$populism_ide >= cutoff, 1, 0)
ess9$rfpopi_rhet <- ifelse(ess9$populism_rhet >= cutoff, 1, 0) 
# -------------------------------------------------------------------------
