# --------------------------------------------------------------------------
# Script author: Irena Zimovska 
# Date: 24.03.2024 
# Contact: irenazimovska@gmail.com 
# --------------------------------------------------------------------------

# Load necessary packages
if (!require('pacman')) install.packages('pacman')
pacman::p_load(here, DescTools, dplyr, tidyverse, ggplot2, Hmisc)

# -----------------------------------------------------------------------------
######################.     CHES data  ######################################

ches <- read.csv("https://www.chesdata.eu/s/CHES2019V3.csv", 
                 fileEncoding = "UTF-8")
# Save to csv 
write_csv(ches, here('data/ches.csv'))

# -----------------------------------------------------------------------------
######## European Social Survey (ESS - 9 round, wave 3) #######################


# Load ESS wave 9
ess9 <- spss.get(here('data/ESS9e03_2/ESS9e03_2.sav'))

# -----------------------------------------------------------------------------
################          ESS PROCESSING           ############################

## Party variables
# Columns of party vote for each country
prtv <- grep("prtv",colnames(ess9))

# Change into character vectors
ess9 <- ess9 %>% mutate_at(prtv, as.character)

col_fun <- function(df, Cols){
  df2 <- df %>%
    mutate(party = coalesce(!!!as.list(df[, Cols])))
  return(df2)
}

# Create an integrated column of party vote
ess9 <- col_fun(df = ess9, Cols = prtv)

unique(ess9$party)

# -----------------------------------------------------------------------------
################          PopuList party classification    ####################

PopuList <- read.csv(here("data/The PopuList 3.0.csv"), sep = ';')

# PopuList2 <- read.csv(here("data/party_list.csv"))

# transform names and types to make a join 
ess9$cntry <- as.character(ess9$cntry)
names(PopuList)[names(PopuList) == "country_name"] <- 'cntry'
names(PopuList)[names(PopuList) == "party_name_short"] <- 'party'


ess9 <- ess9 %>% left_join(PopuList, by = c("cntry", "party"))

