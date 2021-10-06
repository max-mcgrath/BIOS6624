library(tidyr)
library(dplyr)
library(magrittr)

dataRaw <- read.csv("DataRaw/hiv_6624_final.csv")

# age, body mass index,smoking status, education, race/ethnicity, and adherence

# Select baseline outcomes and relevant baseline covariates
yearZero <- dataRaw %>%
    filter(years == 0) %>%
    select(NEWID = newid, VLOAD_0 = VLOAD, LEU3N_0 = LEU3N, 
           MENT_0 = AGG_MENT, 
           PHYS_0 = AGG_PHYS, DRUGS_0 = hard_drugs, EDUC_0 = EDUCBAS,
           AGE_0 = age, RACE_0 = RACE, BMI_0 = BMI, SMOKE_0 = SMOKE) %>%
    mutate(LOG_VLOAD_0 = log10(VLOAD_0),
           .keep = "unused")

# Select year two outcomes and relevant year two covariates
yearTwo <- dataRaw %>%
    filter(years == 2) %>%
    select(NEWID = newid, VLOAD_2 = VLOAD, LEU3N_2 = LEU3N, 
           MENT_2 = AGG_MENT, 
           PHYS_2 = AGG_PHYS, 
           ADH_2 = ADH) %>%
    mutate(LOG_VLOAD_2 = log10(VLOAD_2),
           .keep = "unused")

# Combine data, create outcome difference columns, create race and education
#   covariates
fullData <- full_join(yearZero, yearTwo, by = "NEWID") %>%
    mutate(LOG_VLOAD_DIFF = LOG_VLOAD_2 - LOG_VLOAD_0,
           LEU3N_DIFF = LEU3N_2 - LEU3N_0,
           MENT_DIFF = MENT_2 - MENT_0,
           PHYS_DIFF = PHYS_2 - PHYS_0) %>%
    mutate(WHITE_NH = as.numeric(.data$RACE_0 == 1),
           BLACK_NH = as.numeric(.data$RACE_0 == 3),
           HISPANIC = as.numeric(.data$RACE_0 == 2 | .data$RACE_0 == 4 | 
                .data$RACE_0 == 8),
           OTHER = as.numeric(.data$RACE_0 > 4 & .data$RACE_0 < 8)) %>%
    mutate(LESS_HS = as.numeric(.data$EDUC_0 <= 2),
           HS = as.numeric(.data$EDUC_0 == 3 | .data$EDUC_0 == 4),
           GREAT_HS = as.numeric(.data$EDUC_0 > 4)) %>%
    mutate(SMOKE = as.numeric(.data$SMOKE_0 == 3)) %>%
    select(-LOG_VLOAD_2, -LEU3N_2, -MENT_2, -PHYS_2,
           -EDUC_0, -RACE_0, -NEWID, -SMOKE_0)

# Keep only complete cases
cleanData <- fullData %>% drop_na()

# Create outcome specific data
vloadData <- cleanData %>% 
    select(-LEU3N_0, -LEU3N_DIFF,
           -PHYS_0, -PHYS_DIFF,
           -MENT_0, -MENT_DIFF)

leu3nData <- cleanData %>% 
    select(-LOG_VLOAD_0, -LOG_VLOAD_DIFF,
           -PHYS_0, -PHYS_DIFF,
           -MENT_0, -MENT_DIFF)

mentData <- cleanData %>% 
    select(-LOG_VLOAD_0, -LOG_VLOAD_DIFF, 
           -LEU3N_0, -LEU3N_DIFF,
           -PHYS_0, -PHYS_DIFF)

physData <- cleanData %>% 
    select(-LOG_VLOAD_0, -LOG_VLOAD_DIFF, 
           -LEU3N_0, -LEU3N_DIFF,
           -MENT_0, -MENT_DIFF)
