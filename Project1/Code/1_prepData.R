library(tidyr)
library(dplyr)
library(magrittr)

dataRaw <- read.csv("DataRaw/hiv_6624_final.csv")

# age, body mass index,smoking status, education, race/ethnicity, and adherence

# Select baseline outcomes and relevant baseline covariates
yearZero <- dataRaw %>%
    filter(years == 0) %>%
    select(newid, VLOAD_0 = VLOAD, LEU3N_0 = LEU3N, AGG_MENT_0 = AGG_MENT, 
           AGG_PHYS_0 = AGG_PHYS, hard_drugs_0 = hard_drugs, EDUC_0 = EDUCBAS,
           age_0 = age, RACE_0 = RACE, BMI_0 = BMI, SMOKE_0 = SMOKE)

# Select year two outcomes and relevant year two covariates
yearTwo <- dataRaw %>%
    filter(years == 2) %>%
    select(newid, VLOAD_2 = VLOAD, LEU3N_2 = LEU3N, AGG_MENT_2 = AGG_MENT, 
           AGG_PHYS_2 = AGG_PHYS,
           ADH_2 = ADH)

# Combine data, create outcome difference columns
fullData <- full_join(yearZero, yearTwo, by = "newid") %>%
    mutate(VLOAD_DIFF = VLOAD_2 - VLOAD_0,
           LEU3N_DIFF = LEU3N_2 - LEU3N_0,
           MENT_DIFF = AGG_MENT_2 - AGG_MENT_0,
           PHYS_DIFF = AGG_PHYS_2 - AGG_PHYS_0,
           .keep = "unused")

# Split education into subcategories
# Will do later
