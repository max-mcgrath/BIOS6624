library(tidyr)
library(dplyr)
library(magrittr)
library(table1)

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
#   covariates, drop reference columns
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
    mutate(ADH_2 = as.numeric(.data$ADH_2 == 1 | .data$ADH_2 == 2)) %>%
    select(-LOG_VLOAD_2, -LEU3N_2, -MENT_2, -PHYS_2,
           -EDUC_0, -RACE_0, -NEWID, -SMOKE_0, -WHITE_NH, -HS)

# Keep only complete cases
cleanData <- fullData %>% drop_na() %>%
    filter(.data$BMI_0 != -1,
           .data$BMI_0 <= 50)

# Data for Table 1 (cleaned, but doesn't split variables into dummy columns)
tableOneData <- full_join(yearZero, yearTwo, by = "NEWID") %>%
    drop_na() %>%
    filter(.data$BMI_0 != -1,
           .data$BMI_0 <= 50) %>%
    mutate(LOG_VLOAD_DIFF = LOG_VLOAD_2 - LOG_VLOAD_0,
           LEU3N_DIFF = LEU3N_2 - LEU3N_0,
           MENT_DIFF = MENT_2 - MENT_0,
           PHYS_DIFF = PHYS_2 - PHYS_0) %>%
    mutate(RACE_0 = as.factor(case_when(.data$RACE_0 == 1 ~ "White",
                              .data$RACE_0 == 2 ~ "Hispanic",
                              .data$RACE_0 == 3 ~ "Black",
                              .data$RACE_0 == 4 ~ "Hispanic",
                              .data$RACE_0 == 8 ~ "Hispanic",
                              TRUE ~ "Other")),
           EDUC_0 = as.factor(case_when(.data$EDUC_0 <= 2 ~ "Less than HS",
                              .data$EDUC_0 == 3 | .data$EDUC_0 == 4 ~ "HS",
                              TRUE ~ "Greater than HS")),
           SMOKE_0 = as.factor(case_when(.data$SMOKE_0 == 3 ~ "Active Smoker",
                               TRUE ~ "Non-Smoker")),
           ADH_2 = as.factor(case_when(.data$ADH_2 == 1 | .data$ADH_2 == 2 ~ 
                                           "Greater than 95% Adherent",
                                       TRUE ~ "Less than 95% Adherent")),
           VLOAD_0 = exp(LOG_VLOAD_0),
           VLOAD_2 = exp(LOG_VLOAD_2),
           DRUGS_0 = factor(.data$DRUGS_0,
                               levels = c(0,1),
                               labels = c("No Hard Drug Use at Baseline",
                                          "Hard Drug Use at Baseline")))

# Citation: Code borrowed (and modified) from Emily Cooper
label(tableOneData$AGE_0) <- "Age at baseline (years)"
label(tableOneData$BMI_0) <- "BMI at baseline"
label(tableOneData$RACE_0) <- "Race/Ethnicity at baseline"
label(tableOneData$SMOKE_0) <- "Smoking status at baseline"
label(tableOneData$EDUC_0) <- "Education at baseline"
label(tableOneData$ADH_2) <- "Adherence to medication at 2-years"
label(tableOneData$LEU3N_DIFF) <- "Change in # of CD4+ cells"
label(tableOneData$LOG_VLOAD_DIFF) <- "Change in standardized viral load (log10 copies/ml)"
label(tableOneData$MENT_DIFF) <- "Change in SF36 MCS score"
label(tableOneData$PHYS_DIFF) <- "Change in SF36 PCS score"
label(tableOneData$LEU3N_0) <- "Baseline # of CD4+ cells"
label(tableOneData$LOG_VLOAD_0) <- "Baseline standardized viral load (log10 copies/ml)"
label(tableOneData$MENT_0) <- "Baseline SF36 MCS score"
label(tableOneData$PHYS_0) <- "Baseline SF36 PCS score"

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
