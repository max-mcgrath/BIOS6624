library(table1)
library(naniar)
source("Code/1_prepData.R")

# Create baseline table
table1(~ LOG_VLOAD_DIFF + LEU3N_DIFF + MENT_DIFF + PHYS_DIFF +
           AGE_0 + BMI_0 + RACE_0 + EDUC_0 + SMOKE_0 + ADH_2 +
           LOG_VLOAD_0 + LEU3N_0 + MENT_0 + PHYS_0 | DRUGS_0, 
       data = tableOneData)

# Examine missingness
yearZeroMissing <- yearZero %>%
    select(-NEWID) %>%
    miss_var_summary() %>%
    mutate(variable = case_when(.data$variable == "LOG_VLOAD_0" ~ "Log Viral Load",
                                .data$variable == "LEU3N_0" ~ "CD4+ Cell Count",
                                .data$variable == "PHYS_0" ~ "SF36 PCS Score",
                                .data$variable == "MENT_0" ~ "SF36 MCS Score",
                                .data$variable == "BMI_0" ~ "BMI",
                                .data$variable == "EDUC_0" ~ "Educational Attainment",
                                .data$variable == "SMOKE_0" ~ "Smoking Status",
                                .data$variable == "DRUGS_0" ~ "Hard Drug Use",
                                .data$variable == "AGE_0" ~ "Age",
                                .data$variable == "RACE_0" ~ "Race/ethnicity",
                                TRUE ~ .data$variable)) %>%
    select(variable, nMissing1 = n_miss, pctMissing1 = pct_miss)

yearTwoMissing <- yearTwo %>%
    select(-NEWID) %>%
    miss_var_summary()  %>%
    mutate(variable = case_when(.data$variable == "LOG_VLOAD_2" ~ "Log Viral Load",
                                .data$variable == "LEU3N_2" ~ "CD4+ Cell Count",
                                .data$variable == "PHYS_2" ~ "SF36 PCS Score",
                                .data$variable == "MENT_2" ~ "SF36 MCS Score",
                                .data$variable == "ADH_2" ~ "Adherence",
                                TRUE ~ .data$variable)) %>%
    select(variable, nMissing2 = n_miss, pctMissing2 = pct_miss)

missingSummary <- full_join(yearZeroMissing, yearTwoMissing)
