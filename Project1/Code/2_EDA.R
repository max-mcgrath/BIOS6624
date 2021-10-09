library(table1)
source("Code/1_prepData.R")

# Create baseline table
table1(~ LOG_VLOAD_DIFF + LEU3N_DIFF + MENT_DIFF + PHYS_DIFF +
           AGE_0 + BMI_0 + RACE_0 + EDUC_0 + SMOKE_0 + ADH_2 +
           LOG_VLOAD_0 + LEU3N_0 + MENT_0 + PHYS_0 | DRUGS_0, 
       data = tableOneData)
