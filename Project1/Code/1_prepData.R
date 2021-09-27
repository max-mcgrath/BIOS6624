library(tidyr)
library(dplyr)
library(magrittr)

dataRaw <- read.csv("DataRaw/hiv_6624_final.csv")

yearZero <- dataRaw %>%
    filter(years == 0) %>%
    select(newid, VLOAD, LEU3N, AGG_MENT, AGG_PHYS, hard_drugs)

yearTwo <- dataRaw %>%
    filter(years == 2) %>%
    select(newid, VLOAD, LEU3N, AGG_MENT, AGG_PHYS, hard_drugs)
