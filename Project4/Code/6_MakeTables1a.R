library(dplyr)
library(janitor)

# Read data
nSim <- readRDS("DataRaw/nSim.rda")
coefEstsBS <- readRDS("DataRaw/coefEstsBS1a.rda")
coefEstsAIC <- readRDS("DataRaw/coefEstsAIC1a.rda")
coefEstsBIC <- readRDS("DataRaw/coefEstsBIC1a.rda")
coefEstsLASSOCV <- readRDS("DataRaw/coefEstsLASSOCV1a.rda")
coefEstsLASSOFIX <- readRDS("DataRaw/coefEstsLASSOFIX1a.rda")
coefEstsENCV <- readRDS("DataRaw/coefEstsENCV1a.rda")
coefEstsENFIX <- readRDS("DataRaw/coefEstsENFIX1a.rda")

# Model summary table ----------------------------------------------------------
modelDF <- data.frame(
    model = c("p-value", "AIC", "BIC", "LASSO w/ fixed lambda",
              "LASSO w/ CV", "EN w/ fixed lambda", "EN w/ CV"),
    averageTPR = c(totalTPR[1], totalTPR[2], totalTPR[3], totalTPR[4], 
                   totalTPR[5], totalTPR[6], totalTPR[7]),
    FPR = c(falsePostiveBS, falsePostiveAIC, falsePostiveBIC, 
            falsePostiveLASSOFIX, falsePostiveLASSOCV, falsePostiveENFIX, 
            falsePostiveENCV),
    FDR = c(falseDiscoveryBS, falseDiscoveryAIC, falseDiscoveryBIC, 
            falseDiscoveryLASSOFIX, falseDiscoveryLASSOCV, falseDiscoveryENFIX,
            falseDiscoveryENCV),
    type1error = c(typeOneBS, typeOneAIC, typeOneBIC, typeOneLASSOFIX, 
                   typeOneLASSOCV, typeOneENFIX, typeOneENCV),
    type2error = c(typeTwoBS, typeTwoAIC, typeTwoBIC, typeTwoLASSOFIX, 
                   typeTwoLASSOCV, typeTwoENFIX, typeTwoENCV)
) %>%
    mutate(across(where(is.double), .fn = ~ round(.x, 3))) %>%
    t() %>%
    row_to_names(row_number = 1) %>%
    as.data.frame() %>%
    mutate_all(as.numeric)

modelDF <- cbind("stat" = rownames(modelDF), 
                 data.frame(modelDF, row.names = NULL))

modelDF$stat <- c("Total TPR", "FPR", "FDR", "Type I Error", "Type 2 Error")

# Coefficient summary table ----------------------------------------------------
coefDF <- data.frame(
    model = c("p-value", "AIC", "BIC", "LASSO",
              "LASSO w/ CV", "EN", "EN w/ CV"),
    biasB1 = c(1/6 - mean(coefEstsBS[, 1], na.rm = TRUE), 
               1/6 - mean(coefEstsAIC[, 1], na.rm = TRUE), 
               1/6 - mean(coefEstsBIC[, 1], na.rm = TRUE), 
               1/6 - mean(coefEstsLASSOFIX[, 1], na.rm = TRUE), 
               1/6 - mean(coefEstsLASSOCV[, 1], na.rm = TRUE), 
               1/6 - mean(coefEstsENFIX[, 1], na.rm = TRUE), 
               1/6 - mean(coefEstsENCV[, 1], na.rm = TRUE)),
    ciB1 = c(mean(coefEstsBS[,21], na.rm = TRUE), 
             mean(coefEstsAIC[,21], na.rm = TRUE), 
             mean(coefEstsBIC[,21], na.rm = TRUE), 
             mean(coefEstsLASSOFIX[,21], na.rm = TRUE), 
             mean(coefEstsLASSOCV[,21], na.rm = TRUE), 
             mean(coefEstsENFIX[,21], na.rm = TRUE), 
             mean(coefEstsENCV[,21], na.rm = TRUE)),
    tprB1 = c(truePositiveBS[1], truePositiveAIC[1], truePositiveBIC[1], 
              truePositiveLASSOFIX[1], truePositiveLASSOCV[1], 
              truePositiveENFIX[1], truePositiveENCV[1]),
    biasB2 = c(1/3 - mean(coefEstsBS[, 2], na.rm = TRUE), 
               1/3 - mean(coefEstsAIC[, 2], na.rm = TRUE), 
               1/3 - mean(coefEstsBIC[, 2], na.rm = TRUE), 
               1/3 - mean(coefEstsLASSOFIX[, 2], na.rm = TRUE), 
               1/3 - mean(coefEstsLASSOCV[, 2], na.rm = TRUE), 
               1/3 - mean(coefEstsENFIX[, 2], na.rm = TRUE), 
               1/3 - mean(coefEstsENCV[, 2], na.rm = TRUE)),
    ciB2 = c(mean(coefEstsBS[,22], na.rm = TRUE), 
             mean(coefEstsAIC[,22], na.rm = TRUE), 
             mean(coefEstsBIC[,22], na.rm = TRUE), 
             mean(coefEstsLASSOFIX[,22], na.rm = TRUE), 
             mean(coefEstsLASSOCV[,22], na.rm = TRUE), 
             mean(coefEstsENFIX[,22], na.rm = TRUE), 
             mean(coefEstsENCV[,22], na.rm = TRUE)),
    tprB2 = c(truePositiveBS[2], truePositiveAIC[2], truePositiveBIC[2], 
              truePositiveLASSOFIX[2], truePositiveLASSOCV[2], 
              truePositiveENFIX[2], truePositiveENCV[2]),
    biasB3 = c(1/2 - mean(coefEstsBS[, 3], na.rm = TRUE), 
               1/2 - mean(coefEstsAIC[, 3], na.rm = TRUE), 
               1/2 - mean(coefEstsBIC[, 3], na.rm = TRUE), 
               1/2 - mean(coefEstsLASSOFIX[, 3], na.rm = TRUE), 
               1/2 - mean(coefEstsLASSOCV[, 3], na.rm = TRUE), 
               1/2 - mean(coefEstsENFIX[, 3], na.rm = TRUE), 
               1/2 - mean(coefEstsENCV[, 3], na.rm = TRUE)),
    ciB3 = c(mean(coefEstsBS[,23], na.rm = TRUE), 
             mean(coefEstsAIC[,23], na.rm = TRUE), 
             mean(coefEstsBIC[,23], na.rm = TRUE), 
             mean(coefEstsLASSOFIX[,23], na.rm = TRUE), 
             mean(coefEstsLASSOCV[,23], na.rm = TRUE), 
             mean(coefEstsENFIX[,23], na.rm = TRUE), 
             mean(coefEstsENCV[,23], na.rm = TRUE)),
    tprB3 = c(truePositiveBS[3], truePositiveAIC[3], truePositiveBIC[3], 
              truePositiveLASSOFIX[3], truePositiveLASSOCV[3], 
              truePositiveENFIX[3], truePositiveENCV[3]),
    biasB4 = c(2/3 - mean(coefEstsBS[, 4], na.rm = TRUE), 
               2/3 - mean(coefEstsAIC[, 4], na.rm = TRUE), 
               2/3 - mean(coefEstsBIC[, 4], na.rm = TRUE), 
               2/3 - mean(coefEstsLASSOFIX[, 4], na.rm = TRUE), 
               2/3 - mean(coefEstsLASSOCV[, 4], na.rm = TRUE), 
               2/3 - mean(coefEstsENFIX[, 4], na.rm = TRUE), 
               2/3 - mean(coefEstsENCV[, 4], na.rm = TRUE)),
    ciB4 = c(mean(coefEstsBS[,24], na.rm = TRUE), 
             mean(coefEstsAIC[,24], na.rm = TRUE), 
             mean(coefEstsBIC[,24], na.rm = TRUE), 
             mean(coefEstsLASSOFIX[,24], na.rm = TRUE), 
             mean(coefEstsLASSOCV[,24], na.rm = TRUE), 
             mean(coefEstsENFIX[,24], na.rm = TRUE), 
             mean(coefEstsENCV[,24], na.rm = TRUE)),
    tprB4 = c(truePositiveBS[3], truePositiveAIC[3], truePositiveBIC[3], 
              truePositiveLASSOFIX[3], truePositiveLASSOCV[3], 
              truePositiveENFIX[3], truePositiveENCV[3]),
    biasB5 = c(5/6 - mean(coefEstsBS[, 5], na.rm = TRUE), 
               5/6 - mean(coefEstsAIC[, 5], na.rm = TRUE), 
               5/6 - mean(coefEstsBIC[, 5], na.rm = TRUE), 
               5/6 - mean(coefEstsLASSOFIX[, 5], na.rm = TRUE), 
               5/6 - mean(coefEstsLASSOCV[, 5], na.rm = TRUE), 
               5/6 - mean(coefEstsENFIX[, 5], na.rm = TRUE), 
               5/6 - mean(coefEstsENCV[, 5], na.rm = TRUE)),
    ciB5 = c(mean(coefEstsBS[,25], na.rm = TRUE), 
             mean(coefEstsAIC[,25], na.rm = TRUE), 
             mean(coefEstsBIC[,25], na.rm = TRUE), 
             mean(coefEstsLASSOFIX[,25], na.rm = TRUE), 
             mean(coefEstsLASSOCV[,25], na.rm = TRUE), 
             mean(coefEstsENFIX[,25], na.rm = TRUE), 
             mean(coefEstsENCV[,25], na.rm = TRUE)),
    tprB5 = c(truePositiveBS[5], truePositiveAIC[5], truePositiveBIC[5], 
              truePositiveLASSOFIX[5], truePositiveLASSOCV[5], 
              truePositiveENFIX[5], truePositiveENCV[5])
) %>%
    mutate(across(where(is.double), .fn = ~ round(.x, 3))) %>%
    t() %>%
    row_to_names(row_number = 1) %>%
    as.data.frame() %>%
    mutate_all(as.numeric)

coefDF <- cbind("stat" = rownames(coefDF), 
                data.frame(coefDF, row.names = NULL))

coefDF$stat <- c("Bias", "95\\% CI", "TPR",
                 "Bias", "95\\% CI", "TPR",
                 "Bias", "95\\% CI", "TPR",
                 "Bias", "95\\% CI", "TPR",
                 "Bias", "95\\% CI", "TPR")

colnames(modelDF) <- colnames(coefDF)


