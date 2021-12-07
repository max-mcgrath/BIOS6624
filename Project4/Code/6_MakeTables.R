library(dplyr)
library(janitor)

# Read data
nSim <- readRDS("DataRaw/nSim.rda")
coefEstsBS <- readRDS("DataRaw/coefEstsBS.rda")
coefEstsAIC <- readRDS("DataRaw/coefEstsAIC.rda")
coefEstsBIC <- readRDS("DataRaw/coefEstsBIC.rda")
coefEstsLASSOCV <- readRDS("DataRaw/coefEstsLASSOCV.rda")
coefEstsLASSOFIX <- readRDS("DataRaw/coefEstsLASSOFIX.rda")
coefEstsENCV <- readRDS("DataRaw/coefEstsENCV.rda")
coefEstsENFIX <- readRDS("DataRaw/coefEstsENFIX.rda")

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
    type1error = c(NA, NA, NA, NA, NA, NA, NA),
    type2error = c(NA, NA, NA, NA, NA, NA, NA)
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
# Calculate quantiles 
quantsDF <- rbind(coefEstsBS, coefEstsAIC, coefEstsBIC, coefEstsLASSOFIX,
                  coefEstsLASSOCV, coefEstsENFIX, coefEstsENCV) %>%
    as.data.frame() %>%
    mutate(model = rep(c("BS", "AIC", "BIC", "LASSOFIX", 
                         "LASSOCV", "ENFIX", "ENCV"), each = 1000)) %>%
    group_by(model) %>%
    summarize(across(.cols = c(V1, V2, V3, V4, V5),
                     .fns = function(x) {
                         paste0("(", round(quantile(x, 0.025, na.rm = TRUE), 3),
                                ", ", 
                                round(quantile(x, 0.975, na.rm = TRUE), 3),
                                ")")
                     }))

quantsDF <- quantsDF[match(c("BS", "AIC", "BIC", "LASSOFIX", "LASSOCV", "ENFIX", 
                         "ENCV"), quantsDF$model), ]

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
    ciB1 = c(quantsDF$V1[1], quantsDF$V1[2], quantsDF$V1[3], quantsDF$V1[4],
             quantsDF$V1[5], quantsDF$V1[6], quantsDF$V1[7]),
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
    ciB2 = c(quantsDF$V2[1], quantsDF$V2[2], quantsDF$V2[3], quantsDF$V2[4],
             quantsDF$V2[5], quantsDF$V2[6], quantsDF$V2[7]),
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
    ciB3 = c(quantsDF$V3[1], quantsDF$V3[2], quantsDF$V3[3], quantsDF$V3[4],
             quantsDF$V3[5], quantsDF$V3[6], quantsDF$V3[7]),
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
    ciB4 = c(quantsDF$V4[1], quantsDF$V4[2], quantsDF$V4[3], quantsDF$V4[4],
             quantsDF$V4[5], quantsDF$V4[6], quantsDF$V4[7]),
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
    ciB5 = c(quantsDF$V5[1], quantsDF$V5[2], quantsDF$V5[3], quantsDF$V5[4],
             quantsDF$V5[5], quantsDF$V5[6], quantsDF$V5[7]),
    tprB5 = c(truePositiveBS[5], truePositiveAIC[5], truePositiveBIC[5], 
              truePositiveLASSOFIX[5], truePositiveLASSOCV[5], 
              truePositiveENFIX[5], truePositiveENCV[5])
) %>%
    mutate(across(where(is.double), .fn = ~ round(.x, 3))) %>%
    t() %>%
    row_to_names(row_number = 1) %>%
    as.data.frame()

coefDF <- cbind("stat" = rownames(coefDF), 
                data.frame(coefDF, row.names = NULL))

coefDF$stat <- c("Bias", "95\\% CI", "TPR",
                 "Bias", "95\\% CI", "TPR",
                 "Bias", "95\\% CI", "TPR",
                 "Bias", "95\\% CI", "TPR",
                 "Bias", "95\\% CI", "TPR")

colnames(modelDF) <- colnames(coefDF)


