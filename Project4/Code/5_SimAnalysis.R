# Read data
nSim <- readRDS("DataRaw/nSim.rda")
coefEstsBS <- readRDS("DataRaw/coefEstsBS.rda")
coefEstsAIC <- readRDS("DataRaw/coefEstsAIC.rda")
coefEstsBIC <- readRDS("DataRaw/coefEstsBIC.rda")
coefEstsLASSOCV <- readRDS("DataRaw/coefEstsLASSOCV.rda")
coefEstsLASSOFIX <- readRDS("DataRaw/coefEstsLASSOFIX.rda")
coefEstsENCV <- readRDS("DataRaw/coefEstsENCV.rda")
coefEstsENFIX <- readRDS("DataRaw/coefEstsENFIX.rda")

# Calculate true positive rate -------------------------------------------------
truePositiveBS <- c(sum(!is.na(coefEstsBS[, 1])) / nSim,
                    sum(!is.na(coefEstsBS[, 2])) / nSim,
                    sum(!is.na(coefEstsBS[, 3])) / nSim,
                    sum(!is.na(coefEstsBS[, 4])) / nSim,
                    sum(!is.na(coefEstsBS[, 5])) / nSim)
truePositiveAIC <- c(sum(!is.na(coefEstsAIC[, 1])) / nSim,
                     sum(!is.na(coefEstsAIC[, 2])) / nSim,
                     sum(!is.na(coefEstsAIC[, 3])) / nSim,
                     sum(!is.na(coefEstsAIC[, 4])) / nSim,
                     sum(!is.na(coefEstsAIC[, 5])) / nSim)
truePositiveBIC <- c(sum(!is.na(coefEstsBIC[, 1])) / nSim,
                     sum(!is.na(coefEstsBIC[, 2])) / nSim,
                     sum(!is.na(coefEstsBIC[, 3])) / nSim,
                     sum(!is.na(coefEstsBIC[, 4])) / nSim,
                     sum(!is.na(coefEstsBIC[, 5])) / nSim)
truePositiveLASSOCV <- c(sum(!is.na(coefEstsLASSOCV[, 1])) / nSim,
                         sum(!is.na(coefEstsLASSOCV[, 2])) / nSim,
                         sum(!is.na(coefEstsLASSOCV[, 3])) / nSim,
                         sum(!is.na(coefEstsLASSOCV[, 4])) / nSim,
                         sum(!is.na(coefEstsLASSOCV[, 5])) / nSim)
truePositiveLASSOFIX <- c(sum(!is.na(coefEstsLASSOFIX[, 1])) / nSim,
                          sum(!is.na(coefEstsLASSOFIX[, 2])) / nSim,
                          sum(!is.na(coefEstsLASSOFIX[, 3])) / nSim,
                          sum(!is.na(coefEstsLASSOFIX[, 4])) / nSim,
                          sum(!is.na(coefEstsLASSOFIX[, 5])) / nSim)
truePositiveENCV <- c(sum(!is.na(coefEstsENCV[, 1])) / nSim,
                      sum(!is.na(coefEstsENCV[, 2])) / nSim,
                      sum(!is.na(coefEstsENCV[, 3])) / nSim,
                      sum(!is.na(coefEstsENCV[, 4])) / nSim,
                      sum(!is.na(coefEstsENCV[, 5])) / nSim)
truePositiveENFIX <- c(sum(!is.na(coefEstsENFIX[, 1])) / nSim,
                       sum(!is.na(coefEstsENFIX[, 2])) / nSim,
                       sum(!is.na(coefEstsENFIX[, 3])) / nSim,
                       sum(!is.na(coefEstsENFIX[, 4])) / nSim,
                       sum(!is.na(coefEstsENFIX[, 5])) / nSim)

# Calculate false positive rate ------------------------------------------------
falsePostiveBS <- sum(!is.na(coefEstsBS[, 6:20])) / (nSim * 15)
falsePostiveAIC <- sum(!is.na(coefEstsAIC[, 6:20])) / (nSim * 15)
falsePostiveBIC <- sum(!is.na(coefEstsBIC[, 6:20])) / (nSim * 15)
falsePostiveLASSOCV <- sum(!is.na(coefEstsLASSOCV[, 6:20])) / (nSim * 15)
falsePostiveLASSOFIX <- sum(!is.na(coefEstsLASSOFIX[, 6:20])) / (nSim * 15)
falsePostiveENCV <- sum(!is.na(coefEstsENCV[, 6:20])) / (nSim * 15)
falsePostiveENFIX <- sum(!is.na(coefEstsENFIX[, 6:20])) / (nSim * 15)

# Calculate false discovery rate -----------------------------------------------
falseDiscoveryBS <- sum(!is.na(coefEstsBS[, 6:20])) / sum(!is.na(coefEstsBS))
falseDiscoveryAIC <- sum(!is.na(coefEstsAIC[, 6:20])) / sum(!is.na(coefEstsAIC))
falseDiscoveryBIC <- sum(!is.na(coefEstsBIC[, 6:20])) /
    sum(!is.na(coefEstsBIC))
falseDiscoveryLASSOCV <- sum(!is.na(coefEstsLASSOCV[, 6:20])) / 
    sum(!is.na(coefEstsLASSOCV))
falseDiscoveryLASSOFIX <- sum(!is.na(coefEstsLASSOFIX[, 6:20])) /
    sum(!is.na(coefEstsLASSOFIX))
falseDiscoveryENCV <- sum(!is.na(coefEstsENCV[, 6:20])) /
    sum(!is.na(coefEstsENCV))
falseDiscoveryENFIX <- sum(!is.na(coefEstsENFIX[, 6:20])) /
    sum(!is.na(coefEstsENFIX))