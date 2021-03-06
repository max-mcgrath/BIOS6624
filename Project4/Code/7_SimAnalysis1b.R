# Case Scenario 1b -------------------------------------------------------------
# ------------------------------------------------------------------------------
# Read data
nSim <- readRDS("DataRaw/nSim1b.rda")
coefEstsBS <- readRDS("DataRaw/coefEstsBS1b.rda")
coefEstsAIC <- readRDS("DataRaw/coefEstsAIC1b.rda")
coefEstsBIC <- readRDS("DataRaw/coefEstsBIC1b.rda")
coefEstsLASSOCV <- readRDS("DataRaw/coefEstsLASSOCV1b.rda")
coefEstsLASSOFIX <- readRDS("DataRaw/coefEstsLASSOFIX1b.rda")
coefEstsENCV <- readRDS("DataRaw/coefEstsENCV1b.rda")
coefEstsENFIX <- readRDS("DataRaw/coefEstsENFIX1b.rda")

# Calculate true positive rate -------------------------------------------------
totalTPR <- c(sum(!is.na(coefEstsBS[, 1:5])) / (nSim * 5),
              sum(!is.na(coefEstsAIC[, 1:5])) / (nSim * 5),
              sum(!is.na(coefEstsBIC[, 1:5])) / (nSim * 5),
              sum(!is.na(coefEstsLASSOFIX[, 1:5])) / (nSim * 5),
              sum(!is.na(coefEstsLASSOCV[, 1:5])) / (nSim * 5),
              sum(!is.na(coefEstsENFIX[, 1:5])) / (nSim * 5),
              sum(!is.na(coefEstsENCV[, 1:5])) / (nSim * 5))
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

# Calculate Type I and II error rates ------------------------------------------
# Backwards selection
typeOneBS <- 1 - sum(coefEstsBS[, 26:30] == 1, na.rm = TRUE) / 
    sum(!is.na(coefEstsBS[, 26:30]))
typeTwoBS <- sum(coefEstsBS[, 31:45] == 1, na.rm = TRUE) / 
    sum(!is.na(coefEstsBS[, 31:45]))
typeOneAIC <- 1 - sum(coefEstsAIC[, 26:30] == 1, na.rm = TRUE) / 
    sum(!is.na(coefEstsAIC[, 26:30]))
typeTwoAIC <- sum(coefEstsAIC[, 31:45] == 1, na.rm = TRUE) / 
    sum(!is.na(coefEstsAIC[, 31:45]))
typeOneBIC <- 1 - sum(coefEstsBIC[, 26:30] == 1, na.rm = TRUE) / 
    sum(!is.na(coefEstsBIC[, 26:30]))
typeTwoBIC <- sum(coefEstsBIC[, 31:45] == 1, na.rm = TRUE) / 
    sum(!is.na(coefEstsBIC[, 31:45]))

# Regularization
typeOneLASSOCV <- 1 - sum(coefEstsLASSOCV[, 26:30] == 1, na.rm = TRUE) / 
    sum(!is.na(coefEstsLASSOCV[, 26:30]))
typeTwoLASSOCV <- sum(coefEstsLASSOCV[, 31:45] == 1, na.rm = TRUE) / 
    sum(!is.na(coefEstsLASSOCV[, 31:45]))
typeOneLASSOFIX <- 1 - sum(coefEstsLASSOFIX[, 26:30] == 1, na.rm = TRUE) / 
    sum(!is.na(coefEstsLASSOFIX[, 26:30]))
typeTwoLASSOFIX <- sum(coefEstsLASSOFIX[, 31:45] == 1, na.rm = TRUE) / 
    sum(!is.na(coefEstsLASSOFIX[, 31:45]))
typeOneENCV <- 1 - sum(coefEstsENCV[, 26:30] == 1, na.rm = TRUE) / 
    sum(!is.na(coefEstsENCV[, 26:30]))
typeTwoENCV <- sum(coefEstsENCV[, 31:45] == 1, na.rm = TRUE) / 
    sum(!is.na(coefEstsENCV[, 31:45]))
typeOneENFIX <- 1 - sum(coefEstsENFIX[, 26:30] == 1, na.rm = TRUE) / 
    sum(!is.na(coefEstsENFIX[, 26:30]))
typeTwoENFIX <- sum(coefEstsENFIX[, 31:45] == 1, na.rm = TRUE) / 
    sum(!is.na(coefEstsENFIX[, 31:45]))
