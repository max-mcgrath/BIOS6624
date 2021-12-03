# Case Scenario 1a
#
# N=250 subjects with 20 variables in the model. 5 of them would be considered 
# important with coefficients between 0.5/3 and 2.5/3 in 0.5/3 unit increments 
# (0.5/3, 1/3, 1.5/3, 2.0/3, and 2.5/3). In case 1a consider all X variables as 
# independent from one another
#
# Load libraries
library(hdrm) # Generating data
library(olsrr) # P-value backwards selection
library(parallel) # Multithreading
library(RcmdrMisc) # BIC selection
library(glmnet) # LASSO

# Simulation parameters
nSim <- 200
N <- 250

# Generate data (used for all model selection processes)
simData <- parallel::mclapply(1:nSim, function(x) {
    dta <- genData(n = N, p = 20, p1 = 5, 
                   beta = c(.5 / 3, 1 / 3, 1.5 / 3, 2 / 3, 2.5 / 3, rep(0, 15)))
    data.frame(y = dta$y, dta$X)
}, mc.cores = 4)

# Backwards selection ----------------------------------------------------------
# Parallel version
# coefEsts <- matrix(0, nrow = nSim, ncol = 20)
# coefEsts <- parallel::mclapply(1:nSim, function(x) {
#     dta <- simData[[x]]
#     initMod <- lm(y ~ ., data = dta)
#     backSel <- ols_step_backward_p(initMod, prem = 0.15)
#     rtn <- numeric(20)
#     rtn[!(backSel$indvar %in% backSel$removed)] <- 
#         backSel$model$coefficients[-1]
#     rtn[(backSel$indvar %in% backSel$removed)] <- NA
#     rtn
# }, mc.cores = 4)

# Loop version
coefEstsBS <- matrix(0, nrow = nSim, ncol = 20)
for (i in 1:nSim) {
    initMod <- lm(y ~ ., data = simData[[i]])
    backSel <- ols_step_backward_p(initMod, prem = 0.15)
    coefEstsBS[i, !(backSel$indvar %in% backSel$removed)] <- 
        backSel$model$coefficients[-1]
    coefEstsBS[i, (backSel$indvar %in% backSel$removed)] <- NA
}

# Calculate true positive rate (for each significant coefficient), false
#   positive rate, and false discovery rate
(truePositiveBS <- c(sum(!is.na(coefEstsBS[, 1])) / nSim,
                     sum(!is.na(coefEstsBS[, 2])) / nSim,
                     sum(!is.na(coefEstsBS[, 3])) / nSim,
                     sum(!is.na(coefEstsBS[, 4])) / nSim,
                     sum(!is.na(coefEstsBS[, 5])) / nSim))
(falsePostiveBS <- sum(!is.na(coefEstsBS[, 6:20])) / (nSim * 15))
(falseDiscoveryBS <- sum(!is.na(coefEstsBS[, 6:20])) / sum(!is.na(coefEstsBS)))

# AIC
# Loop version
coefEstsAIC <- matrix(0, nrow = nSim, ncol = 20)
for (i in 1:nSim) {
    allVars <- colnames(simData[[i]])[-1]
    initMod <- lm(y ~ ., data = simData[[i]])
    backSel <- ols_step_backward_aic(initMod, prem = 0.15)
    coefEstsAIC[i, !(allVars %in% backSel$predictors)] <- 
        backSel$model$coefficients[-1]
    coefEstsAIC[i, (allVars %in% backSel$predictors)] <- NA
}

# Calculate true positive rate (for each significant coefficient), false
#   positive rate, and false discovery rate
(truePositiveAIC <- c(sum(!is.na(coefEstsAIC[, 1])) / nSim,
                     sum(!is.na(coefEstsAIC[, 2])) / nSim,
                     sum(!is.na(coefEstsAIC[, 3])) / nSim,
                     sum(!is.na(coefEstsAIC[, 4])) / nSim,
                     sum(!is.na(coefEstsAIC[, 5])) / nSim))
(falsePostiveAIC <- sum(!is.na(coefEstsAIC[, 6:20])) / (nSim * 15))
(falseDiscoveryAIC <- sum(!is.na(coefEstsAIC[, 6:20])) / 
        sum(!is.na(coefEstsAIC)))

# BIC
# Loop version
coefEstsBIC <- matrix(0, nrow = nSim, ncol = 20)
for (i in 1:nSim) {
    allVars <- colnames(simData[[i]])[-1]
    initMod <- lm(y ~ ., data = simData[[i]])
    backSel <- stepwise(initMod, criterion = "BIC")
    retainedVars <- names(backSel$coefficients)[names(backSel$coefficients) !=
                                                    "(Intercept)"]
    coefEstsBIC[i, (allVars %in% retainedVars)] <- 
        backSel$coefficients[names(backSel$coefficients) != "(Intercept)"]
    coefEstsBIC[i, !(allVars %in% retainedVars)] <- NA
}

# Calculate true positive rate (for each significant coefficient), false
#   positive rate, and false discovery rate
(truePositiveBIC <- c(sum(!is.na(coefEstsBIC[, 1])) / nSim,
                      sum(!is.na(coefEstsBIC[, 2])) / nSim,
                      sum(!is.na(coefEstsBIC[, 3])) / nSim,
                      sum(!is.na(coefEstsBIC[, 4])) / nSim,
                      sum(!is.na(coefEstsBIC[, 5])) / nSim))
(falsePostiveBIC <- sum(!is.na(coefEstsBIC[, 6:20])) / (nSim * 15))
(falseDiscoveryBIC <- sum(!is.na(coefEstsBIC[, 6:20])) / 
        sum(!is.na(coefEstsBIC)))

# LASSO w/ CV
coefEstsLASSOCV <- matrix(0, nrow = nSim, ncol = 20)
for (i in 1:nSim) {
    allVars <- colnames(simData[[i]])[-1]
    x <- model.matrix(~ . - 1, data = simData[[i]][, -1])
    y <- simData[[i]][, 1]
    grid <- 10^seq(10, -2, length = 100)
    modelCV <- cv.glmnet(x = x, y = y, lambda = grid, nfolds = 5)
    retainedVars <- rownames(
        coef(modelCV, modelCV$lambda.min))[coef(modelCV, 
                                                modelCV$lambda.min)[, 1] != 0]
    coefEstsLASSOCV[i, (allVars %in% retainedVars)] <- 
        coef(modelCV, modelCV$lambda.min)[rownames(coef(modelCV, modelCV$lambda.min)) != "(Intercept)" &
                                              coef(modelCV, modelCV$lambda.min)[, 1] != 0]
    coefEstsLASSOCV[i, !(allVars %in% retainedVars)] <- NA
}

(truePositiveLASSOCV <- c(sum(!is.na(coefEstsLASSOCV[, 1])) / nSim,
                      sum(!is.na(coefEstsLASSOCV[, 2])) / nSim,
                      sum(!is.na(coefEstsLASSOCV[, 3])) / nSim,
                      sum(!is.na(coefEstsLASSOCV[, 4])) / nSim,
                      sum(!is.na(coefEstsLASSOCV[, 5])) / nSim))
(falsePostiveLASSOCV <- sum(!is.na(coefEstsLASSOCV[, 6:20])) / (nSim * 15))
(falseDiscoveryLASSOCV <- sum(!is.na(coefEstsLASSOCV[, 6:20])) / 
        sum(!is.na(coefEstsLASSOCV)))

# Lasso w/ fixed lambda
# Elastic net
