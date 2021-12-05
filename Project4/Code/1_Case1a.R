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
nSim <- 1000
N <- 250

# Set seed
set.seed(123)

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

# P-value
coefEstsBS <- matrix(0, nrow = nSim, ncol = 20)
for (i in 1:nSim) {
    initMod <- lm(y ~ ., data = simData[[i]])
    backSel <- ols_step_backward_p(initMod, prem = 0.15)
    coefEstsBS[i, !(backSel$indvar %in% backSel$removed)] <- 
        backSel$model$coefficients[-1]
    coefEstsBS[i, (backSel$indvar %in% backSel$removed)] <- NA
}

# AIC
coefEstsAIC <- matrix(0, nrow = nSim, ncol = 20)
for (i in 1:nSim) {
    allVars <- colnames(simData[[i]])[-1]
    initMod <- lm(y ~ ., data = simData[[i]])
    backSel <- ols_step_backward_aic(initMod, prem = 0.15)
    coefEstsAIC[i, !(allVars %in% backSel$predictors)] <- 
        backSel$model$coefficients[-1]
    coefEstsAIC[i, (allVars %in% backSel$predictors)] <- NA
}

# BIC
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

# Penalization -----------------------------------------------------------------
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

# Lasso w/ fixed lambda
coefEstsLASSOFIX <- matrix(0, nrow = nSim, ncol = 20)
for (i in 1:nSim) {
    allVars <- colnames(simData[[i]])[-1]
    x <- model.matrix(~ . - 1, data = simData[[i]][, -1])
    y <- simData[[i]][, 1]
    grid <- 10^seq(10, -2, length = 100)
    modelCV <- glmnet(x = x, y = y, lambda = grid)
    retainedVars <- rownames(
        coef(modelCV, .2))[coef(modelCV, .2)[, 1] != 0]
    coefEstsLASSOFIX[i, (allVars %in% retainedVars)] <- 
        coef(modelCV, .2)[rownames(coef(modelCV, .2)) != "(Intercept)" &
                                              coef(modelCV, .2)[, 1] != 0]
    coefEstsLASSOFIX[i, !(allVars %in% retainedVars)] <- NA
}

# Elastic net w/ CV
coefEstsENCV <- matrix(0, nrow = nSim, ncol = 20)
for (i in 1:nSim) {
    allVars <- colnames(simData[[i]])[-1]
    x <- model.matrix(~ . - 1, data = simData[[i]][, -1])
    y <- simData[[i]][, 1]
    grid <- 10^seq(10, -2, length = 100)
    modelCV <- cv.glmnet(x = x, y = y, lambda = grid, nfolds = 5, alpha = .5)
    retainedVars <- rownames(
        coef(modelCV, modelCV$lambda.min))[coef(modelCV, 
                                                modelCV$lambda.min)[, 1] != 0]
    coefEstsENCV[i, (allVars %in% retainedVars)] <- 
        coef(modelCV, modelCV$lambda.min)[rownames(coef(modelCV, modelCV$lambda.min)) != "(Intercept)" &
                                              coef(modelCV, modelCV$lambda.min)[, 1] != 0]
    coefEstsENCV[i, !(allVars %in% retainedVars)] <- NA
}

# Elastic net w/ fixed lambda
coefEstsENFIX <- matrix(0, nrow = nSim, ncol = 20)
for (i in 1:nSim) {
    allVars <- colnames(simData[[i]])[-1]
    x <- model.matrix(~ . - 1, data = simData[[i]][, -1])
    y <- simData[[i]][, 1]
    grid <- 10^seq(10, -2, length = 100)
    modelFIX <- cv.glmnet(x = x, y = y, lambda = grid, nfolds = 5, alpha = .5)
    retainedVars <- rownames(
        coef(modelFIX, .2))[coef(modelFIX, .2)[, 1] != 0]
    coefEstsENFIX[i, (allVars %in% retainedVars)] <- 
        coef(modelFIX, .2)[rownames(coef(modelFIX, .2)) != "(Intercept)" &
                                              coef(modelFIX, .2)[, 1] != 0]
    coefEstsENFIX[i, !(allVars %in% retainedVars)] <- NA
}

# Write relevant objects to files ----------------------------------------------
saveRDS(nSim, "DataRaw/nSim.rda")
saveRDS(coefEstsBS, "DataRaw/coefEstsBS.rda")
saveRDS(coefEstsAIC, "DataRaw/coefEstsAIC.rda")
saveRDS(coefEstsBIC, "DataRaw/coefEstsBIC.rda")
saveRDS(coefEstsLASSOCV, "DataRaw/coefEstsLASSOCV.rda")
saveRDS(coefEstsLASSOFIX, "DataRaw/coefEstsLASSOFIX.rda")
saveRDS(coefEstsENCV, "DataRaw/coefEstsENCV.rda")
saveRDS(coefEstsENFIX, "DataRaw/coefEstsENFIX.rda")