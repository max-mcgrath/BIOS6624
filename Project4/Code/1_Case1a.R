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
library(dplyr)

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

# Notes on data structure:
# Columns 1-20 correspond to model coefficients
# Columns 21-25 correspond to significant coefficients and are indicators of
#   whether or not their CI covers the "true" parameter value
# Columns 26-45 correspond to coefficients and are indicators of 
#   whether they're p-value is less than .05 for the final model

# P-value
coefEstsBS <- matrix(0, nrow = nSim, ncol = 45)
for (i in 1:nSim) {
    # Create initial model, perform backwards selection
    initMod <- lm(y ~ ., data = simData[[i]])
    backSel <- ols_step_backward_p(initMod, prem = 0.15)
    
    # Store coefficients for retained predictors
    keptCoefs <- numeric(20)
    keptCoefs[!(backSel$indvar %in% backSel$removed)] <- 
        backSel$model$coefficients[-1]
    keptCoefs[(backSel$indvar %in% backSel$removed)] <- NA
    coefEstsBS[i, 1:20] <- keptCoefs
    
    # Create indicator for CI coverage
    coefEstsBS[i, 21] <- ifelse(is.na(coefEstsBS[i, 1]), NA,
                                between(1/6, confint(backSel$model, "V01")[1],
                                        confint(backSel$model, "V01")[2]))
    coefEstsBS[i, 22] <- ifelse(is.na(coefEstsBS[i, 2]), NA,
                                between(1/3, confint(backSel$model, "V02")[1],
                                        confint(backSel$model, "V02")[2]))
    coefEstsBS[i, 23] <- ifelse(is.na(coefEstsBS[i, 3]), NA,
                                between(1/2, confint(backSel$model, "V03")[1],
                                        confint(backSel$model, "V03")[2]))
    coefEstsBS[i, 24] <- ifelse(is.na(coefEstsBS[i, 4]), NA,
                                between(2/3, confint(backSel$model, "V04")[1],
                                        confint(backSel$model, "V04")[2]))
    coefEstsBS[i, 25] <- ifelse(is.na(coefEstsBS[i, 5]), NA,
                                between(5/6, confint(backSel$model, "V05")[1],
                                        confint(backSel$model, "V05")[2]))
    
    # Indicators for which coefficients are significant
    allVars <- colnames(simData[[i]])[-1]
    sigCoefs <- names(which(summary(backSel$model)$coefficients[, 4] < 0.05))
    coefEstsBS[i, 26:45] <- ifelse(is.na(coefEstsBS[i, 1:20]), NA, 
                                   as.numeric(allVars %in% sigCoefs))
}

# AIC
coefEstsAIC <- matrix(0, nrow = nSim, ncol = 45)
for (i in 1:nSim) {
    initMod <- lm(y ~ ., data = simData[[i]])
    backSel <- stepwise(initMod, criterion = "AIC", direction = "backward")
    
    # Store coefficient estimates for retained variables
    allVars <- colnames(simData[[i]])[-1]
    keptCoefs <- numeric(20)
    retainedVars <- names(backSel$coefficients)[names(backSel$coefficients) !=
                                                    "(Intercept)"]
    keptCoefs[(allVars %in% retainedVars)] <- 
        backSel$coefficients[names(backSel$coefficients) != "(Intercept)"]
    keptCoefs[!(allVars %in% retainedVars)] <- NA
    coefEstsAIC[i, 1:20] <- keptCoefs
    
    # Create indicator for CI coverage
    coefEstsAIC[i, 21] <- ifelse(is.na(coefEstsBS[i, 1]), NA,
                                 between(1/6, confint(backSel, "V01")[1],
                                         confint(backSel, "V01")[2]))
    coefEstsAIC[i, 22] <- ifelse(is.na(coefEstsBS[i, 2]), NA,
                                 between(1/3, confint(backSel, "V02")[1],
                                         confint(backSel, "V02")[2]))
    coefEstsAIC[i, 23] <- ifelse(is.na(coefEstsBS[i, 3]), NA,
                                 between(1/2, confint(backSel, "V03")[1],
                                         confint(backSel, "V03")[2]))
    coefEstsAIC[i, 24] <- ifelse(is.na(coefEstsBS[i, 4]), NA,
                                 between(2/3, confint(backSel, "V04")[1],
                                         confint(backSel, "V04")[2]))
    coefEstsAIC[i, 25] <- ifelse(is.na(coefEstsBS[i, 5]), NA,
                                 between(5/6, confint(backSel, "V05")[1],
                                         confint(backSel, "V05")[2]))
    
    # Indicators for which coefficients are significant
    sigCoefs <- names(which(summary(backSel)$coefficients[, 4] < 0.05))
    coefEstsAIC[i, 26:45] <- ifelse(is.na(coefEstsBS[i, 1:20]), NA, 
                                    as.numeric(allVars %in% sigCoefs))
}

# BIC
coefEstsBIC <- matrix(0, nrow = nSim, ncol = 45)
for (i in 1:nSim) {
    initMod <- lm(y ~ ., data = simData[[i]])
    backSel <- stepwise(initMod, criterion = "BIC", direction = "backward")
    
    # Store coefficient estimates for retained variables
    allVars <- colnames(simData[[i]])[-1]
    keptCoefs <- numeric(20)
    retainedVars <- names(backSel$coefficients)[names(backSel$coefficients) !=
                                                    "(Intercept)"]
    keptCoefs[(allVars %in% retainedVars)] <- 
        backSel$coefficients[names(backSel$coefficients) != "(Intercept)"]
    keptCoefs[!(allVars %in% retainedVars)] <- NA
    coefEstsBIC[i, 1:20] <- keptCoefs
    
    # Create indicator for CI coverage
    coefEstsBIC[i, 21] <- ifelse(is.na(coefEstsBS[i, 1]), NA,
                                between(1/6, confint(backSel, "V01")[1],
                                        confint(backSel, "V01")[2]))
    coefEstsBIC[i, 22] <- ifelse(is.na(coefEstsBS[i, 2]), NA,
                                between(1/3, confint(backSel, "V02")[1],
                                        confint(backSel, "V02")[2]))
    coefEstsBIC[i, 23] <- ifelse(is.na(coefEstsBS[i, 3]), NA,
                                between(1/2, confint(backSel, "V03")[1],
                                        confint(backSel, "V03")[2]))
    coefEstsBIC[i, 24] <- ifelse(is.na(coefEstsBS[i, 4]), NA,
                                between(2/3, confint(backSel, "V04")[1],
                                        confint(backSel, "V04")[2]))
    coefEstsBIC[i, 25] <- ifelse(is.na(coefEstsBS[i, 5]), NA,
                                between(5/6, confint(backSel, "V05")[1],
                                        confint(backSel, "V05")[2]))
    
    # Indicators for which coefficients are significant
    sigCoefs <- names(which(summary(backSel)$coefficients[, 4] < 0.05))
    coefEstsBIC[i, 26:45] <- ifelse(is.na(coefEstsBS[i, 1:20]), NA, 
                                    as.numeric(allVars %in% sigCoefs))
    
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
        as.matrix(coef(modelCV, modelCV$lambda.min))[rownames(coef(modelCV, modelCV$lambda.min)) != "(Intercept)" &
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
    modelFIX2 <- glmnet(x = x, y = y, lambda = 0.2)
    modelFIX2$beta
    retainedVars <- rownames(
        coef(modelFIX, .2))[coef(modelFIX, .2)[, 1] != 0]
    coefEstsENFIX[i, (allVars %in% retainedVars)] <- 
        coef(modelFIX, .2)[rownames(coef(modelFIX, .2)) != "(Intercept)" &
                                              coef(modelFIX, .2)[, 1] != 0]
    coefEstsENFIX[i, !(allVars %in% retainedVars)] <- NA
}

# Write relevant objects to files ----------------------------------------------
# saveRDS(nSim, "DataRaw/nSim.rda")
# saveRDS(coefEstsBS, "DataRaw/coefEstsBS1a.rda")
# saveRDS(coefEstsAIC, "DataRaw/coefEstsAIC1a.rda")
# saveRDS(coefEstsBIC, "DataRaw/coefEstsBIC1a.rda")
# saveRDS(coefEstsLASSOCV, "DataRaw/coefEstsLASSOCV1a.rda")
# saveRDS(coefEstsLASSOFIX, "DataRaw/coefEstsLASSOFIX1a.rda")
# saveRDS(coefEstsENCV, "DataRaw/coefEstsENCV1a.rda")
# saveRDS(coefEstsENFIX, "DataRaw/coefEstsENFIX1a.rda")