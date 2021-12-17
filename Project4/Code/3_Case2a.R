# Case Scenario 2a
#
# N=500 subjects with 20 variables in the model. 5 of them would be considered 
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
library(stringr)

# Simulation parameters
nSim <- 1000
N <- 500

# Set seed
set.seed(125)

# Generate data (used for all model selection processes)
simData <- parallel::mclapply(1:nSim, function(x) {
    dta <- genData(n = N, p = 20, p1 = 5, 
                   beta = c(.5 / 3, 1 / 3, 1.5 / 3, 2 / 3, 2.5 / 3, rep(0, 15)))
    data.frame(y = dta$y, dta$X)
}, mc.cores = 4)

# Backwards selection ----------------------------------------------------------
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
coefEstsLASSOCV <- matrix(0, nrow = nSim, ncol = 45)
for (i in 1:nSim) {
    # Run initial model
    x <- model.matrix(~ . - 1, data = simData[[i]][, -1])
    y <- simData[[i]][, 1]
    grid <- 10^seq(10, -2, length = 100)
    modelCV <- cv.glmnet(x = x, y = y, lambda = grid, nfolds = 5)
    
    # Get coefficient estimates
    allVars <- colnames(simData[[i]])[-1]
    keptCoefs <- numeric(20)
    retainedVars <- rownames(
        coef(modelCV, modelCV$lambda.min))[coef(modelCV, 
                                                modelCV$lambda.min)[, 1] != 0]
    keptCoefs[(allVars %in% retainedVars)] <- 
        as.matrix(coef(modelCV, modelCV$lambda.min))[rownames(coef(modelCV, modelCV$lambda.min)) != "(Intercept)" &
                                                         coef(modelCV, modelCV$lambda.min)[, 1] != 0]
    keptCoefs[!(allVars %in% retainedVars)] <- NA
    coefEstsLASSOCV[i, 1:20] <- keptCoefs
    
    # Fit OLS model with retained variables
    formulaString <- paste0("y ~ ", paste0(retainedVars, collapse = " + "))
    formulaString <- str_replace(formulaString, "\\(Intercept\\) \\+ ", "")
    formula <- formula(formulaString)
    olsFit <- lm(formula, data = simData[[i]])
    
    # Create indicator for CI coverage
    coefEstsLASSOCV[i, 21] <- ifelse(is.na(coefEstsLASSOCV[i, 1]), NA,
                                     between(1/6, confint(olsFit, "V01")[1],
                                             confint(olsFit, "V01")[2]))
    coefEstsLASSOCV[i, 22] <- ifelse(is.na(coefEstsLASSOCV[i, 2]), NA,
                                     between(1/3, confint(olsFit, "V02")[1],
                                             confint(olsFit, "V02")[2]))
    coefEstsLASSOCV[i, 23] <- ifelse(is.na(coefEstsLASSOCV[i, 3]), NA,
                                     between(1/2, confint(olsFit, "V03")[1],
                                             confint(olsFit, "V03")[2]))
    coefEstsLASSOCV[i, 24] <- ifelse(is.na(coefEstsLASSOCV[i, 4]), NA,
                                     between(2/3, confint(olsFit, "V04")[1],
                                             confint(olsFit, "V04")[2]))
    coefEstsLASSOCV[i, 25] <- ifelse(is.na(coefEstsLASSOCV[i, 5]), NA,
                                     between(5/6, confint(olsFit, "V05")[1],
                                             confint(olsFit, "V05")[2]))
    
    # Indicators for which coefficients are significant
    sigCoefs <- names(which(summary(olsFit)$coefficients[, 4] < 0.05))
    coefEstsLASSOCV[i, 26:45] <- ifelse(is.na(coefEstsLASSOCV[i, 1:20]), NA, 
                                        as.numeric(allVars %in% sigCoefs))
    
    # Store coefficient estimates from OLS Fit
    keptCoefs <- numeric(20)
    retainedVars <- names(olsFit$coefficients)[names(olsFit$coefficients) !=
                                                   "(Intercept)"]
    keptCoefs[(allVars %in% retainedVars)] <- 
        olsFit$coefficients[names(olsFit$coefficients) != "(Intercept)"]
    keptCoefs[!(allVars %in% retainedVars)] <- NA
    coefEstsLASSOCV[i, 1:20] <- keptCoefs
    
    
    
}

# Lasso w/ fixed lambda
coefEstsLASSOFIX <- matrix(0, nrow = nSim, ncol = 45)
for (i in 1:nSim) {
    # Run initial model
    x <- model.matrix(~ . - 1, data = simData[[i]][, -1])
    y <- simData[[i]][, 1]
    grid <- 10^seq(10, -2, length = 100)
    modelCV <- glmnet(x = x, y = y, lambda = grid)
    
    # Get coefficient estimates
    allVars <- colnames(simData[[i]])[-1]
    keptCoefs <- numeric(20)
    retainedVars <- rownames(
        coef(modelCV, .2))[coef(modelCV, .2)[, 1] != 0]
    keptCoefs[(allVars %in% retainedVars)] <- 
        as.matrix(coef(modelCV, .2))[rownames(coef(modelCV, .2)) != "(Intercept)" &
                                         coef(modelCV, .2)[, 1] != 0]
    keptCoefs[!(allVars %in% retainedVars)] <- NA
    coefEstsLASSOFIX[i, 1:20] <- keptCoefs
    
    # Fit OLS model with retained variables
    formulaString <- paste0("y ~ ", paste0(retainedVars, collapse = " + "))
    formulaString <- str_replace(formulaString, "\\(Intercept\\) \\+ ", "")
    formula <- formula(formulaString)
    olsFit <- lm(formula, data = simData[[i]])
    
    # Create indicator for CI coverage
    coefEstsLASSOFIX[i, 21] <- ifelse(is.na(coefEstsLASSOFIX[i, 1]), NA,
                                      between(1/6, confint(olsFit, "V01")[1],
                                              confint(olsFit, "V01")[2]))
    coefEstsLASSOFIX[i, 22] <- ifelse(is.na(coefEstsLASSOFIX[i, 2]), NA,
                                      between(1/3, confint(olsFit, "V02")[1],
                                              confint(olsFit, "V02")[2]))
    coefEstsLASSOFIX[i, 23] <- ifelse(is.na(coefEstsLASSOFIX[i, 3]), NA,
                                      between(1/2, confint(olsFit, "V03")[1],
                                              confint(olsFit, "V03")[2]))
    coefEstsLASSOFIX[i, 24] <- ifelse(is.na(coefEstsLASSOFIX[i, 4]), NA,
                                      between(2/3, confint(olsFit, "V04")[1],
                                              confint(olsFit, "V04")[2]))
    coefEstsLASSOFIX[i, 25] <- ifelse(is.na(coefEstsLASSOFIX[i, 5]), NA,
                                      between(5/6, confint(olsFit, "V05")[1],
                                              confint(olsFit, "V05")[2]))
    
    # Indicators for which coefficients are significant
    sigCoefs <- names(which(summary(olsFit)$coefficients[, 4] < 0.05))
    coefEstsLASSOFIX[i, 26:45] <- ifelse(is.na(coefEstsLASSOFIX[i, 1:20]), NA, 
                                         as.numeric(allVars %in% sigCoefs))
    
    # Store coefficient estimates from OLS Fit
    keptCoefs <- numeric(20)
    retainedVars <- names(olsFit$coefficients)[names(olsFit$coefficients) !=
                                                   "(Intercept)"]
    keptCoefs[(allVars %in% retainedVars)] <- 
        olsFit$coefficients[names(olsFit$coefficients) != "(Intercept)"]
    keptCoefs[!(allVars %in% retainedVars)] <- NA
    coefEstsLASSOFIX[i, 1:20] <- keptCoefs
    
}

# Elastic net w/ CV
coefEstsENCV <- matrix(0, nrow = nSim, ncol = 45)
for (i in 1:nSim) {
    # Run initial model
    x <- model.matrix(~ . - 1, data = simData[[i]][, -1])
    y <- simData[[i]][, 1]
    grid <- 10^seq(10, -2, length = 100)
    modelCV <- cv.glmnet(x = x, y = y, lambda = grid, nfolds = 5, alpha = .5)
    
    # Get coefficient estimates
    allVars <- colnames(simData[[i]])[-1]
    # keptCoefs <- numeric(20)
    retainedVars <- rownames(
        coef(modelCV, modelCV$lambda.min))[coef(modelCV,
                                                modelCV$lambda.min)[, 1] != 0]
    # keptCoefs[(allVars %in% retainedVars)] <- 
    #     as.matrix(coef(modelCV, modelCV$lambda.min))[rownames(coef(modelCV, modelCV$lambda.min)) != "(Intercept)" &
    #                                                      coef(modelCV, modelCV$lambda.min)[, 1] != 0]
    # keptCoefs[!(allVars %in% retainedVars)] <- NA
    # coefEstsENCV[i, 1:20] <- keptCoefs
    
    # Fit OLS model with retained variables
    formulaString <- paste0("y ~ ", paste0(retainedVars, collapse = " + "))
    formulaString <- str_replace(formulaString, "\\(Intercept\\) \\+ ", "")
    formula <- formula(formulaString)
    olsFit <- lm(formula, data = simData[[i]])
    
    # Create indicator for CI coverage
    coefEstsENCV[i, 21] <- ifelse(is.na(coefEstsENCV[i, 1]), NA,
                                  between(1/6, confint(olsFit, "V01")[1],
                                          confint(olsFit, "V01")[2]))
    coefEstsENCV[i, 22] <- ifelse(is.na(coefEstsENCV[i, 2]), NA,
                                  between(1/3, confint(olsFit, "V02")[1],
                                          confint(olsFit, "V02")[2]))
    coefEstsENCV[i, 23] <- ifelse(is.na(coefEstsENCV[i, 3]), NA,
                                  between(1/2, confint(olsFit, "V03")[1],
                                          confint(olsFit, "V03")[2]))
    coefEstsENCV[i, 24] <- ifelse(is.na(coefEstsENCV[i, 4]), NA,
                                  between(2/3, confint(olsFit, "V04")[1],
                                          confint(olsFit, "V04")[2]))
    coefEstsENCV[i, 25] <- ifelse(is.na(coefEstsENCV[i, 5]), NA,
                                  between(5/6, confint(olsFit, "V05")[1],
                                          confint(olsFit, "V05")[2]))
    
    # Indicators for which coefficients are significant
    sigCoefs <- names(which(summary(olsFit)$coefficients[, 4] < 0.05))
    coefEstsENCV[i, 26:45] <- ifelse(is.na(coefEstsENCV[i, 1:20]), NA, 
                                     as.numeric(allVars %in% sigCoefs))
    
    # Store coefficient estimates from OLS Fit
    keptCoefs <- numeric(20)
    retainedVars <- names(olsFit$coefficients)[names(olsFit$coefficients) !=
                                                   "(Intercept)"]
    keptCoefs[(allVars %in% retainedVars)] <- 
        olsFit$coefficients[names(olsFit$coefficients) != "(Intercept)"]
    keptCoefs[!(allVars %in% retainedVars)] <- NA
    coefEstsENCV[i, 1:20] <- keptCoefs
}

# Elastic net w/ fixed lambda
coefEstsENFIX <- matrix(0, nrow = nSim, ncol = 45)
for (i in 1:nSim) {
    # Run initial model
    x <- model.matrix(~ . - 1, data = simData[[i]][, -1])
    y <- simData[[i]][, 1]
    grid <- 10^seq(10, -2, length = 100)
    modelCV <- glmnet(x = x, y = y, lambda = grid, alpha = .5)
    
    # Get coefficient estimates
    allVars <- colnames(simData[[i]])[-1]
    keptCoefs <- numeric(20)
    retainedVars <- rownames(
        coef(modelCV, .2))[coef(modelCV, .2)[, 1] != 0]
    keptCoefs[(allVars %in% retainedVars)] <- 
        as.matrix(coef(modelCV, .2))[rownames(coef(modelCV, .2)) != "(Intercept)" &
                                         coef(modelCV, .2)[, 1] != 0]
    keptCoefs[!(allVars %in% retainedVars)] <- NA
    coefEstsENFIX[i, 1:20] <- keptCoefs
    
    # Fit OLS model with retained variables
    formulaString <- paste0("y ~ ", paste0(retainedVars, collapse = " + "))
    formulaString <- str_replace(formulaString, "\\(Intercept\\) \\+ ", "")
    formula <- formula(formulaString)
    olsFit <- lm(formula, data = simData[[i]])
    
    # Create indicator for CI coverage
    coefEstsENFIX[i, 21] <- ifelse(is.na(coefEstsENFIX[i, 1]), NA,
                                   between(1/6, confint(olsFit, "V01")[1],
                                           confint(olsFit, "V01")[2]))
    coefEstsENFIX[i, 22] <- ifelse(is.na(coefEstsENFIX[i, 2]), NA,
                                   between(1/3, confint(olsFit, "V02")[1],
                                           confint(olsFit, "V02")[2]))
    coefEstsENFIX[i, 23] <- ifelse(is.na(coefEstsENFIX[i, 3]), NA,
                                   between(1/2, confint(olsFit, "V03")[1],
                                           confint(olsFit, "V03")[2]))
    coefEstsENFIX[i, 24] <- ifelse(is.na(coefEstsENFIX[i, 4]), NA,
                                   between(2/3, confint(olsFit, "V04")[1],
                                           confint(olsFit, "V04")[2]))
    coefEstsENFIX[i, 25] <- ifelse(is.na(coefEstsENFIX[i, 5]), NA,
                                   between(5/6, confint(olsFit, "V05")[1],
                                           confint(olsFit, "V05")[2]))
    
    # Indicators for which coefficients are significant
    sigCoefs <- names(which(summary(olsFit)$coefficients[, 4] < 0.05))
    coefEstsENFIX[i, 26:45] <- ifelse(is.na(coefEstsENFIX[i, 1:20]), NA, 
                                      as.numeric(allVars %in% sigCoefs))
    
    # Store coefficient estimates from OLS Fit
    keptCoefs <- numeric(20)
    retainedVars <- names(olsFit$coefficients)[names(olsFit$coefficients) !=
                                                   "(Intercept)"]
    keptCoefs[(allVars %in% retainedVars)] <- 
        olsFit$coefficients[names(olsFit$coefficients) != "(Intercept)"]
    keptCoefs[!(allVars %in% retainedVars)] <- NA
    coefEstsENFIX[i, 1:20] <- keptCoefs
}

# Write relevant objects to files ----------------------------------------------
saveRDS(nSim, "DataRaw/nSim2a.rda")
saveRDS(coefEstsBS, "DataRaw/coefEstsBS2a.rda")
saveRDS(coefEstsAIC, "DataRaw/coefEstsAIC2a.rda")
saveRDS(coefEstsBIC, "DataRaw/coefEstsBIC2a.rda")
saveRDS(coefEstsLASSOCV, "DataRaw/coefEstsLASSOCV2a.rda")
saveRDS(coefEstsLASSOFIX, "DataRaw/coefEstsLASSOFIX2a.rda")
saveRDS(coefEstsENCV, "DataRaw/coefEstsENCV2a.rda")
saveRDS(coefEstsENFIX, "DataRaw/coefEstsENFIX2a.rda")
