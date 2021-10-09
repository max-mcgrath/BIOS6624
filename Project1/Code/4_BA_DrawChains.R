source("Code/1_prepData.R")

library(readr)
library(rjags)
library(mcmcse)

# Create JAGS Variables --------------------------------------------------------

# General RJAGS variables (used for all outcomes)
# Number of observations (same for all models)
N <- nrow(cleanData)

# Number of predictors (differs by model but not outcome)
pUniNoDrugs <- 2
pUni <- pUniNoDrugs + 1
pMultiNoDrugs <- 11
pMulti <- pMultiNoDrugs + 1

# Error priors (same for all models)
a <- 0.001 # gamma shape for a non-informative prior
b <- 0.001 # gamma rate for a non-informative prior

# Coefficient priors (differ by model but not outcome)
priorPrecision <- .000001
mUniNoDrugs <- rep(0, pUniNoDrugs)
RUniNoDrugs <- matrix(0, pUniNoDrugs, pUniNoDrugs) 
diag(RUniNoDrugs) <- priorPrecision 

mUni <- rep(0, pUni)
RUni <- matrix(0, pUni, pUni)
diag(RUni) <- priorPrecision

mMultiNoDrugs <- rep(0, pMultiNoDrugs)
RMultiNoDrugs <- matrix(0, pMultiNoDrugs, pMultiNoDrugs)
diag(RMultiNoDrugs) <- priorPrecision

mMulti <- rep(0, pMulti)
RMulti <- matrix(0, pMulti, pMulti)
diag(RMulti) <- priorPrecision

# Outcomes
logVloadDiff <- vloadData$LOG_VLOAD_DIFF
leu3nDiff <- leu3nData$LEU3N_DIFF
mentDiff <- mentData$MENT_DIFF
physDiff <- physData$PHYS_DIFF

# VLOAD Model Matrices
vloadUniNoDrugsMM <- model.matrix(LOG_VLOAD_DIFF ~ LOG_VLOAD_0, 
                                  data = vloadData)
vloadUniMM <- model.matrix(LOG_VLOAD_DIFF ~ DRUGS_0 + LOG_VLOAD_0, 
                           data = vloadData)
vloadMultiNoDrugsMM <- model.matrix(LOG_VLOAD_DIFF ~ . -DRUGS_0,
                                    data = vloadData)
vloadMultiMM <- model.matrix(LOG_VLOAD_DIFF ~ DRUGS_0 + .,
                                    data = vloadData)

# LEU3N Model Matrices
leu3nUniNoDrugsMM <- model.matrix(LEU3N_DIFF ~ LEU3N_0, 
                                  data = leu3nData)
leu3nUniMM <- model.matrix(LEU3N_DIFF ~ DRUGS_0 + LEU3N_0, 
                           data = leu3nData)
leu3nMultiNoDrugsMM <- model.matrix(LEU3N_DIFF ~ . -DRUGS_0,
                                    data = leu3nData)
leu3nMultiMM <- model.matrix(LEU3N_DIFF ~ DRUGS_0 + .,
                             data = leu3nData)

# MENT Model Matrices
mentUniNoDrugsMM <- model.matrix(MENT_DIFF ~ MENT_0, 
                                  data = mentData)
mentUniMM <- model.matrix(MENT_DIFF ~ DRUGS_0 + MENT_0, 
                           data = mentData)
mentMultiNoDrugsMM <- model.matrix(MENT_DIFF ~ . -DRUGS_0,
                                    data = mentData)
mentMultiMM <- model.matrix(MENT_DIFF ~ DRUGS_0 + .,
                             data = mentData)

# PHYS Model Matrices
physUniNoDrugsMM <- model.matrix(PHYS_DIFF ~ PHYS_0, 
                                  data = physData)
physUniMM <- model.matrix(PHYS_DIFF ~ DRUGS_0 + PHYS_0, 
                           data = physData)
physMultiNoDrugsMM <- model.matrix(PHYS_DIFF ~ . -DRUGS_0,
                                    data = physData)
physMultiMM <- model.matrix(PHYS_DIFF ~ DRUGS_0 + .,
                             data = physData)

# Create JAGS data lists
# VLOAD
vloadUniNoDrugsDat <- list(y = logVloadDiff, X = vloadUniNoDrugsMM, N = N, 
                           p = pUniNoDrugs, m = mUniNoDrugs, R = RUniNoDrugs,
                           a = a, b = b)
vloadUniDat <- list(y = logVloadDiff, X = vloadUniMM, N = N, 
                           p = pUni, m = mUni, R = RUni,
                           a = a, b = b)
vloadMultiNoDrugsDat <- list(y = logVloadDiff, X = vloadMultiNoDrugsMM, N = N, 
                           p = pMultiNoDrugs, m = mMultiNoDrugs, 
                           R = RMultiNoDrugs,
                           a = a, b = b)
vloadMultiDat <- list(y = logVloadDiff, X = vloadMultiMM, N = N, 
                           p = pMulti, m = mMulti, R = RMulti,
                           a = a, b = b)

# LEU3N
leu3nUniNoDrugsDat <- list(y = leu3nDiff, X = leu3nUniNoDrugsMM, N = N, 
                           p = pUniNoDrugs, m = mUniNoDrugs, R = RUniNoDrugs,
                           a = a, b = b)
leu3nUniDat <- list(y = leu3nDiff, X = leu3nUniMM, N = N, 
                    p = pUni, m = mUni, R = RUni,
                    a = a, b = b)
leu3nMultiNoDrugsDat <- list(y = leu3nDiff, X = leu3nMultiNoDrugsMM, N = N, 
                            p = pMultiNoDrugs, m = mMultiNoDrugs, 
                            R = RMultiNoDrugs,
                            a = a, b = b)
leu3nMultiDat <- list(y = leu3nDiff, X = leu3nMultiMM, N = N, 
                      p = pMulti, m = mMulti, R = RMulti,
                      a = a, b = b)

# MENT
mentUniNoDrugsDat <- list(y = mentDiff, X = mentUniNoDrugsMM, N = N, 
                           p = pUniNoDrugs, m = mUniNoDrugs, R = RUniNoDrugs,
                           a = a, b = b)
mentUniDat <- list(y = mentDiff, X = mentUniMM, N = N, 
                    p = pUni, m = mUni, R = RUni,
                    a = a, b = b)
mentMultiNoDrugsDat <- list(y = mentDiff, X = mentMultiNoDrugsMM, N = N, 
                            p = pMultiNoDrugs, m = mMultiNoDrugs, 
                           R = RMultiNoDrugs,
                            a = a, b = b)
mentMultiDat <- list(y = mentDiff, X = mentMultiMM, N = N, 
                      p = pMulti, m = mMulti, R = RMulti,
                      a = a, b = b)

# PHYS
physUniNoDrugsDat <- list(y = physDiff, X = physUniNoDrugsMM, N = N, 
                           p = pUniNoDrugs, m = mUniNoDrugs, R = RUniNoDrugs,
                           a = a, b = b)
physUniDat <- list(y = physDiff, X = physUniMM, N = N, 
                    p = pUni, m = mUni, R = RUni,
                    a = a, b = b)
physMultiNoDrugsDat <- list(y = physDiff, X = physMultiNoDrugsMM, N = N, 
                            p = pMultiNoDrugs, m = mMultiNoDrugs, 
                           R = RMultiNoDrugs,
                            a = a, b = b)
physMultiDat <- list(y = physDiff, X = physMultiMM, N = N, 
                      p = pMulti, m = mMulti, R = RMulti,
                      a = a, b = b)


# Run Chains -------------------------------------------------------------------
set.seed(123)
iter <- 25000

# VLOAD
vloadUniNoDrugsMod <- jags.model("Code/linMod.jags", 
                                 data = vloadUniNoDrugsDat, 
                                 n.adapt = 1000, n.chains = 2)
vloadUniNoDrugsChains <- coda.samples(vloadUniNoDrugsMod, 
                                      variable.names = c("beta", "sigma2"),
                                      n.iter = iter)
vloadUniNoDrugsDIC <- dic.samples(vloadUniNoDrugsMod, 
                                  n.iter = iter, type = "pD")

vloadUniMod <- jags.model("Code/linMod.jags", 
                                 data = vloadUniDat, 
                                 n.adapt = 1000, n.chains = 2)
vloadUniChains <- coda.samples(vloadUniMod, 
                                      variable.names = c("beta", "sigma2"),
                                      n.iter = iter)
vloadUniDIC <- dic.samples(vloadUniMod, 
                                  n.iter = iter, type = "pD")

vloadMultiNoDrugsMod <- jags.model("Code/linMod.jags", 
                                 data = vloadMultiNoDrugsDat, 
                                 n.adapt = 1000, n.chains = 2)
vloadMultiNoDrugsChains <- coda.samples(vloadMultiNoDrugsMod, 
                                      variable.names = c("beta", "sigma2"),
                                      n.iter = iter)
vloadMultiNoDrugsDIC <- dic.samples(vloadMultiNoDrugsMod, 
                                  n.iter = iter, type = "pD")

vloadMultiMod <- jags.model("Code/linMod.jags", 
                                 data = vloadMultiDat, 
                                 n.adapt = 1000, n.chains = 2)
vloadMultiChains <- coda.samples(vloadMultiMod, 
                                      variable.names = c("beta", "sigma2"),
                                      n.iter = iter)
vloadMultiDIC <- dic.samples(vloadMultiMod, 
                                  n.iter = iter, type = "pD")


# LEU3N
leu3nUniNoDrugsMod <- jags.model("Code/linMod.jags", 
                                 data = leu3nUniNoDrugsDat, 
                                 n.adapt = 1000, n.chains = 2)
leu3nUniNoDrugsChains <- coda.samples(leu3nUniNoDrugsMod, 
                                      variable.names = c("beta", "sigma2"),
                                      n.iter = iter)
leu3nUniNoDrugsDIC <- dic.samples(leu3nUniNoDrugsMod, 
                                  n.iter = iter, type = "pD")

leu3nUniMod <- jags.model("Code/linMod.jags", 
                          data = leu3nUniDat, 
                          n.adapt = 1000, n.chains = 2)
leu3nUniChains <- coda.samples(leu3nUniMod, 
                               variable.names = c("beta", "sigma2"),
                               n.iter = iter)
leu3nUniDIC <- dic.samples(leu3nUniMod, 
                           n.iter = iter, type = "pD")

leu3nMultiNoDrugsMod <- jags.model("Code/linMod.jags", 
                                   data = leu3nMultiNoDrugsDat, 
                                   n.adapt = 1000, n.chains = 2)
leu3nMultiNoDrugsChains <- coda.samples(leu3nMultiNoDrugsMod, 
                                        variable.names = c("beta", "sigma2"),
                                        n.iter = iter)
leu3nMultiNoDrugsDIC <- dic.samples(leu3nMultiNoDrugsMod, 
                                    n.iter = iter, type = "pD")

leu3nMultiMod <- jags.model("Code/linMod.jags", 
                            data = leu3nMultiDat, 
                            n.adapt = 1000, n.chains = 2)
leu3nMultiChains <- coda.samples(leu3nMultiMod, 
                                 variable.names = c("beta", "sigma2"),
                                 n.iter = iter)
leu3nMultiDIC <- dic.samples(leu3nMultiMod, 
                             n.iter = iter, type = "pD")

# MENT
mentUniNoDrugsMod <- jags.model("Code/linMod.jags", 
                                 data = mentUniNoDrugsDat, 
                                 n.adapt = 1000, n.chains = 2)
mentUniNoDrugsChains <- coda.samples(mentUniNoDrugsMod, 
                                      variable.names = c("beta", "sigma2"),
                                      n.iter = iter)
mentUniNoDrugsDIC <- dic.samples(mentUniNoDrugsMod, 
                                  n.iter = iter, type = "pD")

mentUniMod <- jags.model("Code/linMod.jags", 
                          data = mentUniDat, 
                          n.adapt = 1000, n.chains = 2)
mentUniChains <- coda.samples(mentUniMod, 
                               variable.names = c("beta", "sigma2"),
                               n.iter = iter)
mentUniDIC <- dic.samples(mentUniMod, 
                           n.iter = iter, type = "pD")

mentMultiNoDrugsMod <- jags.model("Code/linMod.jags", 
                                   data = mentMultiNoDrugsDat, 
                                   n.adapt = 1000, n.chains = 2)
mentMultiNoDrugsChains <- coda.samples(mentMultiNoDrugsMod, 
                                        variable.names = c("beta", "sigma2"),
                                        n.iter = iter)
mentMultiNoDrugsDIC <- dic.samples(mentMultiNoDrugsMod, 
                                    n.iter = iter, type = "pD")

mentMultiMod <- jags.model("Code/linMod.jags", 
                            data = mentMultiDat, 
                            n.adapt = 1000, n.chains = 2)
mentMultiChains <- coda.samples(mentMultiMod, 
                                 variable.names = c("beta", "sigma2"),
                                 n.iter = iter)
mentMultiDIC <- dic.samples(mentMultiMod, 
                             n.iter = iter, type = "pD")

# PHYS
physUniNoDrugsMod <- jags.model("Code/linMod.jags", 
                                data = physUniNoDrugsDat, 
                                n.adapt = 1000, n.chains = 2)
physUniNoDrugsChains <- coda.samples(physUniNoDrugsMod, 
                                     variable.names = c("beta", "sigma2"),
                                     n.iter = iter)
physUniNoDrugsDIC <- dic.samples(physUniNoDrugsMod, 
                                 n.iter = iter, type = "pD")

physUniMod <- jags.model("Code/linMod.jags", 
                         data = physUniDat, 
                         n.adapt = 1000, n.chains = 2)
physUniChains <- coda.samples(physUniMod, 
                              variable.names = c("beta", "sigma2"),
                              n.iter = iter)
physUniDIC <- dic.samples(physUniMod, 
                          n.iter = iter, type = "pD")

physMultiNoDrugsMod <- jags.model("Code/linMod.jags", 
                                  data = physMultiNoDrugsDat, 
                                  n.adapt = 1000, n.chains = 2)
physMultiNoDrugsChains <- coda.samples(physMultiNoDrugsMod, 
                                       variable.names = c("beta", "sigma2"),
                                       n.iter = iter)
physMultiNoDrugsDIC <- dic.samples(physMultiNoDrugsMod, 
                                   n.iter = iter, type = "pD")

physMultiMod <- jags.model("Code/linMod.jags", 
                           data = physMultiDat, 
                           n.adapt = 1000, n.chains = 2)
physMultiChains <- coda.samples(physMultiMod, 
                                variable.names = c("beta", "sigma2"),
                                n.iter = iter)
physMultiDIC <- dic.samples(physMultiMod, 
                            n.iter = iter, type = "pD")


# Convert chains to data frames, add parameter names ---------------------------
# VLOAD
vloadUniNoDrugsDF <- bind_rows(as.data.frame(vloadUniNoDrugsChains[[1]]), 
                               as.data.frame(vloadUniNoDrugsChains[[2]]))
colnames(vloadUniNoDrugsDF) <- c(colnames(vloadUniNoDrugsMM), "sigma")

vloadUniDF <- bind_rows(as.data.frame(vloadUniChains[[1]]), 
                        as.data.frame(vloadUniChains[[2]]))
colnames(vloadUniDF) <- c(colnames(vloadUniMM), "sigma")

vloadMultiNoDrugsDF <- bind_rows(as.data.frame(vloadMultiNoDrugsChains[[1]]), 
                                 as.data.frame(vloadMultiNoDrugsChains[[2]]))
colnames(vloadMultiNoDrugsDF) <- c(colnames(vloadMultiNoDrugsMM), "sigma")

vloadMultiDF <- bind_rows(as.data.frame(vloadMultiChains[[1]]), 
                          as.data.frame(vloadMultiChains[[2]]))
colnames(vloadMultiDF) <- c(colnames(vloadMultiMM), "sigma")

# LEU3N
leu3nUniNoDrugsDF <- bind_rows(as.data.frame(leu3nUniNoDrugsChains[[1]]), 
                              as.data.frame(leu3nUniNoDrugsChains[[2]]))
colnames(leu3nUniNoDrugsDF) <- c(colnames(leu3nUniNoDrugsMM), "sigma")

leu3nUniDF <- bind_rows(as.data.frame(leu3nUniChains[[1]]), 
                       as.data.frame(leu3nUniChains[[2]]))
colnames(leu3nUniDF) <- c(colnames(leu3nUniMM), "sigma")

leu3nMultiNoDrugsDF <- bind_rows(as.data.frame(leu3nMultiNoDrugsChains[[1]]), 
                                as.data.frame(leu3nMultiNoDrugsChains[[2]]))
colnames(leu3nMultiNoDrugsDF) <- c(colnames(leu3nMultiNoDrugsMM), "sigma")

leu3nMultiDF <- bind_rows(as.data.frame(leu3nMultiChains[[1]]), 
                         as.data.frame(leu3nMultiChains[[2]]))
colnames(leu3nMultiDF) <- c(colnames(leu3nMultiMM), "sigma")

# MENT
mentUniNoDrugsDF <- bind_rows(as.data.frame(mentUniNoDrugsChains[[1]]), 
                              as.data.frame(mentUniNoDrugsChains[[2]]))
colnames(mentUniNoDrugsDF) <- c(colnames(mentUniNoDrugsMM), "sigma")

mentUniDF <- bind_rows(as.data.frame(mentUniChains[[1]]), 
                       as.data.frame(mentUniChains[[2]]))
colnames(mentUniDF) <- c(colnames(mentUniMM), "sigma")

mentMultiNoDrugsDF <- bind_rows(as.data.frame(mentMultiNoDrugsChains[[1]]), 
                                as.data.frame(mentMultiNoDrugsChains[[2]]))
colnames(mentMultiNoDrugsDF) <- c(colnames(mentMultiNoDrugsMM), "sigma")

mentMultiDF <- bind_rows(as.data.frame(mentMultiChains[[1]]), 
                         as.data.frame(mentMultiChains[[2]]))
colnames(mentMultiDF) <- c(colnames(mentMultiMM), "sigma")

# PHYS
physUniNoDrugsDF <- bind_rows(as.data.frame(physUniNoDrugsChains[[1]]), 
                              as.data.frame(physUniNoDrugsChains[[2]]))
colnames(physUniNoDrugsDF) <- c(colnames(physUniNoDrugsMM), "sigma")

physUniDF <- bind_rows(as.data.frame(physUniChains[[1]]), 
                              as.data.frame(physUniChains[[2]]))
colnames(physUniDF) <- c(colnames(physUniMM), "sigma")

physMultiNoDrugsDF <- bind_rows(as.data.frame(physMultiNoDrugsChains[[1]]), 
                       as.data.frame(physMultiNoDrugsChains[[2]]))
colnames(physMultiNoDrugsDF) <- c(colnames(physMultiNoDrugsMM), "sigma")

physMultiDF <- bind_rows(as.data.frame(physMultiChains[[1]]), 
                                as.data.frame(physMultiChains[[2]]))
colnames(physMultiDF) <- c(colnames(physMultiMM), "sigma")
