source("Code/4_BA_DrawChains.R")

# Analyze MCMC Convergence, Independence ---------------------------------------

## Trace and posterior density plots
# VLOAD
plot(vloadUniNoDrugsChains)
plot(vloadUniChains)
plot(vloadMultiNoDrugsChains)
plot(vloadMultiChains)

plot(leu3nUniNoDrugsChains)
plot(leu3nUniChains)
plot(leu3nMultiNoDrugsChains)
plot(leu3nMultiChains)

plot(mentUniNoDrugsChains)
plot(mentUniChains)
plot(mentMultiNoDrugsChains)
plot(mentMultiChains)

plot(physUniNoDrugsChains)
plot(physUniChains)
plot(physMultiNoDrugsChains)
plot(physMultiChains)

## ACF Plots
# VLOAD
par(mfrow = c(ceiling(ncol(as.matrix(vloadUniNoDrugsChains))/2), 2), mar = rep(1, 4))
apply(as.matrix(vloadUniNoDrugsChains), 2, acf)
par(mfrow = c(ceiling(ncol(as.matrix(vloadUniChains))/2), 2), mar = rep(1, 4))
apply(as.matrix(vloadUniChains), 2, acf)
par(mfrow = c(ceiling(ncol(as.matrix(vloadMultiNoDrugsChains))/2), 2), mar = rep(1, 4))
apply(as.matrix(vloadMultiNoDrugsChains), 2, acf)
par(mfrow = c(ceiling(ncol(as.matrix(vloadMultiChains))/2), 2), mar = rep(1, 4))
apply(as.matrix(vloadMultiChains), 2, acf)

# LEU3N
par(mfrow = c(ceiling(ncol(as.matrix(leu3nUniNoDrugsChains))/2), 2), mar = rep(1, 4))
apply(as.matrix(leu3nUniNoDrugsChains), 2, acf)
par(mfrow = c(ceiling(ncol(as.matrix(leu3nUniChains))/2), 2), mar = rep(1, 4))
apply(as.matrix(leu3nUniChains), 2, acf)
par(mfrow = c(ceiling(ncol(as.matrix(leu3nMultiNoDrugsChains))/2), 2), mar = rep(1, 4))
apply(as.matrix(leu3nMultiNoDrugsChains), 2, acf)
par(mfrow = c(ceiling(ncol(as.matrix(leu3nMultiChains))/2), 2), mar = rep(1, 4))
apply(as.matrix(leu3nMultiChains), 2, acf)

# MENT
par(mfrow = c(ceiling(ncol(as.matrix(mentUniNoDrugsChains))/2), 2), mar = rep(1, 4))
apply(as.matrix(mentUniNoDrugsChains), 2, acf)
par(mfrow = c(ceiling(ncol(as.matrix(mentUniChains))/2), 2), mar = rep(1, 4))
apply(as.matrix(mentUniChains), 2, acf)
par(mfrow = c(ceiling(ncol(as.matrix(mentMultiNoDrugsChains))/2), 2), mar = rep(1, 4))
apply(as.matrix(mentMultiNoDrugsChains), 2, acf)
par(mfrow = c(ceiling(ncol(as.matrix(mentMultiChains))/2), 2), mar = rep(1, 4))
apply(as.matrix(mentMultiChains), 2, acf)

# PHYS
par(mfrow = c(ceiling(ncol(as.matrix(physUniNoDrugsChains))/2), 2), mar = rep(1, 4))
apply(as.matrix(physUniNoDrugsChains), 2, acf)
par(mfrow = c(ceiling(ncol(as.matrix(physUniChains))/2), 2), mar = rep(1, 4))
apply(as.matrix(physUniChains), 2, acf)
par(mfrow = c(ceiling(ncol(as.matrix(physMultiNoDrugsChains))/2), 2), mar = rep(1, 4))
apply(as.matrix(physMultiNoDrugsChains), 2, acf)
par(mfrow = c(ceiling(ncol(as.matrix(physMultiChains))/2), 2), mar = rep(1, 4))
apply(as.matrix(physMultiChains), 2, acf)