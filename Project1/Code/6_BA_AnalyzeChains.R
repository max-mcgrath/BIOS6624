source("Code/4_BA_DrawChains.R")

# Means, credible intervals, posterior probabilities ---------------------------
postProbLog <- function(x) {
    inBounds <- sum(x < log(1.1) & x > log(.9))
    1 - (inBounds / length(x))
}

postProb <- function(x, mean, eps) {
    inBounds <- sum(x < eps & x > -eps)
    1 - (inBounds / length(x))
}

hpd <- function(x, alpha = 0.05){
    n = length(x)
    m = round(n * alpha)
    x = sort(x)
    y = x[(n - m + 1):n] - x[1:m]
    z = min(y)
    k = which(y == z)[1]
    c(x[k], x[n - m + k])
}

# Store epsilons for calculation of posterior probabilities
leu3nMean <- mean(leu3nData$LEU3N_0)
leu3nEps <- abs(mean(leu3nData$LEU3N_0))*.1
mentMean <- mean(mentData$MENT_0)
mentEps <- abs(mean(mentData$MENT_0))*.1
physMean <- abs(mean(physData$PHYS_0))
physEps <- abs(mean(physData$PHYS_0))*.1

# Summarize VLOAD chains
vloadChains <- bind_rows(
    "vloadUniNoDrugs" = pivot_longer(vloadUniNoDrugsDF,
                                cols = everything(),
                                names_to = "param"),
    "vloadUni" = pivot_longer(vloadUniDF,
                         cols = everything(),
                         names_to = "param"),
    "vloadMultiNoDrugs" = pivot_longer(vloadMultiNoDrugsDF,
                         cols = everything(),
                         names_to = "param"),
    "vloadMulti" = pivot_longer(vloadMultiDF,
                           cols = everything(),
                           names_to = "param"),
    .id = "model")
    

vloadChainsSummary <- vloadChains %>%
    group_by(model, param) %>%
    summarize(bayesEst = mean(value),
              hpdLower = hpd(value, alpha = 0.05)[1],
              hpdUpper = hpd(value, alpha = 0.05)[2],
              lowerQuant = quantile(value, .05),
              upperQuant = quantile(value, .95),
              postProb = postProbLog(value))

# Summarize LEU3N chains
leu3nChains <- bind_rows(
    "leu3nUniNoDrugs" = pivot_longer(leu3nUniNoDrugsDF,
                                cols = everything(),
                                names_to = "param"),
    "leu3nUni" = pivot_longer(leu3nUniDF,
                         cols = everything(),
                         names_to = "param"),
    "leu3nMultiNoDrugs" = pivot_longer(leu3nMultiNoDrugsDF,
                                  cols = everything(),
                                  names_to = "param"),
    "leu3nMulti" = pivot_longer(leu3nMultiDF,
                           cols = everything(),
                           names_to = "param"),
    .id = "model")

leu3nChainsSummary <- leu3nChains %>%
    group_by(model, param) %>%
    summarize(bayesEst = mean(value),
              hpdLower = hpd(value, alpha = 0.05)[1],
              hpdUpper = hpd(value, alpha = 0.05)[2],
              lowerQuant = quantile(value, .05),
              upperQuant = quantile(value, .95),
              postProb = postProb(value, mean = leu3nMean, eps = leu3nEps))

# Summarize MENT chains
mentChains <- bind_rows(
    "mentUniNoDrugs" = pivot_longer(mentUniNoDrugsDF,
                                cols = everything(),
                                names_to = "param"),
    "mentUni" = pivot_longer(mentUniDF,
                         cols = everything(),
                         names_to = "param"),
    "mentMultiNoDrugs" = pivot_longer(mentMultiNoDrugsDF,
                                  cols = everything(),
                                  names_to = "param"),
    "mentMulti" = pivot_longer(mentMultiDF,
                           cols = everything(),
                           names_to = "param"),
    .id = "model")

mentChainsSummary <- mentChains %>%
    group_by(model, param) %>%
    summarize(bayesEst = mean(value),
              hpdLower = hpd(value, alpha = 0.05)[1],
              hpdUpper = hpd(value, alpha = 0.05)[2],
              lowerQuant = quantile(value, .05),
              upperQuant = quantile(value, .95),
              postProb = postProb(value, mentMean, eps = mentEps))

# Summarize PHYS chains
physChains <- bind_rows(
    "physUniNoDrugs" = pivot_longer(physUniNoDrugsDF,
                                cols = everything(),
                                names_to = "param"),
    "physUni" = pivot_longer(physUniDF,
                         cols = everything(),
                         names_to = "param"),
    "physMultiNoDrugs" = pivot_longer(physMultiNoDrugsDF,
                                  cols = everything(),
                                  names_to = "param"),
    "physMulti" = pivot_longer(physMultiDF,
                           cols = everything(),
                           names_to = "param"),
    .id = "model")

physChainsSummary <- physChains %>%
    group_by(model, param) %>%
    summarize(bayesEst = mean(value),
              hpdLower = hpd(value, alpha = 0.05)[1],
              hpdUpper = hpd(value, alpha = 0.05)[2],
              lowerQuant = quantile(value, .05),
              upperQuant = quantile(value, .95),
              postProb = postProb(value, mean = physMean, eps = physEps))


# DIC --------------------------------------------------------------------------
deviances <- data.frame("model" = c("vloadUniNoDrugs", "vloadUni", "vloadMultiNoDrugs", "vloadMulti", 
                                    "leu3nUniNoDrugs", "leu3nUni", "leu3nMultiNoDrugs", "leu3nMulti",
                                    "mentUniNoDrugs", "mentUni", "mentMultiNoDrugs", "mentMulti",
                                    "physUniNoDrugs", "physUni", "physMultiNoDrugs", "physMulti"),
                        "penDIC" = c(sum(vloadUniNoDrugsDIC$deviance), 
                                     sum(vloadUniDIC$deviance),
                                     sum(vloadMultiNoDrugsDIC$deviance), 
                                     sum(vloadMultiDIC$deviance),
                                     sum(leu3nUniNoDrugsDIC$deviance), 
                                     sum(leu3nUniDIC$deviance),
                                     sum(leu3nMultiNoDrugsDIC$deviance),
                                     sum(leu3nMultiDIC$deviance), 
                                     sum(mentUniNoDrugsDIC$deviance),
                                     sum(mentUniDIC$deviance), 
                                     sum(mentMultiNoDrugsDIC$deviance),
                                     sum(mentMultiDIC$deviance), 
                                     sum(physUniNoDrugsDIC$deviance), 
                                     sum(physUniDIC$deviance),
                                     sum(physMultiNoDrugsDIC$deviance), 
                                     sum(physMultiDIC$deviance)),
                        "DIC" = c(sum(vloadUniNoDrugsDIC$deviance) + sum(vloadUniNoDrugsDIC[[2]]),
                                  sum(vloadUniDIC$deviance) + sum(vloadUniDIC[[2]]),
                                  sum(vloadMultiNoDrugsDIC$deviance) + sum(vloadMultiNoDrugsDIC[[2]]),
                                  sum(vloadMultiDIC$deviance) + sum(vloadMultiDIC[[2]]),
                                  sum(leu3nUniNoDrugsDIC$deviance) + sum(leu3nUniNoDrugsDIC[[2]]),
                                  sum(leu3nUniDIC$deviance) + sum(leu3nUniDIC[[2]]),
                                  sum(leu3nMultiNoDrugsDIC$deviance) + sum(leu3nMultiNoDrugsDIC[[2]]),
                                  sum(leu3nMultiDIC$deviance) + sum(leu3nMultiDIC[[2]]),
                                  sum(mentUniNoDrugsDIC$deviance) + sum(mentUniNoDrugsDIC[[2]]),
                                  sum(mentUniDIC$deviance) + sum(mentUniDIC[[2]]),
                                  sum(mentMultiNoDrugsDIC$deviance) + sum(mentMultiNoDrugsDIC[[2]]),
                                  sum(mentMultiDIC$deviance) + sum(mentMultiDIC[[2]]),
                                  sum(physUniNoDrugsDIC$deviance) + sum(physUniNoDrugsDIC[[2]]),
                                  sum(physUniDIC$deviance) + sum(physUniDIC[[2]]),
                                  sum(physMultiNoDrugsDIC$deviance) + sum(physMultiNoDrugsDIC[[2]]),
                                  sum(physMultiDIC$deviance) + sum(physMultiDIC[[2]]))
                        )

# Create table(s) summarizing relevant information -----------------------------
drugsCoeff <- bind_rows(filter(vloadChainsSummary, param == "DRUGS_0"),
                       filter(leu3nChainsSummary, param == "DRUGS_0"),
                       filter(mentChainsSummary, param == "DRUGS_0"),
                       filter(physChainsSummary, param == "DRUGS_0")) %>%
    select(-param)

fullBayesianSummary <- deviances %>%
    left_join(drugsCoeff, by = "model")

fullSummary <- fullBayesianSummary %>%
    full_join(fullFreqSummary, by = "model") %>%
    mutate(across(!model, round, 2)) %>%
    select(model, bayesEst, freqEst, AIC, DIC, postProb, pVal, hpdLower, hpdUpper)
