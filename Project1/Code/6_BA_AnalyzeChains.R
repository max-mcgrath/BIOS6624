source("Code/4_BA_DrawChains.R")

# Means, credible intervals, posterior probabilities ---------------------------
postProbLog <- function(x) {
    inBounds <- sum(x < log(1.10) & x > log(.90))
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


# Summarize VLOAD chains
vloadChains <- bind_rows(
    "uniNoDrugs" = pivot_longer(vloadUniNoDrugsDF,
                                cols = everything(),
                                names_to = "param"),
    "uni" = pivot_longer(vloadUniDF,
                         cols = everything(),
                         names_to = "param"),
    "multiNoDrugs" = pivot_longer(vloadMultiNoDrugsDF,
                         cols = everything(),
                         names_to = "param"),
    "multi" = pivot_longer(vloadMultiDF,
                           cols = everything(),
                           names_to = "param"),
    .id = "model")
    

vloadChainsSummary <- vloadChains %>%
    group_by(model, param) %>%
    summarize(mean = mean(value),
              hpdLower = hpd(value, alpha = 0.05)[1],
              hpdUpper = hpd(value, alpha = 0.05)[2],
              lowerQuant = quantile(value, .05),
              upperQuant = quantile(value, .95),
              postProb = postProbLog(value))

# Summarize LEU3N chains
leu3nChains <- bind_rows(
    "uniNoDrugs" = pivot_longer(leu3nUniNoDrugsDF,
                                cols = everything(),
                                names_to = "param"),
    "uni" = pivot_longer(leu3nUniDF,
                         cols = everything(),
                         names_to = "param"),
    "multiNoDrugs" = pivot_longer(leu3nMultiNoDrugsDF,
                                  cols = everything(),
                                  names_to = "param"),
    "multi" = pivot_longer(leu3nMultiDF,
                           cols = everything(),
                           names_to = "param"),
    .id = "model")


leu3nChainsSummary <- leu3nChains %>%
    group_by(model, param) %>%
    summarize(mean = mean(value),
              upper95 = quantile(value, .95),
              lower95 = quantile(value, .05))

# Summarize MENT chains
mentChains <- bind_rows(
    "uniNoDrugs" = pivot_longer(mentUniNoDrugsDF,
                                cols = everything(),
                                names_to = "param"),
    "uni" = pivot_longer(mentUniDF,
                         cols = everything(),
                         names_to = "param"),
    "multiNoDrugs" = pivot_longer(mentMultiNoDrugsDF,
                                  cols = everything(),
                                  names_to = "param"),
    "multi" = pivot_longer(mentMultiDF,
                           cols = everything(),
                           names_to = "param"),
    .id = "model")

mentChainsSummary <- mentChains %>%
    group_by(model, param) %>%
    summarize(mean = mean(value),
              upper95 = quantile(value, .95),
              lower95 = quantile(value, .05))

# Summarize PHYS chains
physChains <- bind_rows(
    "uniNoDrugs" = pivot_longer(physUniNoDrugsDF,
                                cols = everything(),
                                names_to = "param"),
    "uni" = pivot_longer(physUniDF,
                         cols = everything(),
                         names_to = "param"),
    "multiNoDrugs" = pivot_longer(physMultiNoDrugsDF,
                                  cols = everything(),
                                  names_to = "param"),
    "multi" = pivot_longer(physMultiDF,
                           cols = everything(),
                           names_to = "param"),
    .id = "model")


physChainsSummary <- physChains %>%
    group_by(model, param) %>%
    summarize(mean = mean(value),
              upper95 = quantile(value, .95),
              lower95 = quantile(value, .05))


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
