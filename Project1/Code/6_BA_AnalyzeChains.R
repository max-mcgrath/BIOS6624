# source("Code/4_BA_DrawChains.R")

# Read data from previous scripts ----------------------------------------------
# Read cleaned data
vloadData <- readRDS("DataProcessed/vloadData.rda")
leu3nData <- readRDS("DataProcessed/leu3nData.rda")
mentData <- readRDS("DataProcessed/mentData.rda")
physData <- readRDS("DataProcessed/physData.rda")

# Read DICs
vloadUniNoDrugsDIC <- readRDS("DataProcessed/vloadUniNoDrugsDIC.rda")
vloadUniDIC <- readRDS("DataProcessed/vloadUniDIC.rda")
vloadMultiNoDrugsDIC <- readRDS("DataProcessed/vloadMultiNoDrugsDIC.rda")
vloadMultiDIC<- readRDS("DataProcessed/vloadMultiDIC.rda")

leu3nUniNoDrugsDIC <- readRDS("DataProcessed/leu3nUniNoDrugsDIC.rda")
leu3nUniDIC <- readRDS("DataProcessed/leu3nUniDIC.rda")
leu3nMultiNoDrugsDIC <- readRDS("DataProcessed/leu3nMultiNoDrugsDIC.rda")
leu3nMultiDIC<- readRDS("DataProcessed/leu3nMultiDIC.rda")

mentUniNoDrugsDIC <- readRDS("DataProcessed/mentUniNoDrugsDIC.rda")
mentUniDIC <- readRDS("DataProcessed/mentUniDIC.rda")
mentMultiNoDrugsDIC <- readRDS("DataProcessed/mentMultiNoDrugsDIC.rda")
mentMultiDIC<- readRDS("DataProcessed/mentMultiDIC.rda")

physUniNoDrugsDIC <- readRDS("DataProcessed/physUniNoDrugsDIC.rda")
physUniDIC <- readRDS("DataProcessed/physUniDIC.rda")
physMultiNoDrugsDIC <- readRDS("DataProcessed/physMultiNoDrugsDIC.rda")
physMultiDIC<- readRDS("DataProcessed/physMultiDIC.rda")

# Read chains
vloadUniNoDrugsDF <- readRDS("DataProcessed/vloadUniNoDrugsDF.rda")
vloadUniDF <- readRDS("DataProcessed/vloadUniDF.rda")
vloadMultiNoDrugsDF <- readRDS("DataProcessed/vloadMultiNoDrugsDF.rda")
vloadMultiDF <- readRDS("DataProcessed/vloadMultiDF.rda")

leu3nUniNoDrugsDF <- readRDS("DataProcessed/leu3nUniNoDrugsDF.rda")
leu3nUniDF <- readRDS("DataProcessed/leu3nUniDF.rda")
leu3nMultiNoDrugsDF <- readRDS("DataProcessed/leu3nMultiNoDrugsDF.rda")
leu3nMultiDF <- readRDS("DataProcessed/leu3nMultiDF.rda")

mentUniNoDrugsDF <- readRDS("DataProcessed/mentUniNoDrugsDF.rda")
mentUniDF <- readRDS("DataProcessed/mentUniDF.rda")
mentMultiNoDrugsDF <- readRDS("DataProcessed/mentMultiNoDrugsDF.rda")
mentMultiDF <- readRDS("DataProcessed/mentMultiDF.rda")

physUniNoDrugsDF <- readRDS("DataProcessed/physUniNoDrugsDF.rda")
physUniDF <- readRDS("DataProcessed/physUniDF.rda")
physMultiNoDrugsDF <- readRDS("DataProcessed/physMultiNoDrugsDF.rda")
physMultiDF <- readRDS("DataProcessed/physMultiDF.rda")

# Read frequentist summary
fullFreqSummary <- readRDS("DataProcessed/fullFreqSummary.rda")

# Means, credible intervals, posterior probabilities ---------------------------
postProbLog <- function(x) {
    inBounds <- sum(x < log10(1.1) & x > log10(.9))
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
# leu3nMean <- mean(leu3nData$LEU3N_0)
# leu3nEps <- abs(mean(leu3nData$LEU3N_0))*.1
# mentMean <- mean(mentData$MENT_0)
# mentEps <- abs(mean(mentData$MENT_0))*.1
# physMean <- abs(mean(physData$PHYS_0))
# physEps <- abs(mean(physData$PHYS_0))*.1
vloadMean <- mean(vloadData$LOG_VLOAD_DIFF)
vloadEps <- abs(mean(vloadData$LOG_VLOAD_DIFF))*.1
leu3nMean <- mean(leu3nData$LEU3N_DIFF)
leu3nEps <- abs(mean(leu3nData$LEU3N_DIFF))*.1
mentMean <- mean(mentData$MENT_DIFF)
mentEps <- abs(mean(mentData$MENT_DIFF))*.1
physMean <- abs(mean(physData$PHYS_DIFF))
physEps <- abs(mean(physData$PHYS_DIFF))*.1

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
    summarize(bayesDrugsEst = mean(value),
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
    summarize(bayesDrugsEst = mean(value),
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
    summarize(bayesDrugsEst = mean(value),
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
    summarize(bayesDrugsEst = mean(value),
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
    select(-param, -lowerQuant, -upperQuant)

fullBayesianSummary <- deviances %>%
    left_join(drugsCoeff, by = "model") %>%
    select(-DIC) %>%
    mutate("modelType" = case_when(.data$model == "vloadUniNoDrugs" ~ "Univariable w/o Drug Use",
                               .data$model == "vloadUni" ~ "Univariable",
                               .data$model == "vloadMultiNoDrugs" ~ "Multivariable w/o Drug Use",
                               .data$model == "vloadMulti" ~ "Multivariable",
                               .data$model == "leu3nUniNoDrugs" ~ "Univariable w/o Drug Use",
                               .data$model == "leu3nUni" ~ "Univariable",
                               .data$model == "leu3nMultiNoDrugs" ~ "Multivariable w/o Drug Use",
                               .data$model == "leu3nMulti" ~ "Multivariable",
                               .data$model == "mentUniNoDrugs" ~ "Univariable w/o Drug Use",
                               .data$model == "mentUni" ~ "Univariable",
                               .data$model == "mentMultiNoDrugs" ~ "Multivariable w/o Drug Use",
                               .data$model == "mentMulti" ~ "Multivariable",
                               .data$model == "physUniNoDrugs" ~ "Univariable w/o Drug Use",
                               .data$model == "physUni" ~ "Univariable",
                               .data$model == "physMultiNoDrugs" ~ "Multivariable w/o Drug Use",
                               .data$model == "physMulti" ~ "Multivariable")) %>%
    select(model, modelType, penDIC, bayesDrugsEst, hpdLower, hpdUpper, postProb)

# fullSummary <- fullBayesianSummary %>%
#     full_join(fullFreqSummary, by = "model") %>%
#     mutate(across(!model, round, 2)) %>%
#     select(model, bayesDrugsEst, freqDrugsEst, AIC, postProb, pVal, hpdLower, hpdUpper)
