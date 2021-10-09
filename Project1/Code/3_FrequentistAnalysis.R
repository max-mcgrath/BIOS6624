source("Code/1_prepData.R")

# Run univariable and multivariable frequentist analyses -----------------------
# VLOAD
lmVloadUniNoDrugs <- lm(LOG_VLOAD_DIFF ~ LOG_VLOAD_0, data = vloadData)
summary(lmVloadUniNoDrugs)
AIC(lmVloadUniNoDrugs)

lmVloadUni <- lm(LOG_VLOAD_DIFF ~ DRUGS_0 + LOG_VLOAD_0, data = vloadData)
summary(lmVloadUni)
AIC(lmVloadUni)

lmVloadMultiNoDrugs <- lm(LOG_VLOAD_DIFF ~ . -DRUGS_0, data = vloadData)
summary(lmVloadNoDrugs)
AIC(lmVloadNoDrugs)

lmVloadMulti <- lm(LOG_VLOAD_DIFF ~ ., data = vloadData)
summary(lmVloadMulti)
AIC(lmVloadMulti)

# LEU3N 
lmLeu3nUniNoDrugs <- lm(LEU3N_DIFF ~ LEU3N_0, data = leu3nData)
summary(lmLeu3nUniNoDrugs)
AIC(lmLeu3nUniNoDrugs)

lmLeu3nUni <- lm(LEU3N_DIFF ~ DRUGS_0 + LEU3N_0, data = leu3nData)
summary(lmLeu3nUni)
AIC(lmLeu3nUni)

lmLeu3nMultiNoDrugs <- lm(LEU3N_DIFF ~ . -DRUGS_0, data = leu3nData)
summary(lmLeu3nMultiNoDrugs)
AIC(lmLeu3nMultiNoDrugs)

lmLeu3nMulti <- lm(LEU3N_DIFF ~ ., data = leu3nData)
summary(lmLeu3nMulti)
AIC(lmLeu3nMulti)

# MENT
lmMentUniNoDrugs <- lm(MENT_DIFF ~ MENT_0, data = mentData)
summary(lmMentUniNoDrugs)
AIC(lmMentUniNoDrugs)

lmMentUni <- lm(MENT_DIFF ~ DRUGS_0 + MENT_0, data = mentData)
summary(lmMentUni)
AIC(lmMentUni)

lmMentMultiNoDrugs <- lm(MENT_DIFF ~ . -DRUGS_0, data = mentData)
summary(lmMentMultiNoDrugs)
AIC(lmMentMultiNoDrugs)

lmMentMulti <- lm(MENT_DIFF ~ ., data = mentData)
summary(lmMentMulti)
AIC(lmMentMulti)

# PHYS 
lmPhysUniNoDrugs <- lm(PHYS_DIFF ~ PHYS_0, data = physData)
summary(lmPhysUniNoDrugs)
AIC(lmPhysUniNoDrugs)

lmPhysUni <- lm(PHYS_DIFF ~ DRUGS_0 + PHYS_0, data = physData)
summary(lmPhysUni)
AIC(lmPhysUni)

lmPhysMultiNoDrugs <- lm(PHYS_DIFF ~ . -DRUGS_0, data = physData)
summary(lmPhysMultiNoDrugs)
AIC(lmPhysMultiNoDrugs)

lmPhysMulti <- lm(PHYS_DIFF ~ ., data = physData)
summary(lmPhysMulti)
AIC(lmPhysMulti)

# Create table summarizing results ---------------------------------------------
fullFreqSummary <- data.frame(
    "model" = c("vloadUniNoDrugs", "vloadUni", "vloadMultiNoDrugs", "vloadMulti", 
              "leu3nUniNoDrugs", "leu3nUni", "leu3nMultiNoDrugs", "leu3nMulti",
              "mentUniNoDrugs", "mentUni", "mentMultiNoDrugs", "mentMulti",
              "physUniNoDrugs", "physUni", "physMultiNoDrugs", "physMulti"),
    "freqEst" = c(NA, coef(lmVloadUni)[["DRUGS_0"]], NA, coef(lmVloadMulti)[["DRUGS_0"]],
                NA, coef(lmLeu3nUni)[["DRUGS_0"]], NA, coef(lmLeu3nMulti)[["DRUGS_0"]],
                NA, coef(lmMentUni)[["DRUGS_0"]], NA, coef(lmMentMulti)[["DRUGS_0"]],
                NA, coef(lmPhysUni)[["DRUGS_0"]], NA, coef(lmPhysMulti)[["DRUGS_0"]]),
    "AIC" = c(AIC(lmVloadUniNoDrugs), AIC(lmVloadUni), AIC(lmVloadMultiNoDrugs), AIC(lmVloadMulti),
            AIC(lmLeu3nUniNoDrugs), AIC(lmLeu3nUni), AIC(lmLeu3nMultiNoDrugs), AIC(lmLeu3nMulti),
            AIC(lmMentUniNoDrugs), AIC(lmMentUni), AIC(lmMentMultiNoDrugs), AIC(lmMentMulti),
            AIC(lmPhysUniNoDrugs), AIC(lmPhysUni), AIC(lmPhysMultiNoDrugs), AIC(lmPhysMulti)),
    "pVal" = c(NA, summary(lmVloadUni)$coefficients["DRUGS_0", 4], NA, summary(lmVloadMulti)$coefficients["DRUGS_0", 4],
               NA, summary(lmLeu3nUni)$coefficients["DRUGS_0", 4], NA, summary(lmLeu3nMulti)$coefficients["DRUGS_0", 4],
               NA, summary(lmMentUni)$coefficients["DRUGS_0", 4], NA, summary(lmMentMulti)$coefficients["DRUGS_0", 4],
               NA, summary(lmPhysUni)$coefficients["DRUGS_0", 4], NA, summary(lmPhysMulti)$coefficients["DRUGS_0", 4])
)
