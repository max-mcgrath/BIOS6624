source("Code/4_BA_DrawChains.R")

# Means and credible intervals -------------------------------------------------
# Summarize VLOAD chains
vloadChains <- bind_rows(
    "uniNoDrugs" = pivot_longer(bind_rows(as.data.frame(vloadUniNoDrugsChains[[1]]),
                                          as.data.frame(vloadUniNoDrugsChains[[2]])),
                                cols = everything(),
                                names_to = "param"),
    "uni" = pivot_longer(bind_rows(as.data.frame(vloadUniChains[[1]]),
                                   as.data.frame(vloadUniChains[[2]])),
                                cols = everything(),
                                names_to = "param"),
    "multiNoDrugs" = pivot_longer(bind_rows(as.data.frame(vloadMultiNoDrugsChains[[1]]),
                                           as.data.frame(vloadMultiNoDrugsChains[[2]])),
                         cols = everything(),
                         names_to = "param"),
    "multi" = pivot_longer(bind_rows(as.data.frame(vloadMultiChains[[1]]),
                                           as.data.frame(vloadMultiChains[[2]])),
                                 cols = everything(),
                                 names_to = "param"),
    .id = "model")
    

vloadChainsSummary <- vloadChains %>%
    group_by(model, param) %>%
    summarize(mean = mean(value),
              upper95 = quantile(value, .95),
              lower95 = quantile(value, .05))

# Summarize LEU3N chains
leu3nChains <- bind_rows(
    "uniNoDrugs" = pivot_longer(bind_rows(as.data.frame(leu3nUniNoDrugsChains[[1]]),
                                          as.data.frame(leu3nUniNoDrugsChains[[2]])),
                                cols = everything(),
                                names_to = "param"),
    "uni" = pivot_longer(bind_rows(as.data.frame(leu3nUniChains[[1]]),
                                   as.data.frame(leu3nUniChains[[2]])),
                         cols = everything(),
                         names_to = "param"),
    "multiNoDrugs" = pivot_longer(bind_rows(as.data.frame(leu3nMultiNoDrugsChains[[1]]),
                                            as.data.frame(leu3nMultiNoDrugsChains[[2]])),
                                  cols = everything(),
                                  names_to = "param"),
    "multi" = pivot_longer(bind_rows(as.data.frame(leu3nMultiChains[[1]]),
                                     as.data.frame(leu3nMultiChains[[2]])),
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
    "uniNoDrugs" = pivot_longer(bind_rows(as.data.frame(mentUniNoDrugsChains[[1]]),
                                          as.data.frame(mentUniNoDrugsChains[[2]])),
                                cols = everything(),
                                names_to = "param"),
    "uni" = pivot_longer(bind_rows(as.data.frame(mentUniChains[[1]]),
                                   as.data.frame(mentUniChains[[2]])),
                         cols = everything(),
                         names_to = "param"),
    "multiNoDrugs" = pivot_longer(bind_rows(as.data.frame(mentMultiNoDrugsChains[[1]]),
                                            as.data.frame(mentMultiNoDrugsChains[[2]])),
                                  cols = everything(),
                                  names_to = "param"),
    "multi" = pivot_longer(bind_rows(as.data.frame(mentMultiChains[[1]]),
                                     as.data.frame(mentMultiChains[[2]])),
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
    "uniNoDrugs" = pivot_longer(bind_rows(as.data.frame(physUniNoDrugsChains[[1]]),
                                          as.data.frame(physUniNoDrugsChains[[2]])),
                                cols = everything(),
                                names_to = "param"),
    "uni" = pivot_longer(bind_rows(as.data.frame(physUniChains[[1]]),
                                   as.data.frame(physUniChains[[2]])),
                         cols = everything(),
                         names_to = "param"),
    "multiNoDrugs" = pivot_longer(bind_rows(as.data.frame(physMultiNoDrugsChains[[1]]),
                                            as.data.frame(physMultiNoDrugsChains[[2]])),
                                  cols = everything(),
                                  names_to = "param"),
    "multi" = pivot_longer(bind_rows(as.data.frame(physMultiChains[[1]]),
                                     as.data.frame(physMultiChains[[2]])),
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
