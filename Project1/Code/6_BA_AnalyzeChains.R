source("Code/4_BA_DrawChains.R")

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
