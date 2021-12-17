library(ggplot2)
library(gridExtra)
library(tidyr)

# Overall True Positive Rate ---------------------------------------------------
# Create data
tprPlotData <- data.frame(
    caseScenario = c(rep("1a", 7), rep("1b", 7), rep("2a", 7), rep("2b", 7)),
    model = factor(rep(c("pVal", "AIC", "BIC", "LASSO", "LASSOCV", "ENET", "ENETCV"), 
                times = 4),
                levels = c("pVal", "AIC", "BIC", "LASSO", "LASSOCV", "ENET", "ENETCV")),
    value = unlist(c(modelDF[1, 2:8], modelDF1b[1, 2:8], modelDF2a[1, 2:8], 
              modelDF2b[1, 2:8]))
)

# Plot TPR
(tprPlot <- ggplot(data = tprPlotData, aes(x = model, y = value, 
                                           group = caseScenario,
                                           color = caseScenario)) +
        geom_line() +
        geom_point() + 
        theme(legend.position = "none") +
        labs(color = "Case Scenario",
             x = " ", y = "Total True Positive Rate"))


# False Positive Rate ----------------------------------------------------------
# Create data
fprPlotData <- data.frame(
    caseScenario = c(rep("1a", 7), rep("1b", 7), rep("2a", 7), rep("2b", 7)),
    model = factor(rep(c("pVal", "AIC", "BIC", "LASSO", "LASSOCV", "ENET", "ENETCV"), 
                       times = 4),
                   levels = c("pVal", "AIC", "BIC", "LASSO", "LASSOCV", "ENET", "ENETCV")),
    value = unlist(c(modelDF[2, 2:8], modelDF1b[2, 2:8], modelDF2a[2, 2:8], 
                     modelDF2b[2, 2:8]))
)

# Plot FPR
(fprPlot <- ggplot(data = fprPlotData, aes(x = model, y = value, 
                                           group = caseScenario,
                                           color = caseScenario)) +
        geom_line() +
        geom_point() + 
        labs(color = "Case Scenario",
             x = " ", y = "False Positive Rate"))

# False Discovery Rate ---------------------------------------------------------
# Create data
fdrPlotData <- data.frame(
    caseScenario = c(rep("1a", 7), rep("1b", 7), rep("2a", 7), rep("2b", 7)),
    model = factor(rep(c("pVal", "AIC", "BIC", "LASSO", "LASSOCV", "ENET", "ENETCV"), 
                       times = 4),
                   levels = c("pVal", "AIC", "BIC", "LASSO", "LASSOCV", "ENET", "ENETCV")),
    value = unlist(c(modelDF[3, 2:8], modelDF1b[3, 2:8], modelDF2a[3, 2:8], 
                     modelDF2b[3, 2:8]))
)

# Plot FDR
(fdrPlot <- ggplot(data = fdrPlotData, aes(x = model, y = value,  
                                           group = caseScenario,
                                           color = caseScenario)) +
        geom_line() +
        geom_point() + 
        theme(legend.position = "none") +
        labs(color = "Case Scenario",
             x = " ", y = "False Discovery Rate"))

# Type I Error Rate ------------------------------------------------------------
# Create data
tOnePlotData <- data.frame(
    caseScenario = c(rep("1a", 7), rep("1b", 7), rep("2a", 7), rep("2b", 7)),
    model = factor(rep(c("pVal", "AIC", "BIC", "LASSO", "LASSOCV", "ENET", "ENETCV"), 
                       times = 4),
                   levels = c("pVal", "AIC", "BIC", "LASSO", "LASSOCV", "ENET", "ENETCV")),
    value = unlist(c(modelDF[4, 2:8], modelDF1b[4, 2:8], modelDF2a[4, 2:8], 
                     modelDF2b[4, 2:8]))
)

# Plot FDR
(tOnePlot <- ggplot(data = tOnePlotData, aes(x = model, y = value, 
                                           group = caseScenario,
                                           color = caseScenario)) +
        geom_line() +
        geom_point() + 
        labs(color = "Case Scenario",
             x = " ", y = "Type I Error Rate"))

# Type II Error Rate -----------------------------------------------------------
# Create data
tTwoPlotData <- data.frame(
    caseScenario = c(rep("1a", 7), rep("1b", 7), rep("2a", 7), rep("2b", 7)),
    model = factor(rep(c("pVal", "AIC", "BIC", "LASSO", "LASSOCV", "ENET", "ENETCV"), 
                       times = 4),
                   levels = c("pVal", "AIC", "BIC", "LASSO", "LASSOCV", "ENET", "ENETCV")),
    value = unlist(c(modelDF[5, 2:8], modelDF1b[5, 2:8], modelDF2a[5, 2:8], 
                     modelDF2b[5, 2:8]))
)

# Plot FDR
(tTwoPlot <- ggplot(data = tTwoPlotData, aes(x = model, y = value, 
                                             group = caseScenario,
                                             color = caseScenario)) +
        geom_line() +
        geom_point() + 
        labs(color = "Case Scenario",
             x = "Model Selection Technique", y = "Type II Error Rate"))

# Average Bias -----------------------------------------------------------------
biasData <- bind_rows(summarize_all(coefDF[c(1, 4, 7, 10, 13), 2:8], mean),
                      summarize_all(coefDF1b[c(1, 4, 7, 10, 13), 2:8], mean),
                      summarize_all(coefDF2a[c(1, 4, 7, 10, 13), 2:8], mean),
                      summarize_all(coefDF2b[c(1, 4, 7, 10, 13), 2:8], mean)) %>%
    mutate(caseScenario = c("1a", "1b", "2a", "2b")) %>%
    pivot_longer(!caseScenario) %>%
    mutate(caseScenario = factor(.data$caseScenario, levels = c("1a", "1b", "2a", "2b")),
           model = factor(rep(c("pVal", "AIC", "BIC", "LASSO", "LASSOCV", "ENET", "ENETCV"), 
                              times = 4),
                          levels = c("pVal", "AIC", "BIC", "LASSO", "LASSOCV", "ENET", "ENETCV")))

(biasPlot <- ggplot(data = biasData, aes(x = model, y = value, 
                                             group = caseScenario,
                                             color = caseScenario)) +
        geom_line() +
        geom_point() + 
        labs(color = "Case Scenario",
             x = "Model Selection Technique", y = "Average Bias"))
    

# Arrange Plots ----------------------------------------------------------------
# modelSummaryPlots <- grid.arrange(tprPlot, fprPlot, fdrPlot, tOnePlot, tTwoPlot,
#                                   ncol = 2,
#                                   layout_matrix = rbind(c(1, 2, 2),
#                                                         c(3, 4, 4),
#                                                         c(NA, 5, 5)))
modelSummaryPlots <- ggpubr::ggarrange(tprPlot, fprPlot, fdrPlot, tOnePlot, 
                                       tTwoPlot, biasPlot,
                  ncol=2, nrow=3, common.legend = TRUE, legend="top")

# Coefficient specific ---------------------------------------------------------
coefTableData <- bind_rows(coefDF, coefDF1b, coefDF2a, coefDF2b) %>%
    mutate(caseScenario = rep(c("1a", "1b", "2a", "2b"), each = 15),
           coef = rep(c(rep("X1", 3), rep("X2", 3), rep("X3", 3), 
                        rep("X4", 3), rep("X5", 3)), 4)) %>%
    pivot_longer(!c(caseScenario, coef, stat), names_to = "model") %>%
    mutate(model = case_when(model == "p.value" ~ "pVal",
                             model == "LASSO" ~ "LAS",
                             model == "LASSO.w..CV" ~ "LCV",
                             model == "EN.w..CV" ~ "ENCV",
                             TRUE ~ model)) %>%
    mutate(model = factor(model, levels = c("pVal", "AIC", "BIC", "LAS",
                                            "LCV", "EN", "ENCV")))

# Bias plot
biasPlot <- ggplot(data = coefTableData %>% filter(stat == "Bias"), 
       aes(x = model, y = value, color = caseScenario, group = caseScenario)) +
    geom_point() +
    geom_line() + 
    facet_wrap(~ coef, ncol = 5, nrow = 1) +
    labs(y = "Coefficient Bias", x = " ", color = "Case Scenario", group = "Case Scenario")

# CI coverage plot
ciCovPlot <- ggplot(data = coefTableData %>% filter(stat == "95\\% CI"), 
                   aes(x = model, y = value, color = caseScenario, group = caseScenario)) +
    geom_point() +
    geom_line() + 
    facet_wrap(~ coef, ncol = 5, nrow = 1) +
    labs(y = "95% CI Coverage", x = " ", color = "Case Scenario", group = "Case Scenario")

# TPR Plot
indTprPlot <- ggplot(data = coefTableData %>% filter(stat == "TPR"), 
                    aes(x = model, y = value, color = caseScenario, group = caseScenario)) +
    geom_point() +
    geom_line() + 
    facet_wrap(~ coef, ncol = 5, nrow = 1) +
    labs(y = "True Positive Rate", y = "Technique", color = "Case Scenario", group = "Case Scenario")
           
# Combine coefficient plots
coefPlots <- ggpubr::ggarrange(biasPlot, ciCovPlot, indTprPlot,
                               ncol=1, nrow=3, common.legend = TRUE, legend="top")
