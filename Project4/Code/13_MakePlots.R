library(ggplot2)
library(gridExtra)

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
             x = "Model Selection Technique", y = "True Positive Rate"))


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
             x = "Model Selection Technique", y = "False Positive Rate"))

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
             x = "Model Selection Technique", y = "False Discovery Rate"))

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
             x = "Model Selection Technique", y = "Type I Error Rate"))

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

# Arrange Plots ----------------------------------------------------------------
# modelSummaryPlots <- grid.arrange(tprPlot, fprPlot, fdrPlot, tOnePlot, tTwoPlot,
#                                   ncol = 2,
#                                   layout_matrix = rbind(c(1, 2, 2),
#                                                         c(3, 4, 4),
#                                                         c(NA, 5, 5)))
modelSummaryPlots <- ggpubr::ggarrange(tprPlot, fprPlot, fdrPlot, tOnePlot, tTwoPlot, 
                  ncol=2, nrow=3, common.legend = TRUE, legend="top")
