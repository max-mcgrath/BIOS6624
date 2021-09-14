# Load libraries
library(lme4)
library(dplyr)
library(ggplot2)

# Aim 3 ------------------------------------------------------------------------
# Read data
questionThreeData <- read_rds("DataProcessed/questionThreeData")

# Further data adjustment necessary to create piecewise regression
# Choose breakpoint
breakPoint <- 30

# Unused code to set breakpoint to mean of second daily observation
# breakPoint <- max(questionThreeData$bookletTime[
#     questionThreeData$collectionSample == 2])

# Create new columns
piecewiseData <- questionThreeData %>%
    mutate(I30 = as.numeric(bookletTime >= breakPoint),
           minus30 = bookletTime - breakPoint,
           I30_minus30 = I30 * minus30)

# DHEA -------------------------------------------------------------------------
# Fit random intercepts model for DHEA
dheaLM <- lmer(logDHEA ~ bookletTime + I30_minus30 + (1 | subjectID), 
               data = piecewiseData)

# Linear model and diagnostics
summary(dheaLM)
confint(dheaLM)
plot(dheaLM)

# Extract fixed coefficients
betaOne <- coef(dheaLM)$subjectID[1, 2]
betaTwo <- coef(dheaLM)$subjectID[1, 3]

# Choose subject for random effect
# Subject IDs: 3012 3013 3015 3018 3019 3020 3021 3023 3024 3025 3026 3027 3028 
#   3029 3030 3032 3033 3034 3035 3036 3037 3038 3041 3042 3046 3047
#   3048 3049 3053 3060 3061
subjectRE <- "3013"

# Create grid
grid <- seq(min(questionThreeData$bookletTime), 
            max(questionThreeData$bookletTime), by = .1)

# Plot model (for the one subject / random intercept)
predictedData <- data.frame(grid) %>%
    mutate(I30 = as.numeric(grid >= breakPoint),
           minus30 = grid - breakPoint,
           I30_minus30 = I30 * minus30) %>%
    mutate(predicted = coef(dheaLM)$subjectID[subjectRE, 1] +
               grid * betaOne +
               I30_minus30 * betaTwo)

(ggplot(data = predictedData, aes(x = grid, y = predicted)) +
        geom_line() +
        geom_point(data = piecewiseData[piecewiseData$subjectID == subjectRE, ], 
                   aes(x = bookletTime, y = logDHEA)))

# Plot model for all subjects / random intercepts
allSubjectPredictions = as.data.frame(
    matrix(0, 
           nrow = length(grid),
           ncol = length(unique(piecewiseData$subjectID))))
i <- 1
for (subj in unique(piecewiseData$subjectID)) {
    allSubjectPredictions[, i] <- 
        coef(dheaLM)$subjectID[as.character(subj), 1] +
        predictedData$grid * betaOne +
        predictedData$I30_minus30 * betaTwo
    i <- i + 1
}
colnames(allSubjectPredictions) <- paste0("s", unique(piecewiseData$subjectID))

plotDataAll <- allSubjectPredictions %>%
    pivot_longer(cols = starts_with("s"), 
                 names_to = "subjectID", values_to = "predicted") %>%
    arrange(subjectID) %>%
    mutate(grid = rep(grid, times = length(unique(subjectID))))

# Plot all subject predictions
(ggplot(data = plotDataAll, aes(x = grid, y = predicted)) +
        geom_line(aes(colour = subjectID)) +
        labs(title = "Predicted Log DHEA vs Minutes Past Waking",
             x = "Minutes Past Waking", y = "Predicted log(dhea) (nmol/L)"))

# Currently unused code chunk to add observed data to above plot
# geom_point(data = piecewiseData, 
#            aes(x = bookletTime, y = logDHEA, colour = as.factor(subjectID)))

# Cortisol ---------------------------------------------------------------------
# Fit random intercepts model for DHEA
cortisolLM <- lmer(logCortisol ~ bookletTime + I30_minus30 + (1 | subjectID), 
               data = piecewiseData)

# Linear model and diagnostics
summary(cortisolLM)
confint(cortisolLM)
plot(cortisolLM)

# Extract fixed coefficients
betaOne <- coef(cortisolLM)$subjectID[1, 2]
betaTwo <- coef(cortisolLM)$subjectID[1, 3]

# Choose subject for random effect
# Subject IDs: 3012 3013 3015 3018 3019 3020 3021 3023 3024 3025 3026 3027 3028 
#   3029 3030 3032 3033 3034 3035 3036 3037 3038 3041 3042 3046 3047
#   3048 3049 3053 3060 3061
subjectRE <- "3013"

# Create grid
grid <- seq(min(questionThreeData$bookletTime), 
            max(questionThreeData$bookletTime), by = .1)

# Plot model (for the one subject / random intercept)
predictedData <- data.frame(grid) %>%
    mutate(I30 = as.numeric(grid >= breakPoint),
           minus30 = grid - breakPoint,
           I30_minus30 = I30 * minus30) %>%
    mutate(predicted = coef(cortisolLM)$subjectID[subjectRE, 1] +
               grid * betaOne +
               I30_minus30 * betaTwo)

(ggplot(data = predictedData, aes(x = grid, y = predicted)) +
        geom_line() +
        geom_point(data = piecewiseData[piecewiseData$subjectID == subjectRE, ], 
                   aes(x = bookletTime, y = logCortisol)))

# Plot model for all subjects / random intercepts
allSubjectPredictions = as.data.frame(
    matrix(0, 
           nrow = length(grid),
           ncol = length(unique(piecewiseData$subjectID))))
i <- 1
for (subj in unique(piecewiseData$subjectID)) {
    allSubjectPredictions[, i] <- 
        coef(cortisolLM)$subjectID[as.character(subj), 1] +
        predictedData$grid * betaOne +
        predictedData$I30_minus30 * betaTwo
    i <- i + 1
}
colnames(allSubjectPredictions) <- paste0("s", unique(piecewiseData$subjectID))

plotDataAll <- allSubjectPredictions %>%
    pivot_longer(cols = starts_with("s"), 
                 names_to = "subjectID", values_to = "predicted") %>%
    arrange(subjectID) %>%
    mutate(grid = rep(grid, times = length(unique(subjectID))))

# Plot all subject predictions
(ggplot(data = plotDataAll, aes(x = grid, y = predicted)) +
        geom_line(aes(colour = subjectID)) +
        labs(title = "Predicted Log Cortisol vs Minutes Past Waking",
             x = "Minutes Past Waking", y = "Predicted log(cortisol) (nmol/L)"))
