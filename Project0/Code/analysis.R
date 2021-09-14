library(lme4)

# Aim 1 ------------------------------------------------------------------------
# Read data
withDateTimes24 <- read_rds("DataProcessed/withDateTimes24")

# Plot Data
ggplot(data = withDateTimes24, mapping = aes(x = bookletTime, y = memTime)) +
    geom_point() +
    geom_smooth(method = 'lm')

# Create linear model to evaluate correlation
timesLinearModel <- lm(bookletTime ~ memTime, data = withDateTimes24)

# Evaluate linear model assumptions
car::residualPlot(timesLinearModel)

# Summary statistics for time differences between booklet and MEM

# Aim 2 ------------------------------------------------------------------------
# Read data
questionTwoData <- read_rds("DataProcessed/questionTwoData")

# Plot book versus MEM adherence
ggplot(data = questionTwoData, aes(x = recordType, y = adherence)) +
    geom_boxplot()

# Create table comparing Book and MEM Adherence
adherenceTable <- questionTwoData %>%
    group_by(recordType) %>%
    summarise(total = length(.data$adherence),
              naCount = sum(is.na(.data$adherence)),
              countLess7_5 = sum(abs(.data$adherence) <= 7.5, na.rm = TRUE),
              percentLess7_5 = sum(abs(.data$adherence) <= 7.5, na.rm = TRUE) / 
                  sum(!is.na(.data$adherence)), 
              count7_5to15 = sum(abs(.data$adherence) > 7.5 & 
                                     abs(.data$adherence) <= 15, na.rm = TRUE),
              percent7_5to15 = sum(abs(.data$adherence) > 7.5 & 
                                       abs(.data$adherence) <= 15, 
                                   na.rm = TRUE) / 
                  sum(!is.na(.data$adherence)),
              countGreater15 = sum(abs(.data$adherence) > 15, na.rm = TRUE),
              percentGreater15 = sum(abs(.data$adherence) > 15, na.rm = TRUE) / 
                  sum(!is.na(.data$adherence)),
              countLess15 = sum(abs(.data$adherence) <= 15, na.rm = TRUE),
              percentLess15 = sum(abs(.data$adherence) <= 15, na.rm = TRUE) / 
                  sum(!is.na(.data$adherence)))

# Aim 3 ------------------------------------------------------------------------
# Read data
questionThreeData <- read_rds("DataProcessed/questionThreeData")

# Plot data
(ggplot(data = questionThreeData, aes(x = bookletTime, y = cortisol,
                                      group = interaction(subjectID, day))) +
        geom_point() +
        geom_line() +
        geom_vline(xintercept = 30, color = "red", alpha = .3))

(ggplot(data = questionThreeData, aes(x = bookletTime, y = dhea,
                                      group = interaction(subjectID, day))) +
        geom_point() +
        geom_line() +
        geom_vline(xintercept = 30, color = "red", alpha = .3))

# Further data adjustment necessary to create piecewise regression
# Choose breakpoint to be mean of second observation
breakPoint <- mean(questionThreeData$bookletTime[
    questionThreeData$collectionSample == 2])

# Create new columns
piecewiseData <- questionThreeData %>%
    mutate(I30 = as.numeric(bookletTime >= breakPoint),
           minus30 = bookletTime - breakPoint,
           I30_minus30 = I30 * minus30)

# DHEA -------------------------------------------------------------------------
# Fit random intercepts model for DHEA
dheaLM <- lmer(dhea ~ bookletTime + I30_minus30 + (1 | subjectID), 
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
                   aes(x = bookletTime, y = dhea)))

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
        geom_line(aes(colour = subjectID)))

# Cortisol ---------------------------------------------------------------------
# Fit random intercepts model for DHEA
cortisolLM <- lmer(log(cortisol) ~ bookletTime + I30_minus30 + (1 | subjectID), 
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
subjectRE <- "3012"

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
                   aes(x = bookletTime, y = dhea)))

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
        geom_line(aes(colour = subjectID)))

# Aim 3 w/ logged variables ----------------------------------------------------
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
        geom_point(data = piecewiseData, 
                   aes(x = bookletTime, y = logDHEA, colour = as.factor(subjectID))))

# logCortisol ------------------------------------------------------------------
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
        geom_point(data = piecewiseData, 
                   aes(x = bookletTime, y = logCortisol, colour = as.factor(subjectID))))
