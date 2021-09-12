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
(ggplot(data = questionThreeData, aes(x = bookletTime, y = cortisolUgDl,
                                      group = interaction(subjectID, day))) +
        geom_point() +
        geom_line())

(ggplot(data = questionThreeData, aes(x = bookletTime, y = dheaPgDl,
                                      group = interaction(subjectID, day))) +
        geom_point() +
        geom_line())

# Miscellaneous  ---------------------------------------------------------------