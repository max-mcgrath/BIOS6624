source("Code/1_ProcessData.R")

# Create Table 1
tableOneContinousOverall <- cleanData %>%
    group_by(SEX) %>%
    summarise(across(c(AGE, SYSBP), list("overallMean" = mean, 
                                         "overallSD" = sd)))

tableOneContinousByResponse <- cleanData %>%
    group_by(SEX, stroke10yr) %>%
    summarise(across(c(AGE, SYSBP), list("mean" = mean, 
                                         "SD" = sd)))

tableOneDichOverall <- cleanData %>%
    group_by(SEX) %>%
    summarise(across(c(BPMEDS, DIABETES, CURSMOKE, ANYCHD),
                     list(nEvent = function(x) { sum(x == 1) },
                          nCensored = function(x) { sum(x == 0) },
                          eventPct = function(x) { sum(x == 1) / sum(.data$stroke10yr == 1) * 100},
                          censoredPct = function(x) { sum(x == 0) / sum(.data$stroke10yr == 0) * 100})))
              
                     