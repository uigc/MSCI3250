### Logistic Regression
library(dplyr)
library(ggplot2)
library(scales)
library(caret)
library(choroplethr)
library(choroplethrMaps)
library(cowplot)
rm(list = ls())

## SECTION 1. Source Files
srcElections <- read.csv('county_elections.csv', stringsAsFactors = FALSE)
srcDemogr <- read.csv('county_facts.csv', stringsAsFactors = FALSE)
srcDict <- read.csv('county_facts_dictionary.csv', stringsAsFactors = FALSE)
srcUnemp <- read.csv('county_unemployment.csv', stringsAsFactors = FALSE)

## SECTION 2. Data Merging and Calculations
demogrSomeF <- function(...) {
  states <- gsub('\"', '', toupper(sapply(substitute(list(...)), deparse)[-1]))
  
  demogrSome <- filter(srcDemogr, state_abbreviation %in% states) %>%
    select(fips = fips, state = state_abbreviation, county = area_name,
           income = INC110213, education = EDU685213, density = POP060210,
           white = RHI825214, hispanic = RHI725214, household = HSD310213) %>%
    mutate(county = tolower(gsub(' County', '', county)))
  
  assign('demogrSome', demogrSome, envir = globalenv())
}

demogrSomeF(IL, IN, IA, KS, MI, MN, MO, NE, ND, OH, SD, WI)

main <- merge(demogrSome, select(srcElections, -c('state', 'county')), by = 'fips')

# unemp <- mutate(srcUnemp, county = tolower(county))

# main <- merge(merge(merge(merge(pres, demogrSome), unemp), rgdp), pop)

rm(demogrSome)

attach(main)
main$trumpWin <- ifelse(trump16 > clinton16 & trump16 > otherpres16, 1, 0) %>%
  factor(levels = c(0, 1))

main$obamaVotes <- obama12 / (romney12 + obama12 + otherpres12)

main$romneyVotes <- romney12 / (romney12 + obama12 + otherpres12)

main$repDelta <- ((trump16 / (trump16 + clinton16 + otherpres16)) -
                    (romney12 / (romney12 + obama12 + otherpres12))) /
  (romney12 / (romney12 + obama12 + otherpres12))

main$cutoff <- 1 - (trump16 / (trump16 + clinton16 + otherpres16))
detach(main)

## SECTION 3. Prediction Model
rm(trainDataIndex, trainData, testData, upData, logReg)

set.seed(42)
trainDataIndex <- createDataPartition(main$trumpWin, p = 0.7, list = FALSE)
trainData <- main[trainDataIndex, ]
testData <- main[- trainDataIndex, ]

# Determine class bias
table(main$trumpWin)
table(trainData$trumpWin)

upData <- upSample(x = trainData[!(names(trainData) %in% c('trumpWin'))],
                   y = trainData$trumpWin)

## SECTION 4. Logistic Regression Model
logReg <- glm(Class ~ romneyVotes, family = 'binomial', data = upData)
summary(logReg)
exp(coef(logReg))

# Apply model on test dataset
predict <- predict(logReg, newdata = testData, type = 'response')
cutoff <- table(main$trumpWin)['0'][[1]] / nrow(main)
winPredictions <- ifelse(predict > cutoff, 1, 0)
mean(winPredictions == testData$trumpWin)

# Assess model fit
with(logReg, null.deviance - deviance)
with(logReg, df.null - df.residual)
with(logReg, pchisq(null.deviance - deviance, df.null - df.residual,
                    lower.tail = FALSE))

## SECTION 5. Plot Logistic Regression Models
logPlot <- function(metric, xlab, percent) {
  metric <- gsub('\"', '', deparse(substitute(metric)))
  
  if (percent == TRUE) {
    main[, metric] <- main[, metric] / 100
    label <- percent_format(accuracy = 1)
  } else {
    main[, metric] <- main[, metric]
    label <- number_format(accuracy = 1)
  }
  
  ggplot(main, aes(x = main[, metric], y = predict)) +
    geom_point(shape = 1, aes(color = as.factor(predict))) +
    scale_color_manual(labels = c('0' = 'Win', '1' = 'Lose'),
                       values = c('0' = 'Blue', '1' = 'Red')) +
    geom_line(aes(y = main$prob), lwd = 1.5) +
    scale_x_continuous(labels = label) +
    labs(x = xlab, y = 'P (Republicans Winning)', color = 'Trump')
}

# Education
logReg <- glm(Class ~ obamaVotes, family = 'binomial', data = upData)
main$prob <- predict(logReg, newdata = main, type = 'response')
main$predict <- ifelse(predict(logReg, newdata = main, type = 'response') > main$cutoff, 1, 0)

logPlot(obamaVotes, '% Obama Votes in 2012', percent = TRUE)

logReg <- glm(Class ~ romneyVotes, family = 'binomial', data = upData)
main$prob <- predict(logReg, newdata = main, type = 'response')
main$predict <- ifelse(predict(logReg, newdata = main, type = 'response') > main$cutoff, 1, 0)

logPlot(romneyVotes, '% Romney Votes in 2012', percent = TRUE)











