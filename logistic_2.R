### Logistic Regression
library(dplyr)
library(ggplot2)
library(scales)
library(caret)
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
           income = INC110213, education = EDU685213, old = AGE775214, foreign = POP645213) %>%
    mutate(county = tolower(gsub(' County', '', county)))
  
  assign('demogrSome', demogrSome, envir = globalenv())
}

demogrSomeF(IL, IN, IA, KS, MI, MN, MO, NE, ND, OH, SD, WI)

main <- merge(merge(demogrSome, select(srcElections, -c('state', 'county')), by = 'fips'),
              select(srcUnemp, -c('state', 'county')), by = 'fips')

rm(demogrSome)

attach(main)
main$trumpWin <- ifelse(trump16 > clinton16 & trump16 > otherpres16, 1, 0) %>%
  factor(levels = c(0, 1))

main$obamaVotes <- obama12 / (romney12 + obama12 + otherpres12) * 100

main$romneyVotes <- romney12 / (romney12 + obama12 + otherpres12) * 100

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
logReg <- glm(Class ~ obamaVotes, family = 'binomial', data = upData)
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

# Obama
logReg <- glm(Class ~ obamaVotes, family = 'binomial', data = upData)
main$prob <- predict(logReg, newdata = main, type = 'response')
main$predict <- ifelse(predict(logReg, newdata = main, type = 'response') > main$cutoff, 1, 0)

logPlot(obamaVotes, '% Obama Votes in 2012', percent = TRUE)

# Romney
logReg <- glm(Class ~ romneyVotes, family = 'binomial', data = upData)
main$prob <- predict(logReg, newdata = main, type = 'response')
main$predict <- ifelse(predict(logReg, newdata = main, type = 'response') > main$cutoff, 1, 0)

logPlot(romneyVotes, '% Romney Votes in 2012', percent = TRUE)

# Old
logReg <- glm(Class ~ old, family = 'binomial', data = upData)
main$prob <- predict(logReg, newdata = main, type = 'response')
main$predict <- ifelse(predict(logReg, newdata = main, type = 'response') > main$cutoff, 1, 0)

logPlot(old, '% Population Aged 65 and over', percent = TRUE)

# Foreign
logReg <- glm(Class ~ foreign, family = 'binomial', data = upData)
main$prob <- predict(logReg, newdata = main, type = 'response')
main$predict <- ifelse(predict(logReg, newdata = main, type = 'response') > main$cutoff, 1, 0)

logPlot(foreign, '% Foreigners in Population', percent = TRUE)

## SECTION 6. Plot Linear Regression Models
main[, c('cutoff', 'prob', 'predict', 'trumpWin')] <- NULL

attach(main)
main$trumpVotes <- trump16 / (trump16 + clinton16 + otherpres16)
main$unempDelta <- (unemp_rate16 - unemp_rate12) / unemp_rate12
main$unempDeltaNorm <- (unempDelta - min(unempDelta)) / (max(unempDelta) - min(unempDelta))
detach(main)

# Counties with increasing unemployment rates from 2012 to 2015 tend to favor Trump.
linReg <- lm(trumpVotes ~ unempDeltaNorm, data = main)
summary(linReg)

ggplot(main, aes(x = unempDeltaNorm, y = trumpVotes)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  scale_x_log10() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = '% Change in Unemployment 2012-2015 (Rescaled 0-1)', y = '% Votes for Trump')

# The elderly tend to favor Trump.
linReg <- lm(trumpVotes ~ old, data = main)
summary(linReg)

ggplot(main, aes(x = old / 100, y = trumpVotes)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = '% Population Aged 65 and over', y = '% Votes for Trump')

# Foreigners tend to disfavor Trump.
linReg <- lm(trumpVotes ~ foreign, data = main)
summary(linReg)

ggplot(main, aes(x = foreign / 100, y = trumpVotes)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  scale_x_log10(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = '% Foreigners in Population', y = '% Votes for Trump')






