### Logistic Regression
## Refer to https://github.com/shenlim/MSCI3250/wiki/Logistic-Regression
library(dplyr)
library(ggplot2)
library(scales)
library(caret)
library(choroplethr)
library(choroplethrMaps)
library(cowplot)
rm(list = ls())

## 1. Source Files
srcPresident <- read.csv('county_presidential.csv', stringsAsFactors = FALSE)
srcDemogr <- read.csv('county_facts.csv', stringsAsFactors = FALSE)
srcDict <- read.csv('county_facts_dictionary.csv', stringsAsFactors = FALSE)
srcRgdp <- read.csv('county_rgdp.csv', stringsAsFactors = FALSE, check.names = FALSE)
srcUnemp <- read.csv('county_unemployment.csv', stringsAsFactors = FALSE)
srcPop <- read.csv('county_population.csv', stringsAsFactors = FALSE)

## 2. Data Cleanup and Merging
pres <- select(srcPresident, fips = combined_fips, state = state_abbr,
               county = county_name,
               votesDem12 = votes_dem_2012, votesRep12 = votes_gop_2012,
               votesDem16 = votes_dem_2016, votesRep16 = votes_gop_2016) %>%
  mutate(county = tolower(gsub(' County', '', county)))

demogrSomeF <- function(...) {
  states <- gsub('\"', '', toupper(sapply(substitute(list(...)), deparse)[-1]))
	
	demogrSome <- filter(srcDemogr, state_abbreviation %in% states) %>%
	  select(state = state_abbreviation, county = area_name,
	         income = INC110213, education = EDU685213, density = POP060210,
	         white = RHI825214, hispanic = RHI725214, household = HSD310213) %>%
	  mutate(county = tolower(gsub(' County', '', county)))
	
	assign('demogrSome', demogrSome, envir = globalenv())
}

demogrSomeF(IL, IN, IA, KS, MI, MN, MO, NE, ND, OH, SD, WI)

unemp <- mutate(srcUnemp, county = tolower(county))
rgdp <- mutate(srcRgdp, county = tolower(county))
pop <- mutate(srcPop, county = tolower(county))

main <- merge(merge(merge(merge(pres, demogrSome), unemp), rgdp), pop)

rm(pres, demogrSome, unemp, rgdp, pop)

## 3. Calculate Unemployment Change, Rgdp Change, 2016 winner
main$unempDelta <- (main$unemp_rate15 - main$unemp_rate12) / main$unemp_rate12

main$rgdppcDelta <- ((main$rgdp2015 / main$pop2015) - (main$rgdp2012 / main$pop2012)) /
  (main$rgdp2012 / main$pop2012)

main$winner16 <- ifelse(main$votesRep16 > main$votesDem16, 1, 0) %>%
  factor(levels = c(0, 1))

## 4. Partition the Dataset
rm(trainDataIndex, trainData, testData, upData, logReg)

set.seed(42)
trainDataIndex <- createDataPartition(main$winner16, p = 0.7, list = FALSE)

trainData <- main[trainDataIndex, ]
testData <- main[- trainDataIndex, ]

# Determine class bias
table(main$winner16)
table(trainData$winner16)

upData <- upSample(x = trainData[!(names(trainData) %in% c('winner16'))],
                   y = trainData$winner16)

## 5. Logistic Regression Model
logReg <- glm(Class ~ income + education, family = 'binomial', data = upData)
summary(logReg)
exp(coef(logReg))

# Apply model on test dataset
predict <- predict(logReg, newdata = testData, type = 'response')
cutoff <- table(main$winner16)['0'][[1]] / nrow(main)
winPredictions <- ifelse(predict > cutoff, 1, 0)
mean(winPredictions == testData$winner16)

# Assess model fit
with(logReg, null.deviance - deviance)
with(logReg, df.null - df.residual)
with(logReg, pchisq(null.deviance - deviance, df.null - df.residual,
                    lower.tail = FALSE))

## 6. Map Visualization
# State names in 'choroplethr' must be formatted correctly
states <- c('illinois', 'indiana', 'iowa', 'kansas', 'michigan', 'minnesota', 'missouri',
            'nebraska', 'north dakota', 'ohio', 'south dakota', 'wisconsin')

# Plot actual 2016 results
mapDf <- data.frame(region = main$fips,
                    value = ifelse(main$winner16 == 1, 'Republican', 'Democrat') %>%
                      as.factor())

county_choropleth(mapDf, state_zoom = states, title = '2016 Actual Results') +
  scale_fill_manual(values = alpha(c('blue', 'red'), 0.6),
                    labels = c('D', 'R'),
                    name = 'Party')

# Plot predicted results based on 'education' and 'income'
predictDf <- data.frame(region = main$fips,
                        value = ifelse(predict(logReg, newdata = main, type = 'response') > cutoff,
                                       'Republican', 'Democrat') %>% as.factor())

county_choropleth(predictDf, state_zoom = states, title = '2016 Predictions') +
	scale_fill_manual(values = alpha(c('blue', 'red'), 0.6),
	                  labels = c('D', 'R'),
	                  name = 'Party')

# Plot accuracy
accuracyDf <- data.frame(region = main$fips,
                         value = ifelse(mapDf$value == predictDf$value, 'Correct', 'Wrong') %>%
                           as.factor())

county_choropleth(accuracyDf, state_zoom = states, title = 'Model Accuracy') +
  scale_fill_manual(values = alpha(c('green', 'red'), 0.6),
                    labels = c('Correct', 'Wrong'),
                    name = 'Result')

## 7. Plot Logistic Regression Models
# We'll build a function to plot consistent logistic regression plots. The function has
# 2 required and 1 optional arguments. Required: A single 'metric' as the x-axis, 'xlab'
# as the x-axis label. Optional: Set 'percent' as TRUE if the metric is in the percent
# format, leave blank otherwise. Run and update the logistic regression model before
# plotting to ensure correct and updated predictions and probabilities in 'main'
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
    scale_color_manual(labels = c('0' = 'D', '1' = 'R'),
                       values = c('0' = 'Blue', '1' = 'Red')) +
    geom_line(aes(y = main$prob), lwd = 1.5) +
    scale_x_continuous(labels = label) +
    labs(x = xlab, y = 'P (Republicans Winning)', color = 'Party')
}

# Education
logReg <- glm(Class ~ education, family = 'binomial', data = upData)
main$prob <- predict(logReg, newdata = main, type = 'response')
main$predict <- ifelse(predict(logReg, newdata = main, type = 'response') > cutoff, 1, 0)

logPlot(education, '% Population with Bachelor\'s or Higher', percent = TRUE)

# Hispanic
logReg <- glm(Class ~ hispanic, family = 'binomial', data = upData)
main$prob <- predict(logReg, newdata = main, type = 'response')
main$predict <- ifelse(predict(logReg, newdata = main, type = 'response') > cutoff, 1, 0)

logPlot(hispanic, '% Hispanic or Latino Population', percent = TRUE)

# White
logReg <- glm(Class ~ white, family = 'binomial', data = upData)
main$prob <- predict(logReg, newdata = main, type = 'response')
main$predict <- ifelse(predict(logReg, newdata = main, type = 'response') > cutoff, 1, 0)

logPlot(white, '% White Population', percent = TRUE)

# We'll add a two more interesting demographic data to the 'main' data frame:
# AGE775214: Persons 65 years and over, percent, 2014
# POP645213: Foreign born persons, percent, 2009-2013
main <- merge(main, select(srcDemogr, fips = fips, old = AGE775214, foreign = POP645213),
              by = 'fips')

# We'll also need to update the prediction model:
trainData <- main[trainDataIndex, ]
testData <- main[- trainDataIndex, ]
upData <- upSample(x = trainData[!(names(trainData) %in% c('winner16'))],
                   y = trainData$winner16)

# Old: Notice that 'older' counties tend to heavily favor the Republican party!
logReg <- glm(Class ~ old, family = 'binomial', data = upData)
main$prob <- predict(logReg, newdata = main, type = 'response')
main$predict <- ifelse(predict(logReg, newdata = main, type = 'response') > cutoff, 1, 0)

logPlot(old, '% of Population aged 65 and over', percent = TRUE)

# Foreign: Notice that counties high in foreign born persons tend to favor Democrats.
logReg <- glm(Class ~ foreign, family = 'binomial', data = upData)
main$prob <- predict(logReg, newdata = main, type = 'response')
main$predict <- ifelse(predict(logReg, newdata = main, type = 'response') > cutoff, 1, 0)

logPlot(foreign, '% Foreign Born Persons in Population', percent = TRUE)
