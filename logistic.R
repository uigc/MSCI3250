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
srcPresident <- read.csv('presidential_12_16.csv', stringsAsFactors = FALSE)
srcDemogr <- read.csv('county_facts.csv', stringsAsFactors = FALSE)
srcDict <- read.csv('county_facts_dictionary.csv', stringsAsFactors = FALSE)
srcRgdp <- read.csv('county_rgdp.csv', stringsAsFactors = FALSE, check.names = FALSE)
srcUnemp <- read.csv('unemp.csv', stringsAsFactors = FALSE)
srcPop <- read.csv('pop.csv', stringsAsFactors = FALSE)

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

main$rgdppcDelta <- ((main$rgdp2015 / main$pop15) - (main$rgdp2012 / main$pop12)) /
  (main$rgdp2012 / main$pop12)

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
# Check parameters before generating plot:
logReg <- glm(Class ~ white, family = 'binomial', data = upData)
predict <- predict(logReg, newdata = main, type = 'response')
main$prob <- predict
main$predict <- ifelse(predict > cutoff, 1, 0)
table(main$predict)

# Education
ggplot(main, aes(x = education / 100, y = predict)) +
  geom_point(shape = 1, aes(color = as.factor(predict))) +
  scale_color_manual(labels = c('0' = 'D', '1' = 'R'),
                     values = c('0' = 'Blue', '1' = 'Red')) +
  geom_line(aes(y = main$prob), lwd = 1.5) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = '% Population with Bachelor\'s or Higher',
       y = 'P (Republicans Winning)', color = 'Party')

# Hispanic
ggplot(main, aes(x = hispanic / 100, y = predict)) +
  geom_point(shape = 1, aes(color = as.factor(predict))) +
  scale_color_manual(labels = c('0' = 'D', '1' = 'R'),
                     values = c('0' = 'Blue', '1' = 'Red')) +
  geom_line(aes(y = main$prob), lwd = 1.5) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = '% Hispanic or Latino Population',
       y = 'P (Republicans Winning)', color = 'Party')

# White
ggplot(main, aes(x = white / 100, y = predict)) +
  geom_point(shape = 1, aes(color = as.factor(predict))) +
  scale_color_manual(labels = c('0' = 'D', '1' = 'R'),
                     values = c('0' = 'Blue', '1' = 'Red')) +
  geom_line(aes(y = main$prob), lwd = 1.5) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = '% White Population',
       y = 'P (Republicans Winning)', color = 'Party')
