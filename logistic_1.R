### 2. Logistic Regression Part 1 - Presidential Election (Republicans)
## Refer to https://github.com/shenlim/MSCI3250/wiki/Logistic-Regression
library(dplyr)
library(ggplot2)
library(scales)
library(caret)
library(choroplethr)
library(choroplethrMaps)
library(cowplot)
rm(list = ls())

## SECTION 1. Source Files
srcPresident <- read.csv('county_presidential.csv', stringsAsFactors = FALSE)
srcDemogr <- read.csv('county_facts.csv', stringsAsFactors = FALSE)
srcDict <- read.csv('county_facts_dictionary.csv', stringsAsFactors = FALSE)
srcRgdp <- read.csv('county_rgdp.csv', stringsAsFactors = FALSE, check.names = FALSE)
srcUnemp <- read.csv('county_unemployment.csv', stringsAsFactors = FALSE)
srcPop <- read.csv('county_population.csv', stringsAsFactors = FALSE)

## SECTION 2. Data Cleanup and Merging
pres <- select(srcPresident, fips = combined_fips, state = state_abbr, county = county_name,
               votesDem12 = votes_dem_2012, votesRep12 = votes_gop_2012,
               votesDem16 = votes_dem_2016, votesRep16 = votes_gop_2016) %>%
  mutate(county = tolower(gsub(' County', '', county)))

demogrSomeF <- function(...) {
  states <- gsub('\"', '', toupper(sapply(substitute(list(...)), deparse)[-1]))
	
	demogrSome <- filter(srcDemogr, state_abbreviation %in% states) %>%
	  select(state = state_abbreviation, county = area_name, income = INC110213,
	         education = EDU685213, white = RHI825214, hispanic = RHI725214,
	         old = AGE775214, foreign = POP645213) %>%
	  mutate(county = tolower(gsub(' County', '', county)))
	
	assign('demogrSome', demogrSome, envir = globalenv())
}

# Select one of four US regions:
# Northeast (9):
demogrSomeF(CT, ME, MA, NH, NJ, NY, PA, RI, VT)
# Proper state names for choroplethR 'state_zoom':
states <- c('connecticut', 'maine', 'massachusetts', 'new hampshire', 'new jersey',
            'new york', 'pennsylvania', 'rhode island', 'vermont')

# South (16):
demogrSomeF(AL, AR, DE, FL, GA, KY, LA, MD, MS, NC, OK, SC, TN, TX, VA, WV)
# Proper state names for choroplethR 'state_zoom':
states <- c('alabama', 'arkansas', 'delaware', 'florida', 'georgia', 'kentucky', 'louisiana',
            'maryland', 'mississippi', 'north carolina', 'oklahoma', 'south carolina',
            'tennessee', 'texas', 'virginia', 'west virginia')

# West (13):
demogrSomeF(AK, AZ, CA, CO, HI, ID, MT, NV, NM, OR, UT, WA, WY)
# Proper state names for choroplethR 'state_zoom':
states <- c('alaska', 'arizona', 'california', 'colorado', 'hawaii', 'idaho', 'montana',
            'nevada', 'new mexico', 'oregon', 'utah', 'washington', 'wyoming')

# Midwest (12):
demogrSomeF(IL, IN, IA, KS, MI, MN, MO, NE, ND, OH, SD, WI)
# Proper state names for choroplethR 'state_zoom':
states <- c('illinois', 'indiana', 'iowa', 'kansas', 'michigan', 'minnesota', 'missouri',
            'nebraska', 'north dakota', 'ohio', 'south dakota', 'wisconsin')

# Important: The rest of the script were tested using 'Midwest' as the region. As such, the
# model interpretations may be different if the user uses a different region. For the first
# run-through, using the 'Midwest' region is recommended.
# Merge other data sets
unemp <- mutate(srcUnemp, county = tolower(county))
rgdp <- mutate(srcRgdp, county = tolower(county))
pop <- mutate(srcPop, county = tolower(county))

main <- merge(merge(merge(merge(pres, demogrSome), unemp), rgdp), pop)

# Remove objects we'll not be using
rm(list = c('pres', 'unemp', 'rgdp', 'pop', ls(pattern = '^src|^demo')))

## SECTION 3. Calculate Unemployment Change, Rgdp Change, 2016 winner
main$unempDelta <- (main$unemp_rate15 - main$unemp_rate12) / main$unemp_rate12

main$rgdppcDelta <- ((main$rgdp2015 / main$pop2015) - (main$rgdp2012 / main$pop2012)) /
  (main$rgdp2012 / main$pop2012)

main$winner16 <- ifelse(main$votesRep16 > main$votesDem16, 1, 0) %>%
  factor(levels = c(0, 1))

## SECTION 4. Partition the Dataset
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

## SECTION 5. Logistic Regression Model
logReg <- glm(Class ~ income + education, family = 'binomial', data = upData)
summary(logReg)
# Intepret coefficients as odds factors by exponentiating the log of odds
exp(coef(logReg))

# Apply model on test dataset and determine accuracy
predict <- predict(logReg, newdata = testData, type = 'response')
cutoff <- table(main$winner16)['0'][[1]] / nrow(main)
winPredictions <- ifelse(predict > cutoff, 1, 0)
mean(winPredictions == testData$winner16)

# Assess model fit. Is the model significantly better than a null model (0 predictors)?
# Chi-Square Test
with(logReg, null.deviance - deviance)
# Degrees of freedom
with(logReg, df.null - df.residual)
# Chi-Square P-Value
with(logReg, pchisq(null.deviance - deviance, df.null - df.residual,
                    lower.tail = FALSE))

## SECTION 6. Map Visualization
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

## SECTION 7. Plot Logistic Regression Models
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
    geom_point(shape = 21, size = 2, aes(fill = as.factor(predict))) +
    scale_fill_manual(values = c('1' = '#F8766D', '0' = '#619CFF'),
                      labels = c('1' = 'R', '0' = 'D')) +
    geom_line(aes(y = main$prob, color = state), lwd = 1.3) +
    scale_x_continuous(labels = label) +
    labs(x = xlab, y = 'P (Republicans Winning)',
         fill = 'Party', color = 'State')
}

# Income
# Notice the relatively horizontal and flat sigmoid curve. There is no significant correlation
# or pattern between median income and the probability of Republicans winning.
logReg <- glm(Class ~ income, family = 'binomial', data = upData)
main$prob <- predict(logReg, newdata = main, type = 'response')
main$predict <- ifelse(predict(logReg, newdata = main, type = 'response') > cutoff, 1, 0)

logPlot(income, 'Median Income ($)', percent = FALSE)
ggsave(filename = 'plot_log_rep_income.png', plot = last_plot(), width = 10, height = 6)

# Education
# The probability of Republicans winning decreases as the % of population with a Bachelor's
# degree or higher increases.
logReg <- glm(Class ~ education, family = 'binomial', data = upData)
main$prob <- predict(logReg, newdata = main, type = 'response')
main$predict <- ifelse(predict(logReg, newdata = main, type = 'response') > cutoff, 1, 0)

logPlot(education, '% Population with Bachelor\'s or Higher', percent = TRUE)
ggsave(filename = 'plot_log_rep_education.png', plot = last_plot(), width = 10, height = 6)

# Hispanic
# The correlation between Hispanic populations and P(Republican Win) is relatively insignificant.
# Counties with higher % of Hispanics in population tend to disfavor the Republican party.
logReg <- glm(Class ~ hispanic, family = 'binomial', data = upData)
main$prob <- predict(logReg, newdata = main, type = 'response')
main$predict <- ifelse(predict(logReg, newdata = main, type = 'response') > cutoff, 1, 0)

logPlot(hispanic, '% Hispanic or Latino Population', percent = TRUE)
ggsave(filename = 'plot_log_rep_hispanic.png', plot = last_plot(), width = 10, height = 6)

# White
# The probability of Republicans winning increases in areas of very high % of whites in population.
logReg <- glm(Class ~ white, family = 'binomial', data = upData)
main$prob <- predict(logReg, newdata = main, type = 'response')
main$predict <- ifelse(predict(logReg, newdata = main, type = 'response') > cutoff, 1, 0)

logPlot(white, '% White Population', percent = TRUE)
ggsave(filename = 'plot_log_rep_white.png', plot = last_plot(), width = 10, height = 6)

# Old
# Notice that 'older' counties tend to heavily favor the Republican party!
logReg <- glm(Class ~ old, family = 'binomial', data = upData)
main$prob <- predict(logReg, newdata = main, type = 'response')
main$predict <- ifelse(predict(logReg, newdata = main, type = 'response') > cutoff, 1, 0)

logPlot(old, '% of Population aged 65 and over', percent = TRUE)
ggsave(filename = 'plot_log_rep_old.png', plot = last_plot(), width = 10, height = 6)

# Foreign
# Notice that counties high in foreign born persons tend to favor Democrats.
logReg <- glm(Class ~ foreign, family = 'binomial', data = upData)
main$prob <- predict(logReg, newdata = main, type = 'response')
main$predict <- ifelse(predict(logReg, newdata = main, type = 'response') > cutoff, 1, 0)

logPlot(foreign, '% Foreign Born Persons in Population', percent = TRUE)
ggsave(filename = 'plot_log_rep_foreign.png', plot = last_plot(), width = 10, height = 6)
