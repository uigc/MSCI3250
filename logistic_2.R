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
srcPop <- read.csv('county_population.csv', stringsAsFactors = FALSE)
srcVet <- read.csv('county_veterans.csv', strip.white = FALSE)

## SECTION 2. Data Merging and Calculations
demogrSomeF <- function(...) {
  states <- gsub('\"', '', toupper(sapply(substitute(list(...)), deparse)[-1]))
  
  demogrSome <- filter(srcDemogr, state_abbreviation %in% states) %>%
    select(fips, state = state_abbreviation, county = area_name, income = INC110213,
           education = EDU685213, old = AGE775214, foreign = POP645213) %>%
    mutate(county = tolower(gsub(' County', '', county)))
  
  assign('demogrSome', demogrSome, envir = globalenv())
}
# Select one of four US regions:
# Midwest:
demogrSomeF(IL, IN, IA, KS, MI, MN, MO, NE, ND, OH, SD, WI)
# Northeast:
demogrSomeF(CT, ME, MA, NH, NJ, NY, PA, RI, VT)
# South:
demogrSomeF(AL, AR, DE, FL, GA, KY, LA, MD, MS, NC, OK, SC, TN, TX, VA, WV)
# West:
demogrSomeF(AK, AZ, CA, CO, HI, ID, MT, NV, NM, OR, UT, WA, WY)

main <- merge(merge(demogrSome, select(srcElections, -c('state', 'county')), by = 'fips'),
              select(srcUnemp, -c('state', 'county')), by = 'fips')

vets <- (merge(select(srcPop, fips, pop2015, pop2016),
               select(srcVet, fips, vet2015, vet2016), by = 'fips'))

vets$vet2015 <- vets$vet2015 / vets$pop2015 * 100
vets$vet2016 <- vets$vet2016 / vets$pop2016 * 100

main <- merge(main, select(vets, fips, vet2015, vet2016), by = 'fips')

rm(demogrSome, vets)

attach(main)
main$trumpWin <- ifelse(trump16 > clinton16 & trump16 > otherpres16, 1, 0) %>%
  factor(levels = c(0, 1))

main$obamaVotes <- obama12 / (romney12 + obama12 + otherpres12) * 100

main$romneyVotes <- romney12 / (romney12 + obama12 + otherpres12) * 100

main$repDelta <- ((trump16 / (trump16 + clinton16 + otherpres16)) -
                    (romney12 / (romney12 + obama12 + otherpres12))) /
  (romney12 / (romney12 + obama12 + otherpres12))

main$cutoff <- 1 - (trump16 / (trump16 + clinton16 + otherpres16))

main$trumpVotes <- trump16 / (trump16 + clinton16 + otherpres16)

main$unempDelta <- (unemp_rate16 - unemp_rate12) / unemp_rate12

main$unempDeltaNorm <- (unempDelta - min(unempDelta)) / (max(unempDelta) - min(unempDelta))
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
logReg <- glm(Class ~ vet, family = 'binomial', data = upData)
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
    geom_point(shape = 21, size = 2, aes(fill = as.factor(predict))) +
    scale_fill_manual(values = c('1' = '#F8766D', '0' = '#619CFF'),
                      labels = c('1' = 'Win', '0' = 'Lose')) +
    geom_line(aes(y = main$prob, color = state), lwd = 1.3) +
    scale_x_continuous(labels = label) +
    labs(x = xlab, y = 'P (Republicans Winning)',
         fill = 'Trump', color = 'State')
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

# Veterans
logReg <- glm(Class ~ vet2016, family = 'binomial', data = upData)
main$prob <- predict(logReg, newdata = main, type = 'response')
main$predict <- ifelse(predict(logReg, newdata = main, type = 'response') > main$cutoff, 1, 0)

logPlot(vet2016, '% Veterans in Population', percent = TRUE)

## SECTION 6. Plot Linear Regression Models
main[, c('cutoff', 'prob', 'predict', 'trumpWin')] <- NULL

# Counties with increasing unemployment rates from 2012 to 2015 tend to favor Trump (log).
linReg <- lm(trumpVotes ~ unempDeltaNorm, data = main)
summary(linReg)

ggplot(main, aes(x = unempDeltaNorm, y = trumpVotes)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  scale_x_log10() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = 'Log Change in Unemployment 2012-2015 (Rescaled 0-1)', y = '% Votes for Trump')
ggsave(filename = 'plot_lin_unemp.png', plot = last_plot(), width = 10, height = 6)

# The elderly tend to favor Trump.
linReg <- lm(trumpVotes ~ old, data = main)
summary(linReg)

ggplot(main, aes(x = old / 100, y = trumpVotes)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = '% Population Aged 65 and over', y = '% Votes for Trump')
ggsave(filename = 'plot_lin_old.png', plot = last_plot(), width = 10, height = 6)

# Foreigners tend to disfavor Trump (log).
linReg <- lm(trumpVotes ~ foreign, data = main)
summary(linReg)

ggplot(main, aes(x = foreign / 100, y = trumpVotes)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  scale_x_log10(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = 'Log % Foreigners in Population', y = '% Votes for Trump')
ggsave(filename = 'plot_lin_foreign.png', plot = last_plot(), width = 10, height = 6)

# Veterans tend to favor Trump (log).
linReg <- lm(trumpVotes ~ vet2016, data = main)
summary(linReg)

ggplot(main, aes(x = vet2016 / 100, y = trumpVotes)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  scale_x_log10(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = 'Log % Veterans in Population', y = '% Votes for Trump')
ggsave(filename = 'plot_lin_vet.png', plot = last_plot(), width = 10, height = 6)

# Separate by State
ggplot(main, aes(x = old / 100, y = trumpVotes, color = state)) +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, lwd = 1.2) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = '% Population Aged 65 and over', y = '% Votes for Trump',
       color = 'State')
ggsave(filename = 'plot_lin_old_state.png', plot = last_plot(), width = 10, height = 6)

ggplot(main, aes(x = foreign / 100, y = trumpVotes, color = state)) +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, lwd = 1.2) +
  scale_x_log10(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = 'Log % Foreigners in Population', y = '% Votes for Trump',
       color = 'State')
ggsave(filename = 'plot_lin_foreign_state.png', plot = last_plot(), width = 10, height = 6)

ggplot(main, aes(x = vet2016 / 100, y = trumpVotes, color = state)) +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, lwd = 1.2) +
  scale_x_log10(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = 'Log % Veterans in Population', y = '% Votes for Trump',
       color = 'State')
ggsave(filename = 'plot_lin_vet_state.png', plot = last_plot(), width = 10, height = 6)
