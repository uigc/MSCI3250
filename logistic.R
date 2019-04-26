### Logistic Regression
library(dplyr)
library(ggplot2)
library(caret)
rm(list = ls())

## 1. Source Files
srcPresident <- read.csv('presidential_12_16.csv', stringsAsFactors = FALSE)
srcDemogr <- read.csv('county_facts.csv', stringsAsFactors = FALSE)
srcDict <- read.csv('county_facts_dictionary.csv', stringsAsFactors = FALSE)
srcRgdp <- read.csv('county_rgdp.csv', stringsAsFactors = FALSE, check.names = FALSE)

## 2. Presidential Election Data Cleanup
pres <- select(srcPresident, state = state_abbr, county = county_name,
							 votesDem12 = votes_dem_2012, votesRep12 = votes_gop_2012,
							 votesDem16 = votes_dem_2016, votesRep16 = votes_gop_2016) %>%
	mutate(county = gsub(' County', '', county))

## 3. Extract Demographic Data in Select States
demogrSomeF <- function(...) {
	states <- gsub('\"', '', toupper(sapply(substitute(list(...)), deparse)[-1]))
	
	demogrSome <- filter(srcDemogr, state_abbreviation %in% states) %>%
		select(state = state_abbreviation, county = area_name, pop14 = PST045214,
					 income = INC110213, education = EDU685213, density = POP060210,
					 white = RHI825214, hispanic = RHI725214, household = HSD310213) %>%
		mutate(county = gsub(' County', '', county))
	
	assign('demogrSome', demogrSome, envir = globalenv())
}

demogrSomeF(IA, IL, MN, NE, MI)

# Merge with the presidential election dataset
demogrSome <- inner_join(demogrSome, pres, by = c('state', 'county'))

# Merge with the RGDP dataset
demogrSome <- inner_join(demogrSome, srcRgdp, by = c('state', 'county'))

demogrSome$rgdppc14 <- demogrSome$rgdp2014 / demogrSome$pop14

demogrSome$rgdpDelta <- (demogrSome$rgdp2015 - demogrSome$rgdp2012) /
	demogrSome$rgdp2012

# Define classes: 1 if Republicans win in 2016, 0 if Democrats
demogrSome$winner16 <- ifelse(demogrSome$votesRep16 > demogrSome$votesDem16, 1 ,0) %>%
	factor(levels = c(0, 1))

## 4. Partition the Dataset
rm(trainDataIndex, trainData, testData, upData, logReg)

set.seed(42)
trainDataIndex <- createDataPartition(demogrSome$winner16, p = 0.7, list = FALSE)

trainData <- demogrSome[trainDataIndex, ]
testData <- demogrSome[- trainDataIndex, ]

table(demogrSome$winner16)
table(trainData$winner16)

upData <- upSample(x = trainData[!(names(trainData) %in% c('winner16'))],
									 y = trainData$winner16)

## 5. Logistic Regression Model
logReg <- glm(Class ~ income + education, family = 'binomial', data = upData)
summary(logReg)
exp(coef(logReg))

# Apply model on test dataset
predict <- predict(logReg, newdata = testData, type = 'response')
winPredictions <- ifelse(predict > 0.5, 1, 0)
mean(winPredictions == testData$winner16)

# Assess model fit
with(logReg, null.deviance - deviance)
with(logReg, df.null - df.residual)
with(logReg, pchisq(null.deviance - deviance, df.null - df.residual,
										lower.tail = FALSE))
