### Descriptive Analysis
library(dplyr)
library(scales)
library(ggplot2)
library(cowplot)
rm(list = ls())

## SECTION 1. Source Files
# Do not write into these files! Use separate data frames for extraction.
srcPrimary <- read.csv('primary_results.csv', stringsAsFactors = FALSE, encoding = 'UTF-8')
srcDemogr <- read.csv('county_facts.csv', stringsAsFactors = FALSE, encoding = 'UTF-8')
srcDict <- read.csv('county_facts_dictionary.csv', stringsAsFactors = FALSE, encoding = 'UTF-8')

# Load real gross domestic product (RGDP) data for all counties. We won't be using
# this data set until Section #7.
srcRgdp <- read.csv('county_rgdp.csv', stringsAsFactors = FALSE,
                    check.names = FALSE, encoding = 'UTF-8')

## SECTION 2. Cleanup Primary Results Data
# It is important to change county names to lower case (for all data sets) to prevent
# merging duplicates or errors later on; e.g. 'St Louis City' and 'St Louis city'
primary <- select(srcPrimary, fips = fips, state = state_abbreviation,
                  county = county, candidate = candidate, party = party,
                  votes = votes, fraction_votes = fraction_votes) %>%
  mutate(county = tolower(county))

## SECTION 3. Extract Demographic Data
# Refer to county_facts_dictionary:
# INC110213: Median household income, 2009-2013
# EDU685213: Bachelor's degree or higher, percent of persons age 25+, 2009-2013
# POP060210: Population per square mile, 2010
# RHI825214: White alone, not Hispanic or Latino, percent, 2014
# RHI725214: Hispanic or Latino, percent, 2014
# HSD310213: Persons per household, 2009-2013
# 
# We might be interested in certain states and not the whole nation.
# The 'demogrSomeF()' function simply returns the demographic data we have
# selected above for each county in states specified by the user.
# 'demogrSomeF()' function usage: Input state abbreviations (not full state
# names) as function arguments. Quotations or capitalizations are not necessary.
demogrSomeF <- function(...) {
  states <- gsub('\"', '', toupper(sapply(substitute(list(...)), deparse)[-1]))
  
  demogrSome <- filter(srcDemogr, state_abbreviation %in% states) %>%
    select(fips = fips, state = state_abbreviation, county = area_name,
           income = INC110213, education = EDU685213, density = POP060210,
           white = RHI825214, hispanic = RHI725214, household = HSD310213) %>%
    mutate(county = tolower(gsub(' County', '', county)))
  
  assign('demogrSome', demogrSome, envir = globalenv())
}

# We'll focus on the Midwest:
demogrSomeF(IL, IN, IA, KS, MI, MN, MO, NE, ND, OH, SD, WI)

# Merge primary results with filtered demographic data for the Midwest.
main <- merge(primary, demogrSome, by = c('fips', 'state', 'county'))

# Let's narrow down the list of candidates to some 'big names':
candidates <- c('Donald Trump', 'Ted Cruz', 'Hillary Clinton', 'Bernie Sanders')

# For each party, find the primary winners of each county in the Midwest.
winners <- rbind(
  group_by(primary, fips, state, county, party) %>%
    filter(party == 'Democrat') %>%
    summarize(winner = candidate[which.max(fraction_votes)],
              votes = max(votes),
              fraction_votes = max(fraction_votes)),
  group_by(primary, fips, state, county, party) %>%
    filter(party == 'Republican') %>%
    summarize(winner = candidate[which.max(fraction_votes)],
              votes = max(votes),
              fraction_votes = max(fraction_votes))
  ) %>% filter(winner %in% candidates)

# Merge primary winners with filtered demographic data for the Midwest.
winners <- merge(winners, demogrSome, by = c('fips', 'state', 'county'))

# Simple summary of primary winners and the average demographic they attract:
group_by(winners, winner) %>%
  summarize(income = round(mean(income)), education = round(mean(education)),
            density = round(mean(density)), white = round(mean(white)),
            hispanic = round(mean(hispanic)), household = round(mean(household))) %>%
  dplyr::rename(candidate = winner)

# Simple scatterplot of primary winners based on household and income:
ggplot(winners, aes(x = income, y = household)) +
  geom_point(aes(color = winner, size = votes)) +
  labs(y = 'Median Household Income', x = '% Persons with Bachelor\'s or higher') +
  scale_color_discrete(winners$winner, name = 'Candidate') +
  guides(size = FALSE)

# Simple boxplot of primary winners and the income demographic they attract:
ggplot(winners, aes(x = winner, y = income, fill = winner)) +
  geom_boxplot() +
  scale_y_continuous(labels = dollar) +
  labs(y = 'Median Household Income', x = 'Candidate') +
  coord_flip() +
  theme(legend.position = 'none')

## SECTION 4. Extract Candidate Data by County and Demographics
# Keep in mind that that we're still focusing on only 4 selected candidates in
# the Midwest.
# Instead of winners, we'll now focus on each candidate and their performance
# (fraction_votes) in all Midwestern counties.

# The loop below basically populates a list (cddList) with data frames.
# Each candidate we choose has a data frame containing his/her performance in all
# Midwestern counties, merged with demographic data. Each data frame can be further
# expanded column-wise as we gather more metrics.
cddList <- list()

for (i in candidates) {
  cddList[[match(i, candidates)]] <- filter(main, candidate == i)
  
  # Name each item in the list (each data frame) with the candidate's last name
  names(cddList)[match(i, candidates)] <- strsplit(i, ' ') %>%
    sapply('[[', length(unlist(strsplit(i, ' '))))
}

## 5. Build Plot Functions
# We now have a populated list of candidates and their respective vote statistics
# merged with demographic data in 'cddList'.
# 
# Next, we'll plot the fraction of votes (a performance metric) for each candidate
# against various demographic data. We'll build functions to maintain consistency,
# reduce clutter, reduce the potential for mistakes.
# 
# Usage: Input a single demographic metric in quotes; e.g. cddPlot('income')
# This metric (the function argument) will be the explanatory variable (x-axis).
cddPlot <- function(metric) {
  plot_grid(plotlist = lapply(cddList, function(df)
    ggplot(df, aes(x = eval(parse(text = metric)), y = fraction_votes)) +
      geom_point() +
      geom_smooth(method = 'lm', formula = y~x) +
      ggtitle(label = df[1, 'candidate']) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
            plot.title = element_text(size = 12))), align = 'h')
  }

# It's not exactly clear when to use a log scale, but it's probably a good idea
# when small values are compressed down to the bottom of the graph. Use this
# plotting function instead for a log transformation of the x-axis.
# Usage: Input demographic metric in quotes; e.g. cddPlotLog('income')
cddPlotLog <- function(metric) {
  plot_grid(plotlist = lapply(cddList, function(df)
    ggplot(df, aes(x = eval(parse(text = metric)), y = fraction_votes)) +
      geom_point() +
      scale_x_log10() +
      geom_smooth(method = 'lm', formula = y~x) +
      ggtitle(label = df[1, 'candidate']) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
            plot.title = element_text(size = 12))), align = 'h')
  }

# Plot fraction_votes as a function of log 'education':
# A negative slope of the regression line means that the fraction of votes in a
# county decreases as the log of percentage of population with a bachelor's degree
# or higher increases. In the case of Donald Trump, the fraction of votes for him
# tends to decrease in areas of high log 'education'. OTOH, Bernie Sanders enjoy
# high popularity in areas of high log 'education'.
# Keep in mind that this is without significance tests.
cddPlotLog('education')

# Plot fraction_votes as a function of 'income'
# Trump's popularity drastically decrease as household income increase.
cddPlot('income')

# Plot fraction_votes as a function of 'hispanic'
# Compared to other candidates, Trump seems to be slightly unpopular in areas of
# high log 'hispanic'.
cddPlotLog('hispanic')

# Plot fraction_votes as a function of 'household'
# Ted Cruz seems to be largely popular with large households.
cddPlot('household')

## 6. Regression Models
# Run a linear regression model of 'fraction_votes' as a function of 'income',
# 'hispanic', and 'household.' Store the results for each candidate in a list.
linRegResults <- list()

for (i in candidates) {
  linRegResults[[match(i, candidates)]] <-
    summary(lm(fraction_votes~income + hispanic + household,
               data = cddList[[strsplit(i, ' ') %>%
                                 sapply('[[', length(unlist(strsplit(i, ' '))))]]
               ))
	
	names(linRegResults)[match(i, candidates)] <- strsplit(i, ' ') %>%
	  sapply('[[', length(unlist(strsplit(i, ' '))))
}

# The results can accessed via each candidate's last name.
linRegResults['Trump']

## 7. Incorporating Real Gross Domestic Product
# Join the demographic data of the 5 states with the real GDP dataset.
demogrSome <- inner_join(demogrSome, srcRgdp, by = c('state', 'county'))

# Compute RGDP per capita. Since we're only provided with 2014 population data
# in 'srcDemogr', we'll compute RGDP per capita in 2014.
demogrSome$rgdppc14 <- demogrSome$rgdp2014 / demogrSome$pop14

# We can derive trends from annual data. A simple trend would be to calculate
# the decimal real GDP change from 2012 to 2015.
demogrSome$rgdpDelta <- (demogrSome$rgdp2015 - demogrSome$rgdp2012) /
  demogrSome$rgdp2012

# We may need to perform log transformations, so negative values (decreasing
# real GDP) will be ignored (undefined)! We'll normalize 'rgdpDelta' by
# rescaling it from 0 to 1.
demogrSome$rgdpDeltaNorm <- (demogrSome$rgdpDelta - min(demogrSome$rgdpDelta)) /
  (max(demogrSome$rgdpDelta) - min(demogrSome$rgdpDelta))

# Join the list of candidates with vote and demographic data 'cddList' we
# created earlier with all real gdp data.
for (i in seq(length(cddList))) {
  cddList[[i]] <- merge(cddList[[i]], demogrSome)
}

# Notice Ted Cruz's popularity decline in areas of high RGDP per capita.
cddPlotLog('rgdppc14')

# Notice Donald Trump's popularity increase in areas of increasing RGDP from
# 2012 to 2015. Earlier, we noticed that Donald Trump's probability of winning
# increases as the median household income decreases. We can infer that Trump's
# chance of winning increases in areas of low income and increasing RGDP.
# Important: This is within our 5 chosen states.
cddPlotLog('rgdpDeltaNorm')

# To be safe, we'll perform a correlation test to make sure that the explanatory
# variables 'income' and 'rgdpDelta' are not significantly correlated to each other.
# The null hypothesis is that the true correlation is equal to 0. Since the P-value
# is 0.2834, we cannot reject the null hypothesis and must conclude that there is
# no significant correlation. This is a simple test for multicollinearity.
cor.test(demogrSome$income, demogrSome$rgdpDelta, method = 'pearson')

# With no significant correlation between predictor variables 'income' and
# 'rgdpDelta', proceed with multiple regression and examine the output:
summary(lm(fraction_votes~income + rgdpDelta, data = cddList[['Trump']]))

# Join each party with our updated 'demogrSome' data frame.
combdRep <- inner_join(demogrSome, votesRep, by = c('state', 'county'))
combdDem <- inner_join(demogrSome, votesDem, by = c('state', 'county'))

# Simple boxplot of Republican winners as a function of real GDP per capita.
# We can remove some outliers using 'boxplot.stats(df$y)$stats[c(1, 5)]'
ggplot(combdRep, aes(x = winner, y = rgdppc14, fill = winner)) +
  geom_boxplot() +
  scale_y_continuous(labels = dollar,
                     limits = boxplot.stats(combdRep$rgdppc14)$stats[c(1, 5)]) +
	labs(y = 'Real GDP Per Capita (2014)', x = 'Candidate') +
  coord_flip() +
  theme(legend.position = 'none')

# Boxplot of Republican winners as a function of real GDP change from 2012-2015.
ggplot(combdRep, aes(x = winner, y = rgdpDelta, fill = winner)) +
  geom_boxplot() +
  scale_y_continuous(labels = percent,
                     limits = boxplot.stats(combdRep$rgdpDelta)$stats[c(1, 5)]) +
  labs(y = expression(Delta * ' Real GDP 2012-2015'), x = 'Candidate') +
  coord_flip() +
  theme(legend.position = 'none')
