### 1. Descriptive Analysis - Primaries
## Refer to https://github.com/shenlim/MSCI3250/wiki
library(dplyr)
library(scales)
library(ggplot2)
library(cowplot)
rm(list = ls())

## SECTION 1. Source Files
# Do not write into these files! Use separate data frames for extraction.
srcPrimary <- read.csv('county_primary.csv', stringsAsFactors = FALSE, encoding = 'UTF-8')
srcDemogr <- read.csv('county_facts.csv', stringsAsFactors = FALSE, encoding = 'UTF-8')
srcDict <- read.csv('county_facts_dictionary.csv', stringsAsFactors = FALSE, encoding = 'UTF-8')

# Load county real gross domestic product (RGDP) and population data.
# We'll use these data sets in Section #6.
srcRgdp <- read.csv('county_rgdp.csv', stringsAsFactors = FALSE, encoding = 'UTF-8')
srcPop <- read.csv('county_population.csv', stringsAsFactors = FALSE, encoding = 'UTF-8')

## SECTION 2. Cleanup Primary Results Data
# It's important to change county names to lower case (for all data sets) to prevent future
# merging duplicates or errors; e.g. 'St Louis City' vs 'St Louis city'
primary <- select(srcPrimary, fips, state = state_abbreviation, county,
                  candidate, party, votes, fraction_votes) %>% mutate(county = tolower(county))

## SECTION 3. Extract Demographic Data
# Refer to county_facts_dictionary, we focus on these for now:
# INC110213: Median household income, 2009-2013
# EDU685213: Bachelor's degree or higher, percent of persons age 25+, 2009-2013
# POP060210: Population per square mile, 2010
# RHI825214: White alone, not Hispanic or Latino, percent, 2014
# RHI725214: Hispanic or Latino, percent, 2014
# HSD310213: Persons per household, 2009-2013
# 
# We might be interested in certain states/region and not the whole nation at once. The 
# 'demogrSomeF()' function returns the county demographic data we selected above in states
# specified by the user. Function usage: Input state abbreviations (not full state names) as
# function arguments. Quotations or capitalizations are not necessary.
demogrSomeF <- function(...) {
  states <- gsub('\"', '', toupper(sapply(substitute(list(...)), deparse)[-1]))
  
  demogrSome <- filter(srcDemogr, state_abbreviation %in% states) %>%
    select(fips, state = state_abbreviation, county = area_name,
           income = INC110213, education = EDU685213, density = POP060210,
           white = RHI825214, hispanic = RHI725214, household = HSD310213) %>%
    mutate(county = tolower(gsub(' County', '', county)))
  
  assign('demogrSome', demogrSome, envir = globalenv())
}

# Select one of four US regions:
# Northeast:
demogrSomeF(CT, ME, MA, NH, NJ, NY, PA, RI, VT)
# South:
demogrSomeF(AL, AR, DE, FL, GA, KY, LA, MD, MS, NC, OK, SC, TN, TX, VA, WV)
# West:
demogrSomeF(AK, AZ, CA, CO, HI, ID, MT, NV, NM, OR, UT, WA, WY)
# Midwest:
demogrSomeF(IL, IN, IA, KS, MI, MN, MO, NE, ND, OH, SD, WI)

# Important: The rest of the script were tested using 'Midwest' as the region. As such, the
# model interpretations may be different if the user uses a different region. For the first
# run-through, using the 'Midwest' region is recommended.
# Let's narrow down the list of candidates to some 'big names':
candidates <- c('Donald Trump', 'Ted Cruz', 'Hillary Clinton', 'Bernie Sanders')

# Merge primary results with filtered county demographic data for the region.
main <- merge(primary, demogrSome, by = c('fips', 'state', 'county')) %>%
  filter(candidate %in% candidates)

# For each party, find the primary winners of each county in the region.
winners <- rbind(
  group_by(primary, fips, state, county, party) %>% filter(party == 'Democrat') %>%
    summarize(winner = candidate[which.max(fraction_votes)],
              votes = max(votes),
              fraction_votes = max(fraction_votes)),
  group_by(primary, fips, state, county, party) %>% filter(party == 'Republican') %>%
    summarize(winner = candidate[which.max(fraction_votes)],
              votes = max(votes),
              fraction_votes = max(fraction_votes))
  ) %>% filter(winner %in% candidates)

# Merge primary winners with filtered demographic data for the region.
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
  scale_y_continuous(labels = dollar,
                     limits = boxplot.stats(winners$income)$stats[c(1, 5)]) +
  labs(y = 'Median Household Income', x = 'Candidate') +
  coord_flip() +
  theme(legend.position = 'none')

# Clear unused objects
rm(demogrSome, primary)

## SECTION 4. Extract Candidate Data by County and Demographics
# Keep in mind that that we're still focusing on only 4 big candidates in the region (Midwest
# for this example). Instead of winners, we'll now focus on each candidate and their performance
# (fraction_votes) in all Midwestern counties.

# The loop below populates the list 'cddList' with data frames. Each of the 4 candidates
# has a data frame containing his/her performance in all counties within the region, merged
# with county demographic data.
cddList <- list()

for (i in candidates) {
  cddList[[match(i, candidates)]] <- filter(main, candidate == i)
  
  # Name each item in the list (each data frame) with the candidate's last name
  names(cddList)[match(i, candidates)] <- strsplit(i, ' ') %>%
    sapply('[[', length(unlist(strsplit(i, ' '))))
  
  rm (i)
}

## SECTION 5. Build Plot Functions
# We now have a populated list of candidates and their respective vote statistics merged
# with demographics in 'cddList'.
# 
# Next, we'll plot the fraction of votes (a performance metric) for each candidate
# against various demographic metrics. We'll build functions to maintain consistency,
# reduce clutter, reduce the potential for mistakes.
# 
# Usage: Input a single demographic metric in quotes; e.g. cddPlot('income')
# The metric (the function argument) will be the explanatory variable (x-axis).
cddPlot <- function(metric) {
  plot_grid(plotlist = lapply(cddList, function(df)
    ggplot(df, aes(x = eval(parse(text = metric)), y = fraction_votes)) +
      geom_point() +
      geom_smooth(method = 'lm', formula = y ~ x) +
      ggtitle(label = df[1, 'candidate']) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
            plot.title = element_text(size = 12))), align = 'h')
  }

# It's not exactly clear when to use a log scale, but it's probably a good idea when small
# values are compressed down to the bottom of the graph. Use this plotting function instead
# for a log transformation of the x-axis.
# Usage: Input demographic metric in quotes; e.g. cddPlotLog('income')
cddPlotLog <- function(metric) {
  plot_grid(plotlist = lapply(cddList, function(df)
    ggplot(df, aes(x = eval(parse(text = metric)), y = fraction_votes)) +
      geom_point() +
      scale_x_log10() +
      geom_smooth(method = 'lm', formula = y ~ x) +
      ggtitle(label = df[1, 'candidate']) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
            plot.title = element_text(size = 12))), align = 'h')
  }

# Plot fraction_votes as a function of log 'education':
# A negative slope of the regression line means that the fraction of votes in a county
# decreases as the log of percentage of population with a bachelor's degree or higher
# increases. In the case of Donald Trump, the fraction of votes for him tends to decrease
# in areas of high log 'education'. OTOH, Bernie Sanders enjoy high popularity in areas of
# high log 'education'. Keep in mind that this is without significance tests.
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

## SECTION 6. Incorporating RGDP and Population
# Join the region demographic data with the RGDP and population data sets.
main <- inner_join(inner_join(main, mutate(srcRgdp, county = tolower(county)),
                              by = c('state', 'county')),
                   mutate(srcPop, county = tolower(county)), by = c('fips', 'county'))

# Compute 2012 and 2015 RGDP per capita.
main$rgdppc12 <- main$rgdp2012 / main$pop2012
main$rgdppc15 <- main$rgdp2015 / main$pop2015

# We can derive trends from annual data. A simple trend would be to calculate RGDP per
# capita change from 2012 to 2015 (decimals).
main$rgdppcDelta <- (main$rgdppc15 - main$rgdppc12) / main$rgdppc12

# We may need to perform log transformations, but negative values (decreasing trend) will
# will be ignored (undefined)! We'll normalize 'rgdpDelta' by rescaling it from 0 to 1.
main$rgdppcDeltaNorm <- (main$rgdppcDelta - min(main$rgdppcDelta)) /
  (max(main$rgdppcDelta) - min(main$rgdppcDelta))

# Join (update) the list of candidates 'cddList' we created earlier with new data.
for (i in seq(length(cddList))) {
  cddList[[i]] <- merge(cddList[[i]], main)
  rm(i)
}

# Notice Ted Cruz's popularity decline in areas of high log RGDP per capita.
cddPlotLog('rgdppc15')

# Notice Donald Trump's popularity increase in areas of increasing log RGDP per capita from
# 2012-2015. Earlier, we noticed that Donald Trump's probability of winning increases as
# median household income decreases. We can infer that Trump's chance of winning is high
# in areas of low income but increasing RGDP per capita.
cddPlotLog('rgdppcDeltaNorm')

# To be safe, we'll perform a correlation test to make sure that the explanatory variables
# 'income' and 'rgdppcDelta' are not significantly correlated to each other. The null
# hypothesis is that the true correlation is equal to 0. Since the P-value is 0.24, we
# cannot reject the null hypothesis and must conclude that there is no significant
# correlation. This is a simple test for multicollinearity.
cor.test(main$income, main$rgdppcDelta, method = 'pearson')

# With no significant correlation between predictor variables 'income' and 'rgdppcDelta',
# we'll proceed with multiple regression and examine the output:
summary(lm(fraction_votes ~ income + rgdppcDelta, data = cddList[['Trump']]))

# Join each party with our updated 'main' data frame.
winners <- merge(winners, main)

# Simple boxplot of Republican winners as a function of 2015 RGDP per capita.
# We can remove some outliers using 'boxplot.stats(df$y)$stats[c(1, 5)]'
ggplot(winners, aes(x = winner, y = rgdppc15, fill = winner)) +
  geom_boxplot() +
  scale_y_continuous(labels = dollar,
                     limits = boxplot.stats(winners$rgdppc15)$stats[c(1, 5)]) +
	labs(y = 'Real GDP Per Capita (2015)', x = 'Candidate') +
  coord_flip() +
  theme(legend.position = 'none')

# Boxplot of Republican winners as a function of RGDP change from 2012-2015.
ggplot(winners, aes(x = winner, y = rgdppcDelta, fill = winner)) +
  geom_boxplot() +
  scale_y_continuous(labels = percent,
                     limits = boxplot.stats(winners$rgdppcDelta)$stats[c(1, 5)]) +
  labs(y = expression(Delta * ' Real GDP 2012-2015'), x = 'Candidate') +
  coord_flip() +
  theme(legend.position = 'none')
