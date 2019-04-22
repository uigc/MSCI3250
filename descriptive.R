### Descriptive analysis
library(dplyr)
library(ggplot2)
library(cowplot)
rm(list = ls())

## Load source files--do not write into
src.primary <- read.csv('primary_results.csv', stringsAsFactors = F)
src.demogr <- read.csv('county_facts.csv', stringsAsFactors = F)
src.dict <- read.csv('county_facts_dictionary.csv', stringsAsFactors = F)

## Extract winners and vote statistics
for (i in levels(as.factor(src.primary$party))) {
  assign(paste('votes', substring(i, 1, 3), sep = ''),
         group_by(src.primary, state_abbreviation, county, party) %>%
           filter(party == i & votes != 0 & fraction_votes != 0) %>%
           summarize(winner = candidate[which.max(fraction_votes)],
                     votes = max(votes),
                     fraction_votes = max(fraction_votes)) %>%
           rename(state = state_abbreviation))
  rm(i)
}

## Extract some demographic data--refer to county_facts_dictionary:
# INC110213: Median household income, 2009-2013
# EDU685213: Bachelor's degree or higher, percent of persons age 25+, 2009-2013
# POP060210: Population per square mile, 2010
# RHI825214: White alone, not Hispanic or Latino, percent, 2014
# RHI225214: Black or African American alone, percent, 2014
# RHI725214: Hispanic or Latino, percent, 2014
# HSD310213: Persons per household, 2009-2013
# 
# 'demoTest' function usage: Input state abbreviations (not full state names)
# as function arguments. Quotations or capitalizations are not necessary.
demoTest <- function(...) {
  states <- gsub('\"', '', toupper(sapply(substitute(list(...)), deparse)[-1]))
  
  demogrSome <- filter(src.demogr, state_abbreviation %in% states) %>%
    select(state = state_abbreviation, county = area_name,
           income = INC110213, education = EDU685213, density = POP060210,
           white = RHI825214, hispanic = RHI725214, household = HSD310213) %>%
    mutate(county = gsub(' County', '', county))
  
  assign('demogrSome', demogrSome, envir = globalenv())
}

demoTest(IA, IL, MN, NE, MI)

demogrAll <- select(src.demogr, state = state_abbreviation, county = area_name,
                    income = INC110213, education = EDU685213, density = POP060210,
                    white = RHI825214, hispanic = RHI725214,  household = HSD310213) %>%
  mutate(county = gsub(' County', '', county))

combdRep <- inner_join(demogrSome, votesRep, by = c('state', 'county'))
combdDem <- inner_join(demogrSome, votesDem, by = c('state', 'county'))

combdAll <- rbind(combdRep, combdDem)

winners <- group_by(combdAll, winner, party) %>%
  summarize(income = round(mean(income)), education = round(mean(education)),
            density = round(mean(density)), white = round(mean(white)),
            hispanic = round(mean(hispanic)), household = round(mean(household)))

ggplot(combdRep, aes(x = income, y = household)) +
  geom_point(aes(color = winner, size = votes))

ggplot(combdRep, aes(x = winner, y = household, fill = winner)) +
  geom_boxplot() +
  coord_flip()

## Candidates to analyze
candidates <- c('Donald Trump', 'Ted Cruz', 'Hillary Clinton', 'Bernie Sanders')
cddList <- list()

for (i in candidates) {
  cddList[[match(i, candidates)]] <- filter(src.primary, candidate == i) %>%
    select(state = state_abbreviation, county = county, party = party,
           candidate = candidate, votes = votes, fraction_votes = fraction_votes) %>%
    inner_join(demogrSome, by = c('state', 'county'))
  
  names(cddList)[match(i, candidates)] <- strsplit(i, ' ') %>%
    sapply('[[', length(unlist(strsplit(i, ' '))))
}

## We now have a populated list of candidates and their respective vote
# statistics (merged with demographic metrics) in cddList.
# The next step is to plot the fraction of votes (a performance metric)
# against various demographic metrics. There are plenty of repeatable
# codes here so we will build a function to reduce clutter.
# Usage: Input demographic metric in quotes; e.g. cddPlot('income')
cddPlot <- function(metric) {
  plot_grid(plotlist = lapply(cddList, function(df)
    ggplot(df, aes(x = eval(parse(text = metric)), y = fraction_votes)) +
      geom_point() +
      geom_smooth(method = 'lm', formula = y~x)),
    labels = names(cddList), align = 'h',
    label_x = 0, label_y = 0, hjust = -0.5, vjust = -1.5)
}

# Plot fraction_votes as a function of 'education':
# A negative slope of the regression line means that the fraction of votes
# in a county decreases as the percentage of population with a bachelor's
# degree or higher increases. In the case of Donald Trump, the fraction of
# votes for him tends to decrease in areas of high 'education'.
# Keep in mind that this is without significance tests.
cddPlot('education')

# Plot fraction_votes as a function of 'income'
# Donald Trump's popularity drastically decreases as median household income
# increases.
cddPlot('income')

# Plot fraction_votes as a function of 'hispanic'
# Interestingly, Donald Trump enjoys some popularity in areas of significant
# Hispanic populations, as opposed to Ted Cruz.
cddPlot('hispanic')
