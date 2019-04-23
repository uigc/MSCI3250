## MSCI:3250 - Analyzing Data for Business Intelligence - Final Project

## Objective
To search for patterns and correlations between demographics, economic situation and political leanings in certain areas.

Some areas of discovery:
* Are there any significant correlation between various demographic/economic data and candidate performance?
* If significant correlations exist, which candidates are more likely to succeed in which demographics?
* Does economic performance or changes in economic performance affect political leanings?

## Required
Codes are tested on R version 3.5.3 (2019-03-11) -- "Great Truth"

Data files:
* primary_results.csv
* county_facts.csv
* county_facts_dictionary.csv

Source:
* [2016 Primary Election Results](https://www.kaggle.com/benhamner/2016-us-election)

## Analysis
**1.** Load the 3 source files. We will use '**primary_results.csv**' and '**county_facts.csv**' extensively. '**county_facts_dictionary.csv**' will be a reference to demographic codes.

```r
srcPrimary <- read.csv('primary_results.csv', stringsAsFactors = FALSE)
srcDemogr <- read.csv('county_facts.csv', stringsAsFactors = FALSE)
srcDict <- read.csv('county_facts_dictionary.csv', stringsAsFactors = FALSE)
```

---
**2.** Extract primary winners and vote statistics (votes and fraction of votes) in each county for both parties.

```r
for (i in levels(as.factor(srcPrimary$party))) {
  assign(paste('votes', substring(i, 1, 3), sep = ''),
         group_by(srcPrimary, state_abbreviation, county, party) %>%
           filter(party == i & votes != 0 & fraction_votes != 0) %>%
           summarize(winner = candidate[which.max(fraction_votes)],
                     votes = max(votes),
                     fraction_votes = max(fraction_votes)) %>%
           rename(state = state_abbreviation))
  rm(i)
}
```

We now have two data frames `votesRep` and `votesDem`, each consisting of the winners and their vote statistics for all counties.

---
**3.** Extract some demographic data to work with. For now, we'll use:

| Code | Description |
| :--- | :--- |
| INC110213 | Median household income, 2009-2013 |
| EDU685213 | Bachelor's degree or higher, percent of persons age 25+, 2009-2013 |
| POP060210 | Population per square mile, 2010 |
| RHI825214 | White alone, not Hispanic or Latino, percent, 2014 |
| RHI725214 | Hispanic or Latino, percent, 2014 |
| HSD310213 | Persons per household, 2009-2013 |

Let's focus on a few Midwestern states instead of the whole nation. Once our visual, descriptive, and predictive models are sufficiently robust, we can apply them to nationwide data. We'll create a function called `demogrSomeF()` that will allow us to specify which states to extract from the source demographic data frame. This will reduce clutter and potential for mistakes.

```r
demogrSomeF <- function(...) {
  states <- gsub('\"', '', toupper(sapply(substitute(list(...)), deparse)[-1]))
  
  demogrSome <- filter(srcDemogr, state_abbreviation %in% states) %>%
    select(state = state_abbreviation, county = area_name,
           income = INC110213, education = EDU685213, density = POP060210,
           white = RHI825214, hispanic = RHI725214, household = HSD310213) %>%
    mutate(county = gsub(' County', '', county))
  
  assign('demogrSome', demogrSome, envir = globalenv())
}
```

Let's pick 5 Midwestern states for now:

```r
demogrSomeF(IA, IL, MN, NE, MI)
```

For some simple visuals of each party's primary winners in each county, join the winners with demographic data of our 5 chosen states.

```r
combdRep <- inner_join(demogrSome, votesRep, by = c('state', 'county'))
combdDem <- inner_join(demogrSome, votesDem, by = c('state', 'county'))
```

The combined data frames `combdRep` and `combdDem` allow for many possible visual analyses. For example, we can visualize the income demographics that each Republican winners attract:

```r
ggplot(combdRep, aes(x = winner, y = income, fill = winner)) +
  geom_boxplot() +
  coord_flip()
```

---
**4.** Let's go deeper into our data for more insightful visuals. Instead of focusing on winners of each county, we'll select some big candidates and analyze their performance in *all* counties. Keep in mind that our analyses are still based on the 5 states we've chosen above.

```r
candidates <- c('Donald Trump', 'Ted Cruz', 'Hillary Clinton', 'Bernie Sanders')

cddList <- list()

for (i in candidates) {
  cddList[[match(i, candidates)]] <- filter(srcPrimary, candidate == i) %>%
    select(state = state_abbreviation, county = county, party = party,
           candidate = candidate, votes = votes, fraction_votes = fraction_votes) %>%
    inner_join(demogrSome, by = c('state', 'county'))
  
  names(cddList)[match(i, candidates)] <- strsplit(i, ' ') %>%
    sapply('[[', length(unlist(strsplit(i, ' '))))
}
```

---
**5.** We now have a populated list of candidates and their respective vote statistics (merged with demographic metrics in 5 states) in `cddList`. The next step is to plot the fraction of votes (a performance metric) against various demographic metrics. There are plenty of repeatable codes here so we'll build functions called `cddPlot()` and `cddPlotLog()` to reduce clutter. It's not exactly clear when to use a log scale, but it's probably a good idea when small values are compressed down to the bottom of the graph. For normal plots, use `cddPlot()` and for log transformations on the independent variable, use `cddPlotLog()`.

```r
cddPlot <- function(metric) {
	plot_grid(plotlist = lapply(cddList, function(df)
		ggplot(df, aes(x = eval(parse(text = metric)), y = fraction_votes)) +
      geom_point() +
      geom_smooth(method = 'lm', formula = y~x) +
    	ggtitle(label = df[1, 4]) +
    	theme(axis.title.x = element_blank(),
    				axis.title.y = element_blank(),
    				plot.title = element_text(size = 12))),
    align = 'h', label_x = 0, label_y = 0, hjust = -0.5, vjust = -1.5)
}

cddPlotLog <- function(metric) {
	plot_grid(plotlist = lapply(cddList, function(df)
		ggplot(df, aes(x = eval(parse(text = metric)), y = fraction_votes)) +
			geom_point() +
			scale_x_log10() +
			geom_smooth(method = 'lm', formula = y~x) +
			ggtitle(label = df[1, 4]) +
			theme(axis.title.x = element_blank(),
						axis.title.y = element_blank(),
						plot.title = element_text(size = 12))),
		align = 'h', label_x = 0, label_y = 0, hjust = -0.5, vjust = -1.5)
}
```

To be continued ...

## Authors
* **Peter Easler**
* **Joe Gajda**
* **Ashley Leibfried**
* **Yu Shen Lim**

## Built With
* [R](https://www.r-project.org/) (3.5.3)
* [RStudio](https://www.rstudio.com/)

## Acknowledgments
Dataset from [Kaggle](https://www.kaggle.com/benhamner/2016-us-election)
