

# Sports Medicine Intervals
# Table 2
# David N Borg
# May, 2022

# Packages
library(tidyverse)
library(janitor)
library(pander)
library(tables)

# Load data sets
d_papers <- read_csv('data-articles-searched.csv') %>%
  clean_names() %>%
  mutate(year = format(date, format = "%Y"))

data <- read_csv('data-intervals.csv') %>%
  clean_names() %>%
  left_join({d_papers %>% select(pubmed, year)}, by = 'pubmed')

d_unbiased <- read_csv('rsta20170356_si_002.csv') %>%
  clean_names()

load("/Users/david/Downloads/Georgescu.Wren (1).RData")
barnett_abstract = complete %>% filter(source == 'Abstract')

# Remove mistakes, boundary, confidence level missing
dsum <- data %>% filter(mistake == 'FALSE')
dsum1 <- dsum %>% filter(lower>0)
dsum2 <- dsum1 %>% filter(ci_level %in% c('90','95','99'))

# Missing CI level
to.table = filter(data, mistake==FALSE, lower>0) %>%
  mutate(is_95_missing = is.na(ci_level))

# Select only 95% CIs
dsub <- to.table %>% filter(ci_level == '95')
length(unique(dsub$pubmed))


#### Lower bound analysis
# Lower intervals between 1 and 1.1
for.table = group_by(dsub, source) %>%
  mutate(narrow = lower>1 & lower<=1.1,
         narrowf = factor(as.numeric(narrow), levels=0:1, labels=c('No','Yes')))
tab = tabular(Heading('(1, 1.1]')*narrowf+1 ~ Heading('')*factor(source)*((n=1) + Percent('col')), data=for.table)
pander(tab, digits=4)

# Lower intervals between 1 and 1.2
for.table = group_by(dsub, source) %>%
  mutate(narrow = lower>1 & lower<=1.2,
         narrowf = factor(as.numeric(narrow), levels=0:1, labels=c('No','Yes')))
tab = tabular(Heading('(1, 1.2]')*narrowf+1 ~ Heading('')*factor(source)*((n=1) + Percent('col')), data=for.table)
pander(tab, digits=4)

## As above but for Barnett and Wren
# Lower intervals between 1 and 1.1
barnett_abstract = complete %>% filter(source == 'Abstract') %>% clean_names()
for.table = group_by(barnett_abstract, source) %>%
  mutate(narrow = lower>1 & lower<=1.1,
         narrowf = factor(as.numeric(narrow), levels=0:1, labels=c('No','Yes')))
tab = tabular(Heading('(1, 1.1]')*narrowf+1 ~ Heading('')*factor(source)*((n=1) + Percent('col')), data=for.table)
pander(tab, digits=4)

# Lower intervals between 1 and 1.2
for.table = group_by(barnett_abstract, source) %>%
  mutate(narrow = lower>1 & lower<=1.2,
         narrowf = factor(as.numeric(narrow), levels=0:1, labels=c('No','Yes')))
tab = tabular(Heading('(1, 1.2]')*narrowf+1 ~ Heading('')*factor(source)*((n=1) + Percent('col')), data=for.table)
pander(tab, digits=4)

## As above but for unbiased dataset
d_unbiased <- d_unbiased %>% mutate(source = 'source')
for.table = group_by(d_unbiased, source) %>%
  mutate(narrow = lower_bound_95_percent_ci>1 & lower_bound_95_percent_ci<=1.1,
         narrowf = factor(as.numeric(narrow), levels=0:1, labels=c('No','Yes')))
tab = tabular(Heading('(1, 1.1]')*narrowf+1 ~ Heading('')*factor(source)*((n=1) + Percent('col')), data=for.table)
pander(tab, digits=4)

# Lower intervals between 1 and 1.2
for.table = group_by(d_unbiased, source) %>%
  mutate(narrow = lower_bound_95_percent_ci>1 & lower_bound_95_percent_ci<=1.2,
         narrowf = factor(as.numeric(narrow), levels=0:1, labels=c('No','Yes')))
tab = tabular(Heading('(1, 1.2]')*narrowf+1 ~ Heading('')*factor(source)*((n=1) + Percent('col')), data=for.table)
pander(tab, digits=4)




#### Upper bound analysis
# Upper intervals between 0.9 and 1
for.table = group_by(dsub, source) %>%
  mutate(narrow = upper>0.9 & upper<1,
         narrowf = factor(as.numeric(narrow), levels=0:1, labels=c('No','Yes')))
tab = tabular(Heading('[0.9, 1]')*narrowf+1 ~ Heading('')*factor(source)*((n=1) + Percent('col')), data=for.table)
pander(tab, digits=4)

# Upper intervals between 0.8 and 1
for.table = group_by(dsub, source) %>%
  mutate(narrow = upper>0.8 & upper<1,
         narrowf = factor(as.numeric(narrow), levels=0:1, labels=c('No','Yes')))
tab = tabular(Heading('[0.8, 1)')*narrowf+1 ~ Heading('')*factor(source)*((n=1) + Percent('col')), data=for.table)
pander(tab, digits=4)

## As above for Barnett and Wren
# Upper intervals between 0.9 and 1
for.table = group_by(barnett_abstract, source) %>%
  mutate(narrow = upper>0.9 & upper<1,
         narrowf = factor(as.numeric(narrow), levels=0:1, labels=c('No','Yes')))
tab = tabular(Heading('[0.9, 1]')*narrowf+1 ~ Heading('')*factor(source)*((n=1) + Percent('col')), data=for.table)
pander(tab, digits=4)

# Upper intervals between 0.8 and 1
for.table = group_by(barnett_abstract, source) %>%
  mutate(narrow = upper>0.8 & upper<1,
         narrowf = factor(as.numeric(narrow), levels=0:1, labels=c('No','Yes')))
tab = tabular(Heading('[0.8, 1)')*narrowf+1 ~ Heading('')*factor(source)*((n=1) + Percent('col')), data=for.table)
pander(tab, digits=4)

## As above for unbiased dataset
# Upper intervals between 0.9 and 1
d_unbiased <- d_unbiased %>% mutate(source = 'source')
for.table = group_by(d_unbiased, source) %>%
  mutate(narrow = upper_bound_95_percent_ci>0.9 & upper_bound_95_percent_ci<1,
         narrowf = factor(as.numeric(narrow), levels=0:1, labels=c('No','Yes')))
tab = tabular(Heading('[0.9, 1]')*narrowf+1 ~ Heading('')*factor(source)*((n=1) + Percent('col')), data=for.table)
pander(tab, digits=4)

# Upper intervals between 0.8 and 1
for.table = group_by(d_unbiased, source) %>%
  mutate(narrow = upper_bound_95_percent_ci>0.8 & upper_bound_95_percent_ci<1,
         narrowf = factor(as.numeric(narrow), levels=0:1, labels=c('No','Yes')))
tab = tabular(Heading('[0.8, 1)')*narrowf+1 ~ Heading('')*factor(source)*((n=1) + Percent('col')), data=for.table)
pander(tab, digits=4)
