

# Sports Medicine Intervals
# David N Borg
# May, 2022
# Join extracted batches of data

# Packages
library(tidyverse)
library(janitor)

# Load extracted data sets
load("/Users/david/Dropbox/Research projects/Project - Bias in sport and exercise research/intervals-sport-science/intervals-first-batch-sample1.RData")
sample_one = any.data
sample_one_data = data

load("/Users/david/Dropbox/Research projects/Project - Bias in sport and exercise research/intervals-sport-science/intervals-first-batch-sample2.RData")
sample_two_p1 = any.data
sample_two_data_1 = data

load("/Users/david/Dropbox/Research projects/Project - Bias in sport and exercise research/intervals-sport-science/intervals-second-batch.RData")
sample_two_p2 = any.data
sample_two_data_2 = data

## Add Res Sports Med
load("/Users/david/Dropbox/Research projects/Project - Bias in sport and exercise research/intervals-sport-science/intervals-third-batch-res-sports-med.RData")
sample_three = any.data
sample_three_data = data

## Add Eur J batch 1
load("/Users/david/Dropbox/Research projects/Project - Bias in sport and exercise research/intervals-sport-science/intervals-forth-batch-eur-j-phys-rehabil-med.RData")
sample_four = any.data
sample_four_data = data

## Add Eur J batch 2
load("/Users/david/Dropbox/Research projects/Project - Bias in sport and exercise research/intervals-sport-science/intervals-forth-batch-eur-j-phys-rehabil-med-p2.RData")
sample_four_p2 = any.data
sample_four_data_2 = data

## Add batch 5
load("/Users/david/Dropbox/Research projects/Project - Bias in sport and exercise research/intervals-sport-science/intervals-fifth-e1.RData")
a1 = any.data
d1 = data
load("/Users/david/Dropbox/Research projects/Project - Bias in sport and exercise research/intervals-sport-science/intervals-fifth-e2.RData")
a2 = any.data
d2 = data
load("/Users/david/Dropbox/Research projects/Project - Bias in sport and exercise research/intervals-sport-science/intervals-fifth-e3.RData")
a3 = any.data
d3 = data
load("/Users/david/Dropbox/Research projects/Project - Bias in sport and exercise research/intervals-sport-science/intervals-fifth-e4.RData")
a4 = any.data
d4 = data

## Add batch 6
load("/Users/david/Dropbox/Research projects/Project - Bias in sport and exercise research/intervals-sport-science/intervals-sixth.RData")
b6a = any.data
#b6d = data # empty, no data extracted

## Add batch 7
load("/Users/david/Dropbox/Research projects/Project - Bias in sport and exercise research/intervals-sport-science/intervals-seven-e1.RData")
b7a = any.data
#b7d = data # empty, no data extracted
load("/Users/david/Dropbox/Research projects/Project - Bias in sport and exercise research/intervals-sport-science/intervals-seven-e2.RData")
b7a2 = any.data
b7d2 = data

# Merge articles searched (after duplicates removed) and save
m1 <- union(sample_one, sample_two_p1)
m2 <- union(m1, sample_two_p2)
m3 <- union(m2, sample_three)
m4 <- union(m3, sample_four)
m5 <- union(m4, sample_four_p2)
m6 <- union(m5, a1)
m7 <- union(m6, a2)
m8 <- union(m7, a3)
m9 <- union(m8, a4)
m10 <- union(m9, b6a)
m11 <- union(m10, b7a)
union(m11, b7a2) %>%
  write.csv(file = 'data-articles-searched.csv', row.names = F)

# Merge extracted intervals, clean and make new variables
mi1 <- union(sample_one_data,sample_two_data_1)
mi2 <- union(mi1, sample_two_data_2)
mi3 <- union(mi2, sample_three_data)
mi4 <- union(mi3, sample_four_data)
mi5 <- union(mi4, sample_four_data_2)
mi6 <- union(mi5, d1)
mi7 <- union(mi6, d2)
mi8 <- union(mi7, d3)
mi9 <- union(mi8, d4)
#mi10 <- union(mi9, b6d) # no data extracted in b6d
#mi11 <- union(mi10, b7d) # no data extracted in b7d
data_intervals <- union(mi9, b7d2)

head(data_intervals, 10)

data <- data_intervals %>%
  filter(is.odds == 'TRUE' | is.risk == 'TRUE' | is.hazard == 'TRUE') %>%
  mutate(source = 'abstract',
         ci_level = ifelse(ci.level <= 0 | ci.level >= 1, NA, ci.level),
         mistake = mean < lower | mean > upper,
         pubmed = as.numeric(as.character(pubmed))) %>%
  clean_names()

data %>% write.csv(file = 'data-intervals.csv', row.names = F)


#### End


