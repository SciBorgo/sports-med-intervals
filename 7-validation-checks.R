

# Validation checks
# DN Borg & IB Stewart
# May, 2022

d <- arrow::read_parquet("data-articles-searched.parquet")

# repeatable analysis
set.seed(2022-05-26)
articles <- d %>%
  filter(any.ci == 'FALSE') %>%
  sample_n(size = 100)
write.csv(articles, file = "random-sample-100-no-intervals.csv")

interval_articles <- d %>%
  filter(any.ci == 'TRUE') %>%
  sample_n(size = 100)
write.csv(interval_articles, file = "random-sample-100-with-intervals.csv")