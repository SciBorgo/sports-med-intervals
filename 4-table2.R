

# Sports Medicine Intervals
# Table 2
# David N Borg
# May, 2022


# Load data sets
d_papers <- arrow::read_parquet("data-articles-searched.parquet") %>%
  clean_names() %>%
  mutate(year = format(date, format = "%Y"))

data <- read_csv('data-intervals.csv') %>%
  clean_names() %>%
  left_join({d_papers %>% select(pubmed, year)}, by = 'pubmed')

d_unbiased <- arrow::read_parquet("unbiased.parquet") %>%
  clean_names()

complete = arrow::read_parquet("complete.parquet")
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
# length(unique(dsub$pubmed))


dsub_abs = dsub %>%
  filter(source == "abstract") %>%
  mutate(set = "Current Study") %>%
  select(set, lower, upper)

barnett_abs = complete %>% 
  filter(source == 'Abstract') %>%
  mutate(set = "Barnett & Wren") %>%
  select(set, lower, upper)

d_un_abs = d_unbiased %>%
  rename(lower=lower_bound_95_percent_ci,
         upper=upper_bound_95_percent_ci) %>%
  mutate(set = "Unbiased") %>%
    select(set, lower, upper)

# Build summary set -------
df_tabcomb = bind_rows(dsub_abs, barnett_abs, d_un_abs) %>%
  mutate(set = factor(set,
                      levels = c("Current Study", "Barnett & Wren", "Unbiased"))) %>%
  mutate(narrow = lower>1 & lower<=1.1,
         ll_11 = factor(as.numeric(narrow), levels=0:1, labels=c('No','Yes'))) %>%
  mutate(narrow2 = lower>1 & lower<=1.2,
         ll_12 = factor(as.numeric(narrow2), levels=0:1, labels=c('No','Yes'))) %>%
  mutate(narrow3 = upper>0.9 & upper<1,
         ul_11 = factor(as.numeric(narrow3), levels=0:1, labels=c('No','Yes')))%>%
  mutate(narrow4 = upper>0.8 & upper<1,
         ul_12 = factor(as.numeric(narrow4), levels=0:1, labels=c('No','Yes')))

gt_tab2 = df_tabcomb %>%
  select(-lower, -narrow, -narrow2, -narrow3, -narrow4, -upper) %>%
  tbl_summary(by = set,    statistic = list(
    all_continuous() ~ "{median} ({IQR})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  label = list(ll_11 ~ "Lower interval between 1 and 1.1",
               ll_12 ~ "Lower intervals between 1 and 1.2",
               ul_11 ~ "Upper interval between 0.9 and 1",
               ul_12 ~ "Upper intervals between 0.8 and 1"),
  missing = "no"
  ) %>%
  modify_caption("**Table 2. CI characteristics**")
