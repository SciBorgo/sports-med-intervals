

# Sports Medicine Intervals
# David N Borg
# May, 2022


# Load data sets
d_papers <- arrow::read_parquet("data-articles-searched.parquet") %>%
  clean_names() %>%
  mutate(year = format(date, format = "%Y"))

data <- read_csv('data-intervals.csv') %>%
  clean_names() %>%
  left_join({d_papers %>% select(pubmed, year)}, by = 'pubmed')

# Missing CI level: Investigate missings
gg_miss = data %>% select(pubmed,is_odds,is_risk,is_hazard,journal,year,ci_level,mistake) %>%
  vis_miss()

gg_miss_year = data %>% select(pubmed,is_odds,is_risk,is_hazard,journal,year,ci_level,mistake) %>%
  gg_miss_case(facet = year)

gg_miss_journal = data %>% select(pubmed,is_odds,is_risk,is_hazard,journal,year,ci_level,mistake) %>%
  gg_miss_case(facet = journal)

gg_miss_ci_year = data %>% select(pubmed,is_odds,is_risk,is_hazard,journal,year,ci_level,mistake) %>%
  select(ci_level,year) %>%
  mutate('CI level' = ci_level,
         ci_level = NULL) %>%
  gg_miss_fct(fct = year) +
  labs(y = 'Variable', x = 'Year') 
#p1 # decrease in missing data over time

gg_miss_ci_journal1 = data %>% select(pubmed,is_odds,is_risk,is_hazard,journal,year,ci_level,mistake) %>%
  select(ci_level,journal) %>%
  gg_miss_fct(fct = journal)

gg_miss_ci_journal = data %>%
  select(journal,ci_level) %>%
  group_by(journal) %>%
  miss_var_summary() %>%
  arrange(-pct_miss) %>%
  mutate(journal = recode_factor(journal,
                                 'Clinical journal of sport medicine : official journal of the Canadian Academy of Sport Medicine' = 'Clinical J of Sport Med',
                                 'American journal of physical medicine & rehabilitation' = 'American J of Phys Med & Rehab',
                                 'British journal of sports medicine' = 'British J of Sports Med',
                                 'European journal of physical and rehabilitation medicine' = 'European J of Physical & Rehab Med',
                                 'International journal of sports medicine' = 'International J of Sports Med',
                                 'Journal of rehabilitation medicine' = 'J of Rehab Med',
                                 'Journal of science and medicine in sport' = 'J of Science & Med in Sport',
                                 'Medicine and science in sports and exercise' = 'Med & Science in Sports & Exercise',
                                 'Research in sports medicine (Print)' = 'Research in Sports Med',
                                 'Scandinavian journal of medicine & science in sports' = 'Scandinavian J of Med & Science in Sports',
                                 'Sports medicine (Auckland, N.Z.)' = 'Sports Med',
                                 'The American journal of sports medicine' = 'The American J of Sports Med',
                                 'The Journal of sports medicine and physical fitness' = 'J of Sports Med & Phys Fitness',
                                 'The Physician and sportsmedicine' = 'The Physician & Sportsmedicine',
                                 'Archives of physical medicine and rehabilitation' = 'Archives of Phys Med & Rehab'),
         `% Miss` = pct_miss) %>%
  ggplot(aes(y = 'CI level', x = reorder(journal,  `% Miss`), fill =  `% Miss`)) +
  geom_tile() +
  labs(y = 'Variable', x = 'Journal') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_viridis_c()


# 'interaction' between journal and year
journalbyyear = data %>%
  select(journal,ci_level, year) %>%
  group_by(journal,year) %>%
  miss_var_summary() %>%
  arrange(-pct_miss) %>%
  ggplot(aes(y = year, x = reorder(journal, pct_miss), fill = pct_miss)) +
  geom_tile() +
  labs(y = 'Confidence interval level', x = 'Journal') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_viridis_c()

# Supplement 1: Panel missing data plots
pg1 = plot_grid(gg_miss_ci_year, gg_miss_ci_journal,
          ncol = 1,
          nrow = 2,
          labels = c('A','B'),
          label_size = 16,
          align = 'v',
          axis = "lr",
          rel_heights = c(.25,.5),
          scale = .9)
ggsave(pg1, file = "supplement1.png", units="in", width = 7, height = 7, dpi = 600)

# Supplement 3: Cumulative distribution of intervals with missing level of confidence interval
rr.level <- data %>% filter(lower>0, mistake == 'FALSE', !ci_level %in% c('90','95','99'))
lplot = ggplot(rr.level, aes(lower))+
  stat_ecdf(data = rr.level, aes(x = upper, colour = 'Upper'), geom = "step", size=1)+
  geom_vline(xintercept = 1, lty=1, col='black')+
  stat_ecdf(geom = "step", aes(colour = 'Lower'), size=1)+
  #scale_x_log10(breaks=c(0.1,0.5,1,2,10), labels=c(0.1,0.5,1,2,10))+
  scale_x_continuous(breaks=c(0,0.5,1,1.5,2), labels=c(0,0.5,1,1.5,2))+
  xlab('Ratio')+
  ylab('Cumulative distribution')+
  coord_cartesian(xlim=c(0, 2))+ # limit x-axis to focus on key change
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  facet_wrap(~'Intervals with missing level of confidence') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(colour=guide_legend(title="Interval"))

ggsave(lplot, file = "supplement3.png", units="in", dpi = 600, width = 5, height = 3.5)

