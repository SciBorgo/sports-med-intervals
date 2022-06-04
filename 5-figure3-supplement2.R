

# Sports Medicine Intervals
# Cumulative distributions by year and journal
# David N Borg
# May, 2022

# Packages
library(tidyverse)
library(janitor)
library(ggplot2)
library(cowplot)

# Load data sets
d_papers <- read_csv('data-articles-searched.csv') %>%
  clean_names() %>%
  mutate(year = format(date, format = "%Y"))

data <- read_csv('data-intervals.csv') %>%
  clean_names() %>%
  left_join({d_papers %>% select(pubmed, year)}, by = 'pubmed')

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

# Exploratory analyses: by journal
table(dsub$journal)
dsub %>%
  filter(!journal %in% c('The Physician and sportsmedicine',
                         'The Journal of sports medicine and physical fitness',
                         'Research in sports medicine (Print)',
                         'Journal of sports science & medicine',
                         'International journal of sports medicine',
                         'European journal of physical and rehabilitation medicine'
  )) %>%
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
                                 'Archives of physical medicine and rehabilitation' = 'Archives of Phys Med & Rehab')) %>%
  ggplot()+
  geom_vline(xintercept = 1, lty=1, col='black')+
  stat_ecdf(geom = "step", size=1.1, aes(x = lower, colour = '95% Lower'))+
  stat_ecdf(geom = "step", size=1.1, aes(x = upper, colour = '95% Upper'))+
  scale_x_continuous(breaks=c(0,0.5,1,1.5,2), labels=c(0,0.5,1,1.5,2))+
  xlab('Ratio')+
  ylab('Cumulative distribution')+
  coord_cartesian(xlim=c(0, 2))+ # limit x-axis to focus on key change
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  facet_wrap(~journal) +
  guides(colour=guide_legend(title="Interval"))
ggsave(file = "supplement2.png", units="in", dpi = 600, width = 12, height = 7)

dsub %>%
  filter(!journal %in% c('Research in sports medicine (Print)',
                         'Journal of sports science & medicine',
                         'European journal of physical and rehabilitation medicine')) %>%
  ggplot(aes(x=lower, colour = journal))+
  geom_vline(xintercept = 1, lty=1, col='black')+
  stat_ecdf(geom = "step", size=1.1)+
  scale_x_log10(breaks=c(0.1,0.5,1,2,10), labels=c(0.1,0.5,1,2,10))+
  xlab('Ratio')+
  ylab('Cumulative distribution')+
  coord_cartesian(xlim=c(0.1, 10))+ # limit x-axis to focus on key change
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  facet_wrap(~journal)+
  guides(colour = 'none')
ggsave(file = "figure_lower_by_time.png", units="in", dpi = 600, width = 7, height = 5)



## Explore change over time
dsub %>%
  mutate(year = as.integer(year),
         year_cut = cut(year,
                        breaks = c(2000,2007,2012,2017,2023),
                        labels = c('2002-2006','2007-2012','2012-2016','2017-2022'))) %>%
  ggplot()+
  geom_vline(xintercept = 1, lty=1, col='black')+
  stat_ecdf(aes(lower, colour = '95% lower'), geom = "step", size=1)+
  stat_ecdf(aes(upper, colour = '95% upper'), geom = "step", size=1)+
  scale_x_continuous(breaks=c(0,0.5,1,1.5,2,2.5,3), labels=c(0,0.5,1,1.5,2,2.5,3))+
  coord_cartesian(xlim=c(0.1, 3))+
  xlab('Ratio')+
  ylab('Cumulative distribution')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  facet_wrap(~year_cut) +
  guides(colour=guide_legend(title="Interval")) +
  scale_colour_manual(values = c('grey70','black'))
ggsave(file = "figure3.png", units="in", dpi = 600, width = 7, height = 5)



#### End


