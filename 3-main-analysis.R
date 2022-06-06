

# Sports Medicine Intervals
# David N Borg
# May, 2022

# Packages
library(tidyverse)
library(janitor)
library(ggplot2)
library(cowplot)
library(naniar)
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


## Summary of articles searched
length(unique(d_papers$pubmed)) # checks

# Articles by journal
with(d_papers, table(journal))

# Articles by journal and year
with(d_papers, table(journal, year))


## Summary of ratio interval papers
data %>% group_by(journal) %>%
  count() %>%
  mutate(pc = (n/nrow(data)*100),
         pc = round(pc,1))

data %>% group_by(pubmed, journal) %>%
  count() -> unique_papers
unique_papers %>% group_by(journal) %>%
  count() -> out
out
out %>% write.csv(file = 'interval_papers.csv', row.names = F)

length(unique(data$pubmed))


# Summary
to.table = group_by(data, journal) %>%
  summarise(n = n(),
            journals = length(unique(journal)),
            zero = sum(lower==0))
to.table

# Remove mistakes, boundary, confidence level missing
dsum <- data %>% filter(mistake == 'FALSE')
dsum1 <- dsum %>% filter(lower>0)
dsum2 <- dsum1 %>% filter(ci_level %in% c('90','95','99'))

# Mistakes
tabular(Heading('Mistake')*factor(mistake) + 1~ Heading('')*factor(source)*((n=1) + Percent('col')), data=data)

# CI lower bound below zero
data %>% filter(lower<0) %>%
  count() %>%
  mutate(pc = (n/nrow(data))*100)

# Missing CI level
to.table = filter(data, mistake==FALSE, lower>0) %>%
  mutate(is_95_missing = is.na(ci_level))
tabular(Heading('Missing')*factor(is_95_missing) + 1~ Heading('')*(factor(source) +1)*((n=1) + Percent('col')), data=to.table)

# CI levels reported
table(to.table$ci_level) # excluding mistakes and intervals where the lower bound was <0
table(data$ci_level) # all papers

# Select only 95% CIs
dsub <- to.table %>% filter(ci_level == '95')
length(unique(dsub$pubmed))

# Lower confidence interval (log-scale)
# split by CI type - make nicer label (also add numbers?)
rr.lower = filter(dsub, lower>0)
lplot = ggplot(rr.lower, aes(x=lower))+
  stat_ecdf(data = d_unbiased, aes(x = lower_bound_95_percent_ci), colour = 'gray40', geom = "step", size=0.8)+
  geom_vline(xintercept = 1, lty=1, col='black')+
  stat_ecdf(geom = "step", colour = 'red', size=1)+
  #scale_x_log10(breaks=c(0.1,0.5,1,2,10), labels=c(0.1,0.5,1,2,10))+
  scale_x_continuous(breaks=c(0,0.5,1,1.5,2), labels=c(0,0.5,1,1.5,2))+
  xlab('Ratio')+
  ylab('Cumulative distribution')+
  coord_cartesian(xlim=c(0, 2))+ # limit x-axis to focus on key change
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  facet_wrap(~'95% CI lower bounds') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
lplot
#ggsave(file = "figure_lower.png", units="in", dpi = 600, width = 4, height = 3.5)

# number and percent excluded from plot by the limits on the x-axis
round(100*sum(rr.lower$lower<0.1)/nrow(rr.lower),1)
round(100*sum(rr.lower$lower>10)/nrow(rr.lower),1)


# Upper confidence interval (log-scale)
uplot = ggplot(dsub, aes(x=upper))+
  stat_ecdf(data = d_unbiased, aes(x = upper_bound_95_percent_ci), colour = 'gray50', geom = "step", size=0.8)+
  geom_vline(xintercept = 1, lty=1, col='black')+
  stat_ecdf(geom = "step", colour = 'red', size=1)+
  #scale_x_log10(breaks=c(0.5,1,1.5,2,2.5,3), labels=c(0.1,0.5,1,2,10))+
  scale_x_continuous(breaks=c(0.5,1,1.5,2,2.5,3), labels=c(0.5,1,1.5,2,2.5,3))+
  coord_cartesian(xlim=c(0.5, 3), ylim=c(0, 1))+ # limit x-axis to focus on key change
  xlab('Ratio')+
  ylab('Cumulative distribution')+
  theme_bw()+  
  theme(panel.grid.minor = element_blank())+
  facet_wrap(~'95% CI upper bounds')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
uplot
#ggsave(file = "figure_upper.png", units="in", dpi = 600, width = 4, height = 3.5)


## Panel plots
plot_grid(lplot, uplot,
          ncol = 2,
          nrow = 1,
          #labels = c('A','B'),
          #label_size = 16
          align = 'v',
          axis = "lr")
ggsave(file = "figure1.png", units="in", width = 7, height = 3.5, dpi = 600)



## Same plots as above, plotted against Barnett and Wren data
barnett_abstract = complete %>% filter(source == 'Abstract')
lbarn = ggplot(dsub, aes(x=lower))+
  stat_ecdf(data = barnett_abstract, aes(x = lower), colour = 'black', geom = "step", size=0.8)+
  geom_vline(xintercept = 1, lty=1, col='black')+
  stat_ecdf(geom = "step", colour = 'red', size=1)+
  #scale_x_log10(breaks=c(0.1,0.5,1,2,10), labels=c(0.1,0.5,1,2,10))+
  scale_x_continuous(breaks=c(0,0.5,1,1.5,2), labels=c(0,0.5,1,1.5,2))+
  xlab('Ratio')+
  ylab('Cumulative distribution')+
  coord_cartesian(xlim=c(0, 2))+ # limit x-axis to focus on key change
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  facet_wrap(~'95% CI lower bounds') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
lbarn

ubarn = ggplot(dsub, aes(x=upper))+
  stat_ecdf(data = barnett_abstract, aes(x = upper), colour = 'black', geom = "step", size=0.8)+
  geom_vline(xintercept = 1, lty=1, col='black')+
  stat_ecdf(geom = "step", colour = 'red', size=1)+
  #scale_x_log10(breaks=c(0.5,1,1.5,2,2.5,3), labels=c(0.1,0.5,1,2,10))+
  scale_x_continuous(breaks=c(0.5,1,1.5,2,2.5,3), labels=c(0.5,1,1.5,2,2.5,3))+
  coord_cartesian(xlim=c(0.5, 3), ylim=c(0, 1))+ # limit x-axis to focus on key change
  xlab('Ratio')+
  ylab('Cumulative distribution')+
  theme_bw()+  
  theme(panel.grid.minor = element_blank())+
  facet_wrap(~'95% CI upper bounds')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ubarn

## Panel plots
plot_grid(lbarn, ubarn,
          ncol = 2,
          nrow = 1,
          #labels = c('A','B'),
          #label_size = 16
          align = 'v',
          axis = "lr")
ggsave(file = "figure2.png", units="in", width = 7, height = 3.5, dpi = 600)


# Z-value curve
z_95 = 1.96
dsub %>%
  mutate(se = ({log(upper)}-{log(lower)})/(2*z_95),
         z = log(mean)/se) %>%
  ggplot() + geom_histogram(aes(x = z), binwidth = 0.02, size = 0.1, alpha = 0.7) +
  theme_bw() +
  xlim(-10,10) +
  facet_grid(~'Distribution of z-values')+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  scale_y_continuous(n.breaks = 6)+
  labs(x = 'z-value', y = 'Count') +
  geom_vline(xintercept = c(-1.96,1.96), lty=1, col='black') +
  xlim(-7,7)
ggsave(file = "figure4-0.02.png", units="in", dpi = 600, width = 4, height = 3)

dsub %>%
  mutate(se = ({log(upper)}-{log(lower)})/(2*z_95),
         z = log(mean)/se,
         Significant = as.factor(between(z, -1.96, 1.96)),
         Significant = recode_factor(Significant, 'FALSE' = 'True', 'TRUE' = 'False')) %>%
  ggplot() + geom_histogram(aes(x = z, fill = Significant), binwidth = 0.02, size = 0.1) +
  theme_bw() +
  xlim(-10,10) +
  facet_grid(~'Distribution of z-values')+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  scale_y_continuous(n.breaks = 6)+
  labs(x = 'z-value', y = 'Count') +
  #geom_vline(xintercept = c(-1.96,1.96), lty=1, col='black') +
  scale_x_continuous(limits = c(-7,7), n.breaks = 8) +
  scale_fill_brewer(palette = 'Set2', direction = 1)
ggsave(file = "figure4-0.02.png", units="in", dpi = 600, width = 4.5, height = 2.75)

dsub %>%
  mutate(se = ({log(upper)}-{log(lower)})/(2*z_95),
         z = log(mean)/se,
         Significant = as.factor(between(z, -1.96, 1.96)),
         Significant = recode_factor(Significant, 'FALSE' = 'True', 'TRUE' = 'False')) %>%
  ggplot() + geom_histogram(aes(x = z, fill = Significant), binwidth = 0.04, size = 0.1) +
  theme_bw() +
  xlim(-10,10) +
  facet_grid(~'Distribution of z-values')+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  scale_y_continuous(n.breaks = 6)+
  labs(x = 'z-value', y = 'Count') +
  #geom_vline(xintercept = c(-1.96,1.96), lty=1, col='black') +
  scale_x_continuous(limits = c(-7,7), n.breaks = 8) +
  scale_fill_brewer(palette = 'Paired', direction = 1)
ggsave(file = "figure4-0.04-blue.png", units="in", dpi = 600, width = 4.5, height = 2.75)

dsub %>%
  mutate(se = ({log(upper)}-{log(lower)})/(2*z_95),
         z = log(mean)/se,
         Significant = as.factor(between(z, -1.96, 1.96)),
         Significant = recode_factor(Significant, 'FALSE' = 'True', 'TRUE' = 'False')) %>%
  ggplot() + geom_histogram(aes(x = z, fill = Significant), binwidth = 0.02, size = 0.1) +
  theme_bw() +
  xlim(-10,10) +
  facet_grid(~'Distribution of z-values')+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  scale_y_continuous(n.breaks = 6)+
  labs(x = 'z-value', y = 'Count') +
  #geom_vline(xintercept = c(-1.96,1.96), lty=1, col='black') +
  scale_x_continuous(limits = c(-7,7), n.breaks = 8) +
  scale_fill_brewer(palette = 'Paired', direction = 1)
ggsave(file = "figure4-0.02-blue.png", units="in", dpi = 600, width = 4.5, height = 2.75)

#### End





