

# Sports Medicine Intervals
# David N Borg
# May, 2022
# search details here https://www.nlm.nih.gov/bsd/mms/medlineelements.html

# Packages
library(dplyr)
library(ggplot2)
library(rentrez)
library(stringr)
library(XML)

# Search parameters
# Get Pubmed IDs from selected journals from 2002 to 2022 (unless otherwise commented)
# Restrict to journal articles and reviews
# This does include articles with no abstract
types = c('Journal Article','Clinical Trial','Meta-Analysis','Review','Randomized Controlled Trial','Multicenter Study') # article types to include
types.search = paste(paste(types, '[PT]', sep=''), collapse=' OR ', sep='')

# Data extraction batches
# Note: Sports Medicine and Arthroscopy Review and Clinics in Sports Medicine did not have any articles with ratio intervals over the studied period and were removed from the below journal lists.
journals = c('Br J Sports Med','Am J Sports Med','Med Sci Sports Exerc','J Sci Med Sport')
journals = c('Scand J Med Sci Sports','Arch Phys Med Rehabil','J Rehabil Med','J Sports Sci Med','Clin J Sport Med','Am J Phys Med Rehabil')
journals = c('Res Sports Med') # restrict to years 2005 to 2022
journals = c('Eur J Phys Rehabil Med') # restrict to years 2008 to 2022
journals = c('Phys Sportsmed','Clin Sports Med','Phys Med Rehabil Clin N Am','J Sports Med Phys Fitness')

# Pull papers
numbers = ids = NULL
for (j in journals){
  for (year in 2002:2022){
    query = paste('"', j, '"[SO] AND ', year, '[PDAT] AND (', types.search , ')', sep='')
    journal.search = entrez_search(db='pubmed', term=query, retmax=50000)
    # frame of numbers, store for flow diagram
    nframe = data.frame(journal=j, year=year, count=journal.search$count) 
    numbers = rbind(numbers, nframe)
    # frame of IDS
    frame = data.frame(journal=j, year=year, pubmed=journal.search$ids)
    ids = rbind(ids, frame)
  }
}

# Plot papers each year
ggplot(data=numbers, aes(x=year, y=count, col=factor(journal)))+
  geom_point()+
  geom_line()+
  scale_color_manual('Journal', values=cbPalette)+
  ylab('Number of abstracts')+
  xlab('Year')+
  guides(color=guide_legend(ncol=2))+
  theme_bw()+
  theme(legend.position=c(0.3,0.84), panel.grid.minor = element_blank())

# Rename
meta = ids

# Remove duplicates
meta = dplyr::filter(meta, duplicated(pubmed)==F)

# Check paper numbers
with(meta, table(journal, year))

# Move onto '1-find-intervals', don't clear environment
# Repeat these steps for each batch

