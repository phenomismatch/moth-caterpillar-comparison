#reading in Moth abundance and Coweeta data
library(dplyr)
library(stringr)

moth <- read.table('c:/git/moth-caterpillar-comparison/data/moth-abundance.txt', header = T, sep = '\t', fill = TRUE, stringsAsFactors = FALSE)%>%
  filter(site=='Blue Heron')
coweeta<- read.table('c:/git/moth-caterpillar-comparison/data/coweeta_cats.txt',header = T, sep = '\t', fill = TRUE, stringsAsFactors = FALSE)%>%
  filter(Year>2009)
#Moth abundance data seems to be clean and complete, has NA's for the morpho species but don't think I need to worry about that.
#Coweeta data is filtered for 2010 and beyond and each observation has the appropriate amount of leaves (n>40 leaves). 
#Still need to look into filtering Coweeta data for duplicates of 0 in both records (Same year/yearday/sample, etc, but 0 in both records)

